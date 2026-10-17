"""Verified frozen inputs for a local worker; no authentication or execution.

These helpers deliberately do not read current room files, extract archives,
load project CLI configuration, or invoke a model. A caller supplies an already
authenticated, lease-scoped MCP call and its locally pinned adapter config.
"""
import base64
import hashlib
import hmac
import json
import os
from pathlib import Path
import re
import shutil
import tempfile
import unicodedata
import uuid


POLICY = {"version": 2, "max-files": 100, "max-file-bytes": 1048576,
          "max-total-bytes": 2097152, "max-input-json-bytes": 4194304, "chunk-bytes": 16384,
          "lease-ttl-ms": 30000}
MODES = {0o100644: 0o600, 0o100755: 0o700}
# Runtime configuration is not task data. A selected config must never become
# an automatic CLI hook/plugin/MCP server, even before the first model call.
CONTROL_PATHS = {".git", ".codex", ".claude", ".agents", ".aws", ".ssh", ".mcp.json", ".claude.json"}


def canonical_uuid(value):
    if not isinstance(value, str) or str(uuid.UUID(value)) != value:
        raise ValueError("Expected a canonical UUID")
    return value


def path_parts(path):
    if (not isinstance(path, str) or not 1 <= len(path) <= 512
            or "\\" in path or any(unicodedata.category(c) == "Cc" for c in path)):
        raise ValueError("Invalid worker filename")
    try:
        path.encode("utf-8")
    except UnicodeError as error:
        raise ValueError("Worker filenames must be UTF-8") from error
    parts = path.split("/")
    if any(not p.strip() or p in {".", ".."} or p.casefold() in CONTROL_PATHS for p in parts):
        raise ValueError("Worker input cannot name traversal or runtime configuration")
    return parts


def _closed(value, keys):
    if type(value) is not dict or set(value) != set(keys):
        raise ValueError("Unexpected worker input fields")


def _text(value, maximum):
    return isinstance(value, str) and 1 <= len(value) <= maximum


def file_bytes(file):
    _closed(file, {"base64", "bytes", "sha256", "mode"})
    if (not isinstance(file["base64"], str) or len(file["base64"]) > 1398104
            or type(file["bytes"]) is not int or not 0 <= file["bytes"] <= POLICY["max-file-bytes"]
            or type(file["mode"]) is not int or file["mode"] not in MODES
            or not isinstance(file["sha256"], str) or not re.fullmatch(r"[0-9a-f]{64}", file["sha256"])):
        raise ValueError("Invalid bounded worker file")
    data = base64.b64decode(file["base64"], validate=True)
    if len(data) != file["bytes"] or not hmac.compare_digest(hashlib.sha256(data).hexdigest(), file["sha256"]):
        raise ValueError("Worker file size or digest does not match")
    return data


def validate(frozen, config):
    _closed(frozen, {"input-id", "task", "config", "files", "limits", "policy"})
    canonical_uuid(frozen["input-id"])
    _closed(frozen["config"], {"adapter", "version", "model", "policy"})
    if (frozen["config"] != config or not isinstance(config["adapter"], str)
            or config["adapter"] not in {"codex", "claude-code"}
            or not _text(config["version"], 200) or not _text(config["model"], 200)
            or config["policy"] != "local-trusted-v1" or not _text(frozen["task"], 16000)):
        raise ValueError("Worker task or pinned adapter configuration differs")
    _closed(frozen["policy"], POLICY)
    if any(type(frozen["policy"][k]) is not int or frozen["policy"][k] != v for k, v in POLICY.items()):
        raise ValueError("Unsupported worker file policy")
    _closed(frozen["limits"], {"timeout-ms", "settlement"})
    limits = frozen["limits"]
    if (type(limits["timeout-ms"]) is not int or not 1000 <= limits["timeout-ms"] <= 600000
            or not isinstance(limits["settlement"], str) or limits["settlement"] not in {"review", "discard"}):
        raise ValueError("Invalid worker limits")
    files = frozen["files"]
    if type(files) is not dict or len(files) > POLICY["max-files"]:
        raise ValueError("Too many worker input files")
    total = 0
    for path, file in files.items():
        parts = path_parts(path)
        if any("/".join(parts[:n]) in files for n in range(1, len(parts))):
            raise ValueError("Worker input has a file/directory collision")
        total += len(file_bytes(file))
        if total > POLICY["max-total-bytes"]:
            raise ValueError("Worker input exceeds total byte limit")
    return frozen


def _unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("Duplicate JSON field")
        result[key] = value
    return result


def _invalid_constant(_):
    raise ValueError("Non-JSON numeric constant")


def fetch(call, run, config):
    """Read one immutable lease input; validate the entire bundle before use.

    The input UUID is an opaque server content identity (not a hash of JSON).
    It must match every authenticated chunk and the decoded payload; SHA256
    independently checks the exact byte encoding, then each file is checked.
    """
    canonical_uuid(run)
    descriptor, chunks, offset = None, [], 0
    while True:
        part = call("simmis_worker-input", {"run": run, "offset": offset, "length": POLICY["chunk-bytes"]})
        if type(part) is not dict or set(part) not in (
                {"input-id", "bytes", "sha256", "offset", "base64"},
                {"input-id", "bytes", "sha256", "offset", "base64", "next-offset"}):
            raise ValueError("Invalid worker input chunk")
        identity = {k: part[k] for k in ("input-id", "bytes", "sha256")}
        canonical_uuid(identity["input-id"])
        if (type(identity["bytes"]) is not int or not 1 <= identity["bytes"] <= POLICY["max-input-json-bytes"]
                or not isinstance(identity["sha256"], str) or not re.fullmatch(r"[0-9a-f]{64}", identity["sha256"])
                or type(part["offset"]) is not int or part["offset"] != offset
                or (descriptor is not None and identity != descriptor)
                or not isinstance(part["base64"], str) or len(part["base64"]) > 21848):
            raise ValueError("Worker input identity, limit or offset differs")
        descriptor = identity
        data = base64.b64decode(part["base64"], validate=True)
        end = offset + len(data)
        if (not 1 <= len(data) <= POLICY["chunk-bytes"] or end > identity["bytes"]
                or (end < identity["bytes"] and (type(part.get("next-offset")) is not int or part["next-offset"] != end))
                or (end == identity["bytes"] and "next-offset" in part)):
            raise ValueError("Invalid worker input chunk length")
        chunks.append(data)
        offset = end
        if offset == identity["bytes"]:
            break
    encoded = b"".join(chunks)
    if not hmac.compare_digest(hashlib.sha256(encoded).hexdigest(), descriptor["sha256"]):
        raise ValueError("Worker input bundle digest differs")
    try:
        frozen = json.loads(encoded.decode("utf-8"), object_pairs_hook=_unique_object,
                            parse_constant=_invalid_constant)
    except (UnicodeError, RecursionError) as error:
        raise ValueError("Worker input is not bounded UTF-8 JSON") from error
    frozen = validate(frozen, config)
    if frozen["input-id"] != descriptor["input-id"]:
        raise ValueError("Worker input payload identity differs")
    return frozen


def materialize(frozen, config, parent):
    """Create a new private workspace, never reuse or overwrite a directory.

    All validation precedes creation. Descriptor-relative O_NOFOLLOW opens
    prevent traversal through symlinks even if a local process races a path.
    No credentials, CLI settings or unselected room files enter this tree.
    """
    validate(frozen, config)
    root = Path(tempfile.mkdtemp(prefix="simmis-worker-", dir=parent))
    try:
        root_fd = os.open(root, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
        try:
            for path, file in frozen["files"].items():
                parts = path_parts(path)
                directory = os.dup(root_fd)
                try:
                    for part in parts[:-1]:
                        try:
                            os.mkdir(part, mode=0o700, dir_fd=directory)
                        except FileExistsError:
                            pass
                        child = os.open(part, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=directory)
                        os.close(directory)
                        directory = child
                    fd = os.open(parts[-1], os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                                 0o600, dir_fd=directory)
                    with os.fdopen(fd, "wb") as stream:
                        stream.write(file_bytes(file))
                        os.fchmod(stream.fileno(), MODES[file["mode"]])
                        stream.flush()
                        os.fsync(stream.fileno())
                finally:
                    os.close(directory)
        finally:
            os.close(root_fd)
        return root
    except BaseException:
        # Only this fresh mkdtemp result is owned here; never a caller's tree.
        shutil.rmtree(root)
        raise
