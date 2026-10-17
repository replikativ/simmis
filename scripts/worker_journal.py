"""Private immutable physical-controller phases; not an execution engine.

The caller durably creates each private Run directory and holds the permanent
worker-wide controller lock throughout admission, execution and reconciliation.
Credentials live only in claim.json; journal records bind its nonsecret IDs.
No phase starts a process, sends GO, calls MCP, imports, certifies or merges.
Only trusted controller code may supply physical observations/server receipts.
Reading a chain includes file/directory fsync barriers before adopting records
whose original publication may have had an ambiguous durability result.
"""
import hashlib
import json
import os
import re
import urllib.parse
import uuid

import mcp_workspace
import worker_claim as claim
import worker_inputs as inputs
import worker_namespace as namespace
import worker_outputs as outputs
import worker_unit as unit
import worker_runtime as runtime


PHASES = ("launch", "admitted", "stopped", "acknowledged")
MAX_RECORD_BYTES = namespace.MAX_REQUEST_BYTES


def encode(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False,
                      allow_nan=False).encode("utf-8")


def digest(value):
    return hashlib.sha256(encode(value)).hexdigest()


def _closed(value, fields):
    if type(value) is not dict or set(value) != set(fields):
        raise ValueError("Unexpected private journal fields")


def _read(directory, phase):
    name = phase + ".json"
    try:
        inspected = os.stat(name, dir_fd=directory, follow_symlinks=False)
    except FileNotFoundError:
        return None
    claim._regular_info(inspected)
    if inspected.st_size > MAX_RECORD_BYTES:
        raise ValueError("Private journal record exceeds its bound")
    fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=directory)
    try:
        current = os.fstat(fd)
        claim._regular_info(current)
        if (current.st_dev, current.st_ino) != (inspected.st_dev, inspected.st_ino):
            raise ValueError("Private journal record changed during open")
        with os.fdopen(fd, "rb", closefd=False) as stream:
            encoded = stream.read(MAX_RECORD_BYTES + 1)
        if len(encoded) > MAX_RECORD_BYTES:
            raise ValueError("Private journal record exceeds its bound")
        try:
            value = json.loads(encoded.decode("utf-8"), object_pairs_hook=inputs._unique_object,
                               parse_constant=inputs._invalid_constant)
            if type(value) is not dict or encode(value) != encoded:
                raise ValueError("Private journal encoding is not canonical")
        except (ValueError, UnicodeError, RecursionError):
            raise ValueError("Malformed private journal record") from None
        os.fsync(fd)
        return value
    finally:
        os.close(fd)


def _common(value, phase, previous, fields, *, version=1):
    _closed(value, {"version", "phase", "previous", *fields})
    if (type(value["version"]) is not int or value["version"] != version or value["phase"] != phase
            or value["previous"] != (digest(previous) if previous is not None else None)):
        raise ValueError("Private journal phase linkage differs")


def _owner(ticket):
    claim._validate(ticket)
    return {key: ticket[key] for key in ["worker", "run", "nonce"]}


def _validate_launch(value):
    sci = value.get("version") == 2
    policy_field = "execution-profile" if sci else "namespace-policy"
    _common(value, "launch", None, {"owner", "endpoint", "unit", "frozen", policy_field, "unit-policy"},
            version=2 if sci else 1)
    _closed(value["owner"], {"worker", "run", "nonce"})
    for key in ["worker", "run", "nonce"]:
        inputs.canonical_uuid(value["owner"][key])
    # Validate without connecting. Never permit credentials/query/redirect
    # destinations to enter a launch record or later recovered request.
    endpoint = value["endpoint"]
    if (not isinstance(endpoint, str) or not 1 <= len(endpoint) <= 2048
            or urllib.parse.urlunsplit(urllib.parse.urlsplit(endpoint)) != endpoint
            or any(ord(char) < 32 or ord(char) == 127 for char in endpoint)):
        raise ValueError("Private worker endpoint must be canonical")
    mcp_workspace.MCP(endpoint, "")
    unit.validate_name(value["unit"])
    frozen = value["frozen"]
    inputs.validate(frozen, frozen["config"])
    if sci:
        runtime.validate_profile(value["execution-profile"])
    if ((not sci and encode(value["namespace-policy"]) != encode(namespace.POLICY))
            or encode(value["unit-policy"]) != encode(unit.SCI_POLICY if sci else unit.POLICY)):
        raise ValueError("Unsupported private execution policy")


def _validate_admitted(value, launch):
    _common(value, "admitted", launch, {"identity"})
    unit.validate_identity(value["identity"])
    if value["identity"]["unit"] != launch["unit"]:
        raise ValueError("Admission belongs to a different physical unit")


def _validate_stopped(value, launch, admitted):
    _common(value, "stopped", admitted, {"observation", "output", "output-sha256"})
    observation = value["observation"]
    _closed(observation, {"version", "identity", "state", "result", "observed-monotonic-ns"})
    if (type(observation["version"]) is not int or observation["version"] != 1
            or encode(observation["identity"]) != encode(admitted["identity"])
            or observation["state"] not in {"inactive", "failed"}
            or not isinstance(observation["result"], str) or not re.fullmatch(r"[a-z][a-z0-9-]{0,79}", observation["result"])
            or type(observation["observed-monotonic-ns"]) is not int or observation["observed-monotonic-ns"] <= 0):
        raise ValueError("Physical stop observation differs from admitted identity")
    output = value["output"]
    if output is not None:
        outputs.validate(launch["frozen"], launch["frozen"]["config"], output)
        if output["exit-code"] == 0 and observation["result"] != "success":
            raise ValueError("A failed execution unit cannot publish successful output")
    if value["output-sha256"] != (digest(output) if output is not None else None):
        raise ValueError("Stopped output identity differs")


def _validate_acknowledged(value, launch, stopped):
    _common(value, "acknowledged", stopped, {"outcome", "receipt"})
    receipt = value["receipt"]
    if value["outcome"] == "stopped":
        _closed(receipt, {"run", "stop-ack?"})
    elif value["outcome"] == "completed":
        _closed(receipt, {"run", "lease", "state", "cancel-requested?", "stop-ack?", "expires-at-ms", "output-id"})
        if (stopped["output"] is None or receipt["lease"] != launch["owner"]["nonce"]
                or receipt["state"] != "completed" or type(receipt["cancel-requested?"]) is not bool
                or type(receipt["expires-at-ms"]) is not int or not 0 <= receipt["expires-at-ms"] < 2 ** 63):
            raise ValueError("Server completion receipt differs")
        inputs.canonical_uuid(receipt["output-id"])
    else:
        raise ValueError("Unknown server acknowledgment outcome")
    if receipt["run"] != launch["owner"]["run"] or receipt["stop-ack?"] is not True:
        raise ValueError("Server has not acknowledged this exact work")


def _chain(directory):
    records = {phase: _read(directory, phase) for phase in PHASES}
    prior = None
    for phase in PHASES:
        value = records[phase]
        if value is None:
            if any(records[later] is not None for later in PHASES[PHASES.index(phase) + 1:]):
                raise ValueError("Private journal is missing an earlier phase")
            break
        if phase == "launch":
            _validate_launch(value)
        elif phase == "admitted":
            _validate_admitted(value, prior)
        elif phase == "stopped":
            _validate_stopped(value, records["launch"], prior)
        else:
            _validate_acknowledged(value, records["launch"], prior)
        prior = value
    os.fsync(directory)
    return records


def load(folder):
    """Validate and durably adopt existing phases; never authorize a replay."""
    directory = claim._directory(folder)
    try:
        return _chain(directory)
    finally:
        os.close(directory)


def _publish(directory, phase, value):
    encoded = encode(value)
    if len(encoded) > MAX_RECORD_BYTES:
        raise ValueError("Private journal record exceeds its bound")
    staged = None
    try:
        existing = _read(directory, phase)
        if existing is None:
            staged = ".phase-" + uuid.uuid4().hex
            fd = os.open(staged, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                         0o600, dir_fd=directory)
            with os.fdopen(fd, "wb") as stream:
                os.fchmod(stream.fileno(), 0o600)
                claim._regular_info(os.fstat(stream.fileno()))
                stream.write(encoded)
                stream.flush()
                os.fsync(stream.fileno())
            try:
                os.link(staged, phase + ".json", src_dir_fd=directory, dst_dir_fd=directory, follow_symlinks=False)
            except FileExistsError:
                existing = _read(directory, phase)
                if existing is None:
                    raise ValueError("Published private journal phase disappeared")
        if existing is not None and encode(existing) != encoded:
            raise ValueError("Immutable private journal phase conflicts")
        if staged is not None:
            os.unlink(staged, dir_fd=directory)
            staged = None
        os.fsync(directory)
        return json.loads(encoded)
    finally:
        if staged is not None:
            os.unlink(staged, dir_fd=directory)


def prepare(folder, ticket, endpoint, frozen, config, *, execution_profile=None):
    """Persist allocation intent BEFORE StartTransientUnit; require live claim.

    The caller, not a persisted ticket, establishes successful live admission.
    Reopening this phase after a crash never authorizes execution or relaunch.
    """
    inputs.validate(frozen, config)
    owner = _owner(ticket)
    directory = claim._directory(folder)
    try:
        chain = _chain(directory)
        previous = chain["launch"]
        value = {"version": 2 if execution_profile is not None else 1,
                 "phase": "launch", "previous": None, "owner": owner,
                 "endpoint": endpoint, "unit": previous["unit"] if previous else unit.unit_name(),
                 "frozen": frozen, "namespace-policy": namespace.POLICY, "unit-policy": unit.POLICY}
        if execution_profile is not None:
            del value["namespace-policy"]
            value["execution-profile"] = execution_profile
            value["unit-policy"] = unit.SCI_POLICY
        _validate_launch(value)
        return _publish(directory, "launch", value)
    finally:
        os.close(directory)


def _advance(folder, phase, values):
    directory = claim._directory(folder)
    try:
        chain = _chain(directory)
        prior = chain[PHASES[PHASES.index(phase) - 1]]
        if prior is None:
            raise ValueError("Earlier durable controller phase is required")
        value = {"version": 1, "phase": phase, "previous": digest(prior), **values}
        if phase == "admitted":
            _validate_admitted(value, chain["launch"])
        elif phase == "stopped":
            _validate_stopped(value, chain["launch"], prior)
        else:
            _validate_acknowledged(value, chain["launch"], prior)
        return _publish(directory, phase, value)
    finally:
        os.close(directory)


def admit(folder, identity):
    """Persist exact verified guard identity BEFORE GO; restart never sends GO."""
    return _advance(folder, "admitted", {"identity": identity})


def stopped(folder, observation, output=None):
    """After actual UnitBus shutdown, seal its observation and immutable output."""
    return _advance(folder, "stopped", {"observation": observation, "output": output,
                                        "output-sha256": digest(output) if output is not None else None})


def acknowledge(folder, outcome, receipt):
    """Record completion OR shutdown-only acceptance; never conflate the two."""
    return _advance(folder, "acknowledged", {"outcome": outcome, "receipt": receipt})


def unresolved(folder, worker):
    """Inspect EVERY Run directory under one worker before claiming new work.

    Caller owns the permanent worker-wide lock. Any missing/corrupt record or
    unexpected directory entry blocks admission. Private staging residue has
    no authority. Returned summaries contain neither task data nor credentials.
    """
    inputs.canonical_uuid(worker)
    directory = claim._directory(folder)
    pending = []
    try:
        with os.scandir(directory) as runs:
            for entry in runs:
                run = inputs.canonical_uuid(entry.name)
                # Descriptor-relative traversal never follows a Run symlink.
                child = os.open(run, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=directory)
                try:
                    info = os.fstat(child)
                    if info.st_uid != os.getuid() or info.st_mode & 0o077:
                        raise ValueError("Run journal directory must be private and owned by you")
                    ticket = claim._load(child, "claim.json")
                    if ticket["worker"] != worker or ticket["run"] != run:
                        raise ValueError("Run claim belongs to another worker or directory")
                    chain = _chain(child)
                    if chain["launch"] is not None and chain["launch"]["owner"] != _owner(ticket):
                        raise ValueError("Run journal and claim identity differ")
                    if chain["acknowledged"] is None:
                        phase = next((p for p in reversed(PHASES) if chain[p] is not None), "claim-only")
                        pending.append({"run": run, "phase": phase})
                finally:
                    os.close(child)
        os.fsync(directory)
        return sorted(pending, key=lambda record: record["run"])
    finally:
        os.close(directory)
