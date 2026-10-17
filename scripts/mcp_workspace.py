"""Download a verified, read-only workspace snapshot as a cloneable Git bundle.

Authenticate first with mcp_client.py login. MCP stays on your SSH tunnel;
neither credentials nor server paths enter the bundle. Existing files are never
overwritten. This exports current files, not original repository history.
"""
import argparse
import base64
import fcntl
import hashlib
import hmac
import json
import os
from pathlib import Path
import re
import sys
import tempfile
import urllib.parse
import urllib.request
import uuid

import mcp_client as credentials

MAX_BUNDLE = 9437184
CHUNK = 16384


class MCP:
    def __init__(self, url, token, *, lease_proof=None):
        parsed = urllib.parse.urlsplit(url)
        credentials.origin(urllib.parse.urlunsplit((parsed.scheme, parsed.netloc, "", "", "")))
        if parsed.path != "/mcp" or parsed.query or parsed.fragment:
            raise ValueError("Use the /mcp endpoint without query or fragment")
        if lease_proof is not None and (not isinstance(lease_proof, str)
                                        or not re.fullmatch(r"[A-Za-z0-9_-]{43}", lease_proof)):
            raise ValueError("Invalid worker claim proof")
        self.url, self.token, self.session, self.sequence = url, token, None, 0
        # Initialize the session with its proposed proof. Adding a header to
        # an already initialized proofless session cannot upgrade authority.
        # Keep credential material out of tool arguments and diagnostics.
        self._lease_proof = lease_proof
        self.opener = urllib.request.build_opener(urllib.request.ProxyHandler({}), credentials.NoRedirects())

    def request(self, method, params):
        self.sequence += 1
        headers = {"Authorization": "Bearer " + self.token, "Content-Type": "application/json",
                   "Accept": "application/json, text/event-stream", "MCP-Protocol-Version": "2025-06-18"}
        if self._lease_proof is not None:
            headers["Simmis-Lease-Proof"] = self._lease_proof
        if self.session:
            headers["Mcp-Session-Id"] = self.session
        body = {"jsonrpc": "2.0", "method": method, "params": params}
        if not method.startswith("notifications/"):
            body["id"] = self.sequence
        req = urllib.request.Request(self.url, json.dumps(body).encode(), headers)
        with self.opener.open(req, timeout=30) as response:
            self.session = response.headers.get("Mcp-Session-Id", self.session)
            data = response.read(131073)
            if len(data) > 131072:
                raise ValueError("MCP response exceeds limit")
            if not data and method.startswith("notifications/"):
                return None
            result = json.loads(data)
            if not isinstance(result, dict) or result.get("error") or result.get("id") != body.get("id"):
                raise ValueError("MCP request refused")
            return result["result"]

    def __enter__(self):
        self.request("initialize", {"protocolVersion": "2025-06-18", "capabilities": {},
                                    "clientInfo": {"name": "simmis-workspace", "version": "1"}})
        self.request("notifications/initialized", {})
        return self

    def __exit__(self, *args):
        if self.session:
            headers = {"Authorization": "Bearer " + self.token, "Mcp-Session-Id": self.session}
            if self._lease_proof is not None:
                headers["Simmis-Lease-Proof"] = self._lease_proof
            req = urllib.request.Request(self.url, method="DELETE", headers=headers)
            try:
                with self.opener.open(req, timeout=10):
                    pass
            except OSError:
                pass

    def call(self, name, arguments):
        result = self.request("tools/call", {"name": name, "arguments": arguments})
        if not isinstance(result, dict) or result.get("isError"):
            raise ValueError("Workspace read refused; check access or restart after workspace changes")
        if "result" in result.get("structuredContent", {}):
            return result["structuredContent"]["result"]
        content = result.get("content", [])
        if len(content) != 1 or content[0].get("type") != "text":
            raise ValueError("Invalid workspace response")
        return json.loads(content[0]["text"])


def download(call, room, destination):
    descriptor = call("simmis_workspace-bundle", {"room": room})
    if not isinstance(descriptor, dict):
        raise ValueError("Invalid snapshot descriptor")
    size = descriptor.get("bytes")
    snapshot = descriptor.get("snapshot")
    if (type(size) is not int or not 1 <= size <= MAX_BUNDLE
            or descriptor.get("history") is not False
            or type(descriptor.get("files")) is not int or not 0 <= descriptor["files"] <= 512
            or not isinstance(snapshot, str) or str(uuid.UUID(snapshot)) != snapshot
            or not re.fullmatch(r"[0-9a-f]{64}", descriptor.get("sha256", ""))
            or not re.fullmatch(r"[0-9a-f]{40}", descriptor.get("commit", ""))):
        raise ValueError("Invalid snapshot descriptor")
    destination = Path(destination).absolute()
    fd, staged = tempfile.mkstemp(prefix=".simmis-bundle-", dir=destination.parent)
    try:
        digest, offset = hashlib.sha256(), 0
        with os.fdopen(fd, "wb") as stream:
            while offset < size:
                part = call("simmis_workspace-bundle-chunk", {
                    "room": room, "expected-snapshot": snapshot, "offset": offset, "length": CHUNK})
                if (not isinstance(part, dict) or any(part.get(key) != descriptor[key] for key in
                        ("snapshot", "bytes", "sha256", "commit", "files", "history"))
                        or type(part.get("offset")) is not int or part["offset"] != offset):
                    raise ValueError("Snapshot changed or offsets do not match")
                data = base64.b64decode(part["base64"], validate=True)
                end = offset + len(data)
                if (not 1 <= len(data) <= CHUNK or end > size
                        or (end < size and (type(part.get("next-offset")) is not int or part["next-offset"] != end))
                        or (end == size and "next-offset" in part)):
                    raise ValueError("Invalid snapshot chunk length")
                stream.write(data)
                digest.update(data)
                offset = end
            if not hmac.compare_digest(digest.hexdigest(), descriptor["sha256"]):
                raise ValueError("Snapshot digest mismatch")
            stream.flush()
            os.fsync(stream.fileno())
        # Atomic publication that refuses an existing file OR symlink, even if
        # another process creates one during the download. Never replace it.
        os.link(staged, destination)
    finally:
        os.unlink(staged)
    return descriptor


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--session", type=Path, default=Path.home() / ".config/simmis/mcp-session.json")
    parser.add_argument("--mcp-url", default="http://127.0.0.1:47990/mcp")
    parser.add_argument("--room", required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    path = args.session.expanduser().absolute()
    with os.fdopen(credentials.private_file(str(path) + ".lock", os.O_CREAT | os.O_RDWR), "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        with os.fdopen(credentials.private_file(path, os.O_RDONLY)) as stream:
            token = credentials.exchange(path, json.load(stream))["token"]
    with MCP(args.mcp_url, token) as mcp:
        descriptor = download(mcp.call, args.room, args.output)
    print(json.dumps({"output": str(args.output), **descriptor}))


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, KeyError, TypeError):
        sys.exit("Workspace download failed; no existing destination was overwritten. "
                 "Check login, tunnel, room access and workspace limits, or restart after edits.")
