"""Private MCP credential helper. Stdout of `headers` is credential material.

Login keeps a private web session locally, never the password. Only a short-lived
MCP credential is passed to the coding client (read-only unless login explicitly
uses --access work). The separate worker-control mode is only for the private
operator driver, with an explicitly bound MCP destination. No SSH/operator REPL
credentials are needed by this program. Requires Python 3 on Linux/macOS.
"""
import argparse
import fcntl
import getpass
import json
import os
from pathlib import Path
import stat
import sys
import tempfile
import urllib.error
import urllib.parse
import urllib.request


class NoRedirects(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        raise ValueError("Authentication redirects are refused")


def origin(value):
    url = urllib.parse.urlsplit(value)
    if (not url.hostname or url.username or url.password or url.query or url.fragment
            or url.path not in ("", "/")
            or not (url.scheme == "https" or
                    (url.scheme == "http" and url.hostname in ("127.0.0.1", "::1", "localhost")))):
        raise ValueError("Use an HTTPS origin, or HTTP on loopback only")
    return urllib.parse.urlunsplit((url.scheme, url.netloc, "", "", ""))


def mcp_endpoint(value):
    if (not isinstance(value, str) or not 1 <= len(value) <= 2048
            or any(ord(c) < 32 or ord(c) == 127 for c in value)):
        raise ValueError("Expected a canonical MCP destination")
    parsed = urllib.parse.urlsplit(value)
    origin(urllib.parse.urlunsplit((parsed.scheme, parsed.netloc, "", "", "")))
    if (urllib.parse.urlunsplit(parsed) != value or parsed.path != "/mcp"
            or parsed.query or parsed.fragment):
        raise ValueError("Expected a canonical /mcp destination")
    return value


def post(base, route, body=None, token=None):
    headers = {"Content-Type": "application/json", "Accept": "application/json"}
    if token:
        headers["Authorization"] = "Bearer " + token
    request = urllib.request.Request(origin(base) + route,
                                     json.dumps(body or {}).encode(), headers)
    # Don't send credentials via environment-controlled proxies or redirects.
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}), NoRedirects())
    with opener.open(request, timeout=20) as response:
        data = response.read(1024 * 1024 + 1)
        if len(data) > 1024 * 1024:
            raise ValueError("Authentication response too large")
        return json.loads(data)


def private_file(path, flags):
    fd = os.open(path, flags | os.O_NOFOLLOW, 0o600)
    info = os.fstat(fd)
    if (not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid()
            or info.st_mode & 0o077):
        os.close(fd)
        raise ValueError("Credential and lock files must be owned by you with mode 600")
    return fd


def save(path, session):
    # Atomic replace protects an existing session if writing is interrupted.
    fd, name = tempfile.mkstemp(prefix=".mcp-session-", dir=path.parent)
    try:
        with os.fdopen(fd, "w") as stream:
            json.dump(session, stream)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(name, path)
    finally:
        if os.path.exists(name):
            os.unlink(name)


def exchange(path, session):
    base = origin(session["origin"])
    mode = session.get("mcp_mode", "read")
    if mode not in ("read", "work", "worker-control"):
        raise ValueError("Unknown MCP access mode")

    def credential():
        result = post(base, "/auth/mcp-token", {"mode": mode}, token=session["access_token"])
        if result.get("mode", "read") != mode:
            raise ValueError("Server did not issue the requested MCP mode")
        return result

    try:
        return credential()
    except urllib.error.HTTPError as error:
        if error.code != 401:
            raise
    renewed = post(base, "/auth/refresh", {"refresh_token": session["refresh_token"]})
    if not renewed.get("access_token"):
        raise ValueError("Login session cannot be renewed")
    session["access_token"] = renewed["access_token"]
    if renewed.get("refresh_token"):
        session["refresh_token"] = renewed["refresh_token"]
    # Save rotation BEFORE another request, so a transient failure doesn't lose it.
    save(path, session)
    return credential()


def session_credential(path, *, expected_mode, expected_endpoint):
    """Private operator-helper API. Never upgrades a stored session's mode.

    The returned token is credential material, not a tool argument or log.
    Serialize refresh rotation with the same permanent lock as `headers`.
    """
    path = Path(path).expanduser().absolute()
    lock_fd = private_file(str(path) + ".lock", os.O_CREAT | os.O_RDWR)
    with os.fdopen(lock_fd, "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        with os.fdopen(private_file(path, os.O_RDONLY)) as stream:
            encoded = stream.read(1048577)
        if len(encoded) > 1048576:
            raise ValueError("Private MCP session exceeds its bound")
        session = json.loads(encoded)
        if session.get("mcp_mode", "read") != expected_mode:
            raise ValueError("Explicit matching MCP login mode is required")
        # A tunnel alias is an explicit LOGIN choice, never a room/task or
        # per-operation destination. Old sessions default only to their own
        # HTTPS origin's /mcp; they do not silently trust new hosts/ports.
        destination = mcp_endpoint(expected_endpoint)
        bound = mcp_endpoint(session.get("mcp_endpoint", origin(session["origin"]) + "/mcp"))
        if destination != bound:
            raise ValueError("MCP destination differs from the private login binding")
        result = exchange(path, session)
        token = result.get("token")
        if not isinstance(token, str) or not token:
            raise ValueError("No MCP credential returned")
        return token


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("login", "headers"))
    parser.add_argument("--session", type=Path,
                        default=Path.home() / ".config/simmis/mcp-session.json")
    parser.add_argument("--origin", default="https://dev.simm.is")
    parser.add_argument("--email")
    parser.add_argument("--access", choices=("read", "work", "worker-control"),
                        help="Login only: explicitly choose work or private worker-control access; default read")
    parser.add_argument("--mcp-endpoint", help="Login only: explicitly bind an SSH-tunnel /mcp destination")
    args = parser.parse_args()
    if args.command != "login" and (args.access is not None or args.mcp_endpoint is not None):
        raise ValueError("Choose access mode and destination during login, not header renewal")
    path = args.session.expanduser().absolute()
    path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    lock_fd = private_file(str(path) + ".lock", os.O_CREAT | os.O_RDWR)
    with os.fdopen(lock_fd, "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        if args.command == "login":
            # Validate the destination before asking for the password.
            base = origin(args.origin)
            endpoint = mcp_endpoint(args.mcp_endpoint or base + "/mcp")
            email = args.email or input("Simmis email: ")
            result = post(base, "/auth/login", {"email": email, "password": getpass.getpass()})
            session = {"origin": base, "access_token": result["access_token"],
                       "refresh_token": result["refresh_token"],
                       "mcp_mode": args.access or "read", "mcp_endpoint": endpoint}
            # Check MCP availability before replacing an existing session.
            credential = post(base, "/auth/mcp-token", {"mode": session["mcp_mode"]},
                              token=session["access_token"])
            if credential.get("mode", "read") != session["mcp_mode"]:
                raise ValueError("Server did not issue the requested MCP mode")
            if path.exists() or path.is_symlink():
                os.close(private_file(path, os.O_RDONLY))
            save(path, session)
            print("Simmis login saved privately; MCP access: " + session["mcp_mode"] + ".",
                  file=sys.stderr)
        else:
            with os.fdopen(private_file(path, os.O_RDONLY)) as stream:
                session = json.load(stream)
            credential = exchange(path, session)
            token = credential.get("token")
            if not isinstance(token, str) or not token:
                raise ValueError("No MCP credential returned")
            print(json.dumps({"Authorization": "Bearer " + token}))


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, KeyError, EOFError):
        # HTTP errors can include headers/body containing secrets; don't dump them.
        sys.exit("Simmis authentication unavailable. Check connectivity, private file permissions, "
                 "and MCP availability; run login again if your session expired.")
