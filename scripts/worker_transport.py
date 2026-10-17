"""Worker-only MCP over a joined, killable HTTP helper with TOTAL deadlines.

Socket inactivity limits alone cannot enforce renewable-lease liveness. The
trusted helper performs no task execution and spawns no descendants. Its
credentials arrive on a private pipe, never argv/environment; parent death
kills it. A lost response remains ambiguous, never a shutdown assertion.
"""
import ctypes
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import time

sys.path.insert(0, str(Path(__file__).resolve().parent))
from mcp_workspace import MCP as HTTP
import worker_inputs as inputs
from worker_io import Frames, MAX_FRAME_BYTES

NETWORK_SECONDS = 5
TOOLS = {"simmis_worker-pending", "simmis_worker-claim", "simmis_worker-input",
         "simmis_worker-heartbeat", "simmis_worker-complete", "simmis_worker-stop"}


class MCP:
    def __init__(self, url, token, *, lease_proof=None, timeout=NETWORK_SECONDS, on_failure=None):
        HTTP(url, token, lease_proof=lease_proof, timeout=timeout)  # validate only
        if type(timeout) not in {int, float} or not 0 < timeout <= NETWORK_SECONDS:
            raise ValueError("Invalid worker total network deadline")
        self.url, self.token, self.proof = url, token, lease_proof
        self.timeout, self.on_failure = timeout, on_failure
        self.process, self.frames = None, None

    def _terminate(self):
        # The trusted helper has no child-spawning operation. Join the actual
        # process, not a timer or missing PID assertion, before dropping pipes.
        if self.process is not None:
            if self.process.poll() is None:
                self.process.kill()
            self.process.wait()
        if self.frames is not None:
            self.frames.close()
            self.frames = None

    def _request(self, value):
        if self.frames is None:
            raise OSError("Worker transport is not open")
        deadline = time.monotonic() + self.timeout
        try:
            encoded = json.dumps(value, separators=(",", ":"), allow_nan=False).encode() + b"\n"
            self.frames.write(encoded, deadline)
            response = self.frames.read(deadline)
            inputs._closed(response, {"ok", "value"})
            if response["ok"] is not True:
                raise OSError("Worker MCP request failed; outcome may be ambiguous")
            return response["value"]
        except BaseException:
            try:
                # Request candidate stop BEFORE joining a stalled HTTP helper.
                # This callback grants no physical proof or remote ack.
                if self.on_failure is not None:
                    self.on_failure()
            finally:
                self._terminate()
            raise

    def __enter__(self):
        if self.process is not None:
            raise ValueError("Worker transports are single use")
        incoming, writer = os.pipe()
        reader, outgoing = os.pipe()
        self.frames = Frames(writer, reader)
        try:
            self.process = subprocess.Popen(
                [sys.executable, "-I", str(Path(__file__).resolve()), "--child", str(os.getpid())],
                stdin=incoming, stdout=outgoing, stderr=subprocess.DEVNULL, close_fds=True,
                cwd="/", env={"PATH": "/usr/bin:/bin", "LANG": "C.UTF-8"})
        except BaseException:
            self.frames.close()
            self.frames = None
            raise
        finally:
            os.close(incoming)
            os.close(outgoing)
        self._request({"op": "open", "url": self.url, "token": self.token, "lease-proof": self.proof})
        return self

    def call(self, name, arguments):
        if name not in TOOLS:
            raise ValueError("Worker transport cannot invoke human or arbitrary tools")
        return self._request({"op": "call", "name": name, "arguments": arguments})

    def __exit__(self, *_args):
        try:
            if self.frames is not None:
                self._request({"op": "close"})
        except Exception:
            # Session deletion is not a lease/assignment operation. Its failure
            # must not change an already durable acknowledged result.
            pass
        finally:
            self._terminate()


def _read():
    encoded = sys.stdin.buffer.readline(MAX_FRAME_BYTES + 1)
    if not encoded:
        return None
    if len(encoded) > MAX_FRAME_BYTES or not encoded.endswith(b"\n"):
        raise ValueError("Invalid worker transport frame")
    value = json.loads(encoded.decode("utf-8"), object_pairs_hook=inputs._unique_object,
                       parse_constant=inputs._invalid_constant)
    if type(value) is not dict:
        raise ValueError("Invalid worker transport object")
    return value


def _emit(ok, value=None):
    encoded = json.dumps({"ok": ok, "value": value}, separators=(",", ":"), allow_nan=False).encode() + b"\n"
    if len(encoded) > MAX_FRAME_BYTES:
        raise ValueError("Worker transport response exceeds its bound")
    sys.stdout.buffer.write(encoded)
    sys.stdout.buffer.flush()


def child(parent_pid):
    libc = ctypes.CDLL(None, use_errno=True)
    # PR_SET_PDEATHSIG, before reading any credentials. The PID check closes
    # the race in which the parent dies before prctl takes effect.
    if libc.prctl(1, signal.SIGKILL, 0, 0, 0) != 0 or os.getppid() != parent_pid:
        raise ValueError("Worker transport lost its owning controller")
    opening = _read()
    inputs._closed(opening, {"op", "url", "token", "lease-proof"})
    if opening["op"] != "open" or not isinstance(opening["token"], str) or not 1 <= len(opening["token"]) <= 16384:
        raise ValueError("Invalid worker transport opening")
    with HTTP(opening["url"], opening["token"], lease_proof=opening["lease-proof"], timeout=NETWORK_SECONDS) as client:
        _emit(True)
        while True:
            request = _read()
            if request is None:
                return
            if request.get("op") == "close":
                inputs._closed(request, {"op"})
                client.close()
                _emit(True)
                return
            inputs._closed(request, {"op", "name", "arguments"})
            if request["op"] != "call" or request["name"] not in TOOLS or type(request["arguments"]) is not dict:
                raise ValueError("Invalid worker transport call")
            _emit(True, client.call(request["name"], request["arguments"]))


if __name__ == "__main__":
    try:
        if len(sys.argv) != 3 or sys.argv[1] != "--child":
            raise ValueError("Use the trusted worker controller")
        child(int(sys.argv[2]))
    except BaseException:
        try:
            _emit(False)
        except BaseException:
            pass
        sys.exit(1)
