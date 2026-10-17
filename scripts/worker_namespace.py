"""Linux task-namespace primitive, not a worker controller or MCP capability.

A trusted local controller must verify its unit's memory/swap/process/CPU/time
limits before sending GO, then prove whole-unit exit before acknowledging a
server lease. This module cannot claim work, publish results, certify or merge.
Its argv is selected by trusted LOCAL adapter code, never by room input.
No coding CLI adapter or credential mount is supplied here.
"""
import ctypes
import json
import os
from pathlib import Path
import signal
import subprocess
import sys

# Python -I does not add the script directory. Only this read-only trusted
# code directory joins the import path, never the writable task directory.
sys.path.insert(0, str(Path(__file__).resolve().parent))
import worker_inputs as inputs
import worker_outputs as outputs


POLICY = {"version": 1, "work-bytes": 134217728, "runtime-bytes": 16777216,
          "shm-bytes": 8388608, "network": False, "host-writable-mounts": False}
MAX_REQUEST_BYTES = inputs.POLICY["max-input-json-bytes"] + 262144


def namespace_argv():
    """Fixed read-only host runtime and byte-bounded writable tmpfs mounts.

    The caller MUST launch this inside an already capped, verified cgroup.
    Inodes are bounded indirectly by charged kernel memory, not a node quota.
    Nothing permits arbitrary extra host mounts or server-selected options.
    """
    command = ["bwrap", "--unshare-all", "--as-pid-1", "--new-session", "--clearenv"]
    for folder in ["/usr", "/bin", "/lib", "/lib64"]:
        if Path(folder).exists():
            command += ["--ro-bind", folder, folder]
    command += ["--proc", "/proc", "--dev", "/dev", "--remount-ro", "/proc",
                "--remount-ro", "/dev",
                "--perms", "0700", "--size", str(POLICY["work-bytes"]), "--tmpfs", "/work",
                "--perms", "0700", "--size", str(POLICY["runtime-bytes"]), "--tmpfs", "/runtime",
                "--perms", "1777", "--size", str(POLICY["shm-bytes"]), "--tmpfs", "/dev/shm",
                "--symlink", "/runtime/tmp", "/tmp",
                "--ro-bind", str(Path(__file__).resolve().parent), "/opt/simmis-worker",
                "--chdir", "/work", "--remount-ro", "/", "--",
                "/usr/bin/python3", "-I", "/opt/simmis-worker/worker_namespace.py", "--inside"]
    return command


def read_request(stream):
    """One bounded JSON line, followed later by a separate exact GO line.

    The locally selected argv belongs to this physical host envelope, not the
    server's closed frozen-input schema. No shell expansion is performed.
    """
    encoded = stream.readline(MAX_REQUEST_BYTES + 1)
    if len(encoded) > MAX_REQUEST_BYTES or not encoded.endswith(b"\n"):
        raise ValueError("Invalid namespace request framing")
    request = json.loads(encoded.decode("utf-8"), object_pairs_hook=inputs._unique_object,
                         parse_constant=inputs._invalid_constant)
    inputs._closed(request, {"frozen", "config", "argv"})
    inputs.validate(request["frozen"], request["config"])
    command = request["argv"]
    if (type(command) is not list or not 1 <= len(command) <= 64
            or any(not isinstance(arg, str) or not 1 <= len(arg) <= 4096 or "\x00" in arg for arg in command)
            or not command[0].startswith("/")):
        raise ValueError("Expected a bounded local adapter argv")
    for arg in command:
        arg.encode("utf-8")
    return request


def protect_guard():
    """Refuse host execution and prevent same-UID task access to guard state."""
    if os.getpid() != 1:
        raise ValueError("The execution guard must own its private PID namespace")
    libc = ctypes.CDLL(None, use_errno=True)
    libc.prctl.argtypes = [ctypes.c_int, ctypes.c_ulong, ctypes.c_ulong,
                          ctypes.c_ulong, ctypes.c_ulong]
    libc.prctl.restype = ctypes.c_int
    if libc.prctl(4, 0, 0, 0, 0) != 0 or libc.prctl(3, 0, 0, 0, 0) != 0:
        raise OSError("Cannot protect the execution guard")  # SET/GET_DUMPABLE
    status = dict(line.split(":", 1) for line in Path("/proc/self/status").read_text().splitlines() if ":" in line)
    if (status["NoNewPrivs"].strip() != "1"
            or any(int(status[field].strip(), 16) for field in ["CapEff", "CapPrm", "CapInh", "CapBnd", "CapAmb"])):
        raise ValueError("Namespace privileges were not dropped")
    for folder, limit in [("/work", POLICY["work-bytes"]), ("/runtime", POLICY["runtime-bytes"]),
                          ("/dev/shm", POLICY["shm-bytes"])]:
        volume = os.statvfs(folder)
        if volume.f_blocks * volume.f_frsize != limit:
            raise ValueError("Namespace scratch bound differs from the local policy")
    if Path("/run/user").exists() or Path("/sys/fs/cgroup").exists() or Path("/home").exists():
        raise ValueError("Unexpected host control surface in task namespace")


class Cancelled(Exception):
    pass


def _cancel(_signal, _frame):
    raise Cancelled("Namespace cancellation requested")


def _kill_namespace_tasks():
    try:
        # In this private PID namespace, Linux excludes PID 1 and the sender.
        os.kill(-1, signal.SIGKILL)
    except ProcessLookupError:
        pass


def reap_tasks(child):
    """No deadline, missing PID or signal result substitutes for real ECHILD.

    The outer unit has its own finite shutdown deadline. If a process cannot
    be reaped, it must kill the unit and retain exposure until physical exit.
    """
    _kill_namespace_tasks()
    child.wait()
    reaped = 0
    while True:
        try:
            os.waitpid(-1, 0)
            reaped += 1
            _kill_namespace_tasks()
        except InterruptedError:
            continue
        except ChildProcessError:
            return reaped


def _emit(value):
    encoded = json.dumps(value, separators=(",", ":")).encode("utf-8") + b"\n"
    if len(encoded) > MAX_REQUEST_BYTES:
        raise ValueError("Namespace response exceeds its bound")
    sys.stdout.buffer.write(encoded)
    sys.stdout.buffer.flush()


def inside():
    protect_guard()
    os.umask(0o077)
    signal.signal(signal.SIGTERM, _cancel)
    signal.signal(signal.SIGINT, _cancel)
    request = read_request(sys.stdin.buffer)
    frozen, config = request["frozen"], request["config"]
    root = inputs.materialize(frozen, config, "/work")
    for folder in ["home", "tmp", "cache"]:
        Path("/runtime", folder).mkdir(mode=0o700)
    _emit({"kind": "ready", "policy": POLICY, "input-id": frozen["input-id"]})
    if sys.stdin.buffer.readline(4) != b"GO\n":
        return  # EOF or ambiguous admission never authorizes execution.
    child = None
    with open("/runtime/task-input", "w+b") as prompt, open("/runtime/task-log", "w+b") as log:
        prompt.write(frozen["task"].encode("utf-8"))
        prompt.seek(0)
        try:
            child = subprocess.Popen(request["argv"], cwd=root, stdin=prompt, stdout=log, stderr=log,
                                     close_fds=True, env={"PATH": "/usr/bin:/bin", "HOME": "/runtime/home",
                                                         "TMPDIR": "/runtime/tmp", "XDG_CACHE_HOME": "/runtime/cache"})
            try:
                exit_code = child.wait(timeout=frozen["limits"]["timeout-ms"] / 1000)
                exit_code = min(255, exit_code if exit_code >= 0 else 128 - exit_code)
            except subprocess.TimeoutExpired:
                exit_code = 124
        finally:
            # SIGKILL from the outer unit remains effective while cleanup runs.
            previous = signal.pthread_sigmask(signal.SIG_BLOCK, {signal.SIGTERM, signal.SIGINT})
            try:
                reaped = reap_tasks(child) if child is not None else 0
            finally:
                signal.pthread_sigmask(signal.SIG_SETMASK, previous)
        log.seek(0)
        summary = log.read(16000).decode("utf-8", errors="replace")[:16000]
        output = outputs.collect(root, frozen, config, summary, exit_code)
    _emit({"kind": "complete", "input-id": frozen["input-id"], "output": output,
           "reaped-descendants": reaped})


if __name__ == "__main__":
    try:
        if sys.argv[1:] != ["--inside"]:
            raise ValueError("Use the trusted local controller")
        inside()
    except (Exception, KeyboardInterrupt):
        sys.exit("Task namespace failed; no shutdown or import authority is implied")
