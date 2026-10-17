"""Linux test-browser subreaper. No Simmis credentials or agent execution.

The parent may request stop with SIGTERM. Exit acknowledges actual ECHILD,
not signal delivery, pipe closure or the browser main process's disappearance.
Uninterruptible cleanup stays owned; the CI job's outer deadline is the bound.
"""
import ctypes
import os
from pathlib import Path
import signal
import subprocess
import sys


class Stop(Exception):
    pass


def stop(_signum, _frame):
    # Parent death may follow an explicit stop. A second signal must not
    # interrupt unwinding before reap() takes ownership of cleanup.
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    signal.signal(signal.SIGINT, signal.SIG_IGN)
    raise Stop()


def prepare():
    parent = os.getppid()
    libc = ctypes.CDLL(None, use_errno=True)
    libc.prctl.argtypes = [ctypes.c_int, ctypes.c_ulong, ctypes.c_ulong, ctypes.c_ulong, ctypes.c_ulong]
    libc.prctl.restype = ctypes.c_int
    signal.signal(signal.SIGTERM, stop)
    signal.signal(signal.SIGINT, stop)
    if libc.prctl(36, 1, 0, 0, 0) != 0 or libc.prctl(1, signal.SIGTERM, 0, 0, 0) != 0:
        raise OSError("Browser subreaper unavailable")
    if parent == 1 or os.getppid() != parent:
        raise Stop()


def reap():
    # Direct child identities cannot be reused until WE reap them. Orphans
    # are adopted here even if they detached from Chromium's process group.
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    signal.signal(signal.SIGINT, signal.SIG_IGN)
    children = Path(f"/proc/self/task/{os.getpid()}/children")
    while True:
        for pid in children.read_text().split():
            try:
                os.kill(int(pid), signal.SIGKILL)
            except ProcessLookupError:
                pass
        try:
            os.waitpid(-1, 0)
        except InterruptedError:
            continue
        except ChildProcessError:
            return


def main(argv):
    prepare()
    try:
        child = subprocess.Popen(argv, stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                                 stderr=sys.stderr, start_new_session=True)
        return child.wait()
    except Stop:
        return 0
    finally:
        reap()
        # Child stdout is /dev/null: this pipe belongs to the supervisor only.
        # Missing receipt leaves profile cleanup unresolved in the caller.
        sys.stdout.write("browser-tree-reaped-v1\n")
        sys.stdout.flush()


if __name__ == "__main__":
    try:
        if len(sys.argv) < 2:
            raise ValueError("Missing browser command")
        sys.exit(main(sys.argv[1:]))
    except Stop:
        sys.exit(1)
    except Exception:
        sys.stderr.write("Browser supervisor failed\n")
        sys.exit(1)
