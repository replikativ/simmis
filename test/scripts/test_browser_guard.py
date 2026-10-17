import os
import selectors
import signal
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


GUARD = Path(__file__).resolve().parents[2] / "scripts/browser_guard.py"

# Deliberately leave the original process group AND close inherited stdio.
# The ready pipe coordinates parent exit/stop; no sleeps synchronize the test.
PROGRAM = """
import os, signal, sys
from pathlib import Path
r,w=os.pipe()
if os.fork()==0:
    os.close(r)
    os.setsid()
    fd=os.open(os.devnull,os.O_RDWR)
    for target in [0,1,2]: os.dup2(fd,target)
    Path(sys.argv[1]).write_text(str(os.getpid()))
    os.write(w,b'R')
    os.close(w)
    signal.pause()
else:
    os.close(w)
    assert os.read(r,1)==b'R'
    os.close(r)
    if sys.argv[2]=='exit': os._exit(7)
    sys.stderr.write('ready\\n')
    sys.stderr.flush()
    signal.pause()
"""


@unittest.skipUnless(sys.platform == "linux", "Browser supervisor uses Linux subreaping")
class BrowserGuardTest(unittest.TestCase):
    def exercise(self, mode):
        with tempfile.TemporaryDirectory(prefix="simmis-browser-reaper-test-") as root:
            probe = Path(root) / "descendant.pid"
            process = subprocess.Popen([sys.executable, "-I", str(GUARD), sys.executable, "-c", PROGRAM, str(probe), mode],
                                       stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            try:
                if mode in {"stop", "repeat"}:
                    with selectors.DefaultSelector() as ready:
                        ready.register(process.stderr, selectors.EVENT_READ)
                        self.assertTrue(ready.select(timeout=5), "Browser fixture did not become ready")
                    self.assertEqual(process.stderr.readline(), b"ready\n")
                    process.terminate()
                    if mode == "repeat":
                        process.send_signal(signal.SIGINT)
                        process.terminate()
                output, errors = process.communicate(timeout=5)
                self.assertEqual(process.returncode, 7 if mode == "exit" else 0, errors)
                self.assertEqual(output, b"browser-tree-reaped-v1\n")
                pid = int(probe.read_text())
                with self.assertRaises(ProcessLookupError):
                    os.kill(pid, 0)
            finally:
                if process.poll() is None:
                    process.terminate()
                process.communicate(timeout=5)

    def test_main_exit_reaps_detached_descendant_which_closed_stdio(self):
        self.exercise("exit")

    def test_stop_signal_reaps_the_entire_owned_subtree(self):
        self.exercise("stop")

    def test_repeated_stop_signals_do_not_interrupt_reaping(self):
        self.exercise("repeat")

    def test_missing_browser_refuses_and_closes_cleanly(self):
        result = subprocess.run([sys.executable, "-I", str(GUARD), "/nonexistent-simmis-test-browser"],
                                capture_output=True, timeout=5)
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stderr, b"Browser supervisor failed\n")
        self.assertEqual(result.stdout, b"browser-tree-reaped-v1\n")
