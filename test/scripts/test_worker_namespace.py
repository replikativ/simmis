import ctypes
import errno
import io
import json
import os
from pathlib import Path
import selectors
import subprocess
import sys
import unittest
from unittest.mock import Mock, patch
import uuid

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import worker_namespace as namespace
import test_worker_inputs as fixture


def request(code="pass", timeout=2000):
    frozen = fixture.frozen()
    frozen["limits"]["timeout-ms"] = timeout
    return {"frozen": frozen, "config": fixture.CONFIG.copy(), "argv": ["/usr/bin/python3", "-I", "-c", code]}


class NamespaceTest(unittest.TestCase):
    def test_fixed_namespace_has_no_writable_host_mount_or_network(self):
        command = namespace.namespace_argv()
        self.assertEqual(command[0], "bwrap")
        for flag in ["--unshare-all", "--as-pid-1", "--new-session", "--clearenv"]:
            self.assertIn(flag, command)
        for forbidden in ["--bind", "--dev-bind", "--share-net", "--cap-add", "--unshare-user-try"]:
            self.assertNotIn(forbidden, command)
        for mount in ["/work", "/runtime", "/dev/shm"]:
            index = command.index(mount, command.index("--size"))
            self.assertEqual(command[index - 1], "--tmpfs")
        self.assertEqual(command[-4:], ["/usr/bin/python3", "-I", "/opt/simmis-worker/worker_namespace.py", "--inside"])

    def test_bounded_request_leaves_go_on_a_separate_control_frame(self):
        value = request()
        stream = io.BytesIO(json.dumps(value).encode() + b"\nGO\n")
        self.assertEqual(namespace.read_request(stream), value)
        self.assertEqual(stream.read(), b"GO\n")

    def test_bad_frames_json_and_local_argv_are_refused(self):
        for encoded in [b"{}", b"x" * (namespace.MAX_REQUEST_BYTES + 1), b"\xff\n",
                        b'{"frozen":{},"frozen":{}}\n', b'{"x":NaN}\n']:
            with self.subTest(encoded_length=len(encoded)), self.assertRaises((ValueError, KeyError)):
                namespace.read_request(io.BytesIO(encoded))
        for command in [[], "sh", ["relative"], ["/bin/sh", "\x00"], ["/bin/sh", True], ["/bin/sh", "x" * 4097]]:
            value = request()
            value["argv"] = command
            with self.subTest(command_type=type(command)), self.assertRaises(ValueError):
                namespace.read_request(io.BytesIO(json.dumps(value).encode() + b"\n"))
        value = request()
        value["remote-host-command"] = "not accepted"
        with self.assertRaises(ValueError):
            namespace.read_request(io.BytesIO(json.dumps(value).encode() + b"\n"))

    def test_guard_refuses_execution_outside_its_private_pid_one(self):
        with patch.object(namespace.os, "getpid", return_value=12345), self.assertRaises(ValueError):
            namespace.protect_guard()

    def test_reaper_waits_through_interrupts_until_actual_no_children(self):
        child = Mock()
        with patch.object(namespace, "_kill_namespace_tasks") as kill, \
                patch.object(namespace.os, "waitpid", side_effect=[InterruptedError(), (3, 9), ChildProcessError()]):
            self.assertEqual(namespace.reap_tasks(child), 1)
        child.wait.assert_called_once()
        self.assertEqual(kill.call_count, 2)

    def test_unexpected_reap_failure_never_returns_completion(self):
        with patch.object(namespace, "_kill_namespace_tasks"), \
                patch.object(namespace.os, "waitpid", side_effect=OSError("Unknown physical state")), \
                self.assertRaises(OSError):
            namespace.reap_tasks(Mock())


@unittest.skipUnless(os.environ.get("SIMMIS_WORKER_NAMESPACE_TESTS") == "1",
                     "Explicit host integration: requires Linux user systemd and bubblewrap")
class NamespaceIntegrationTest(unittest.TestCase):
    """No model calls. These tests assert local containment, not lease receipts."""

    def read_line(self, stream):
        with selectors.DefaultSelector() as ready:
            ready.register(stream, selectors.EVENT_READ)
            self.assertTrue(ready.select(timeout=25), "Namespace did not emit its bounded protocol frame")
        encoded = stream.readline(namespace.MAX_REQUEST_BYTES + 1)
        self.assertLessEqual(len(encoded), namespace.MAX_REQUEST_BYTES)
        return json.loads(encoded) if encoded else None

    def run_guard(self, code, *, go=True, timeout=2000, expect_oom=False):
        value = request(code, timeout)
        unit = "simmis-namespace-test-" + uuid.uuid4().hex + ".service"
        command = ["systemd-run", "--user", "--quiet", "--wait", "--pipe", "--unit=" + unit,
                   "--property=Type=exec", "--property=ExitType=cgroup", "--property=Restart=no",
                   "--property=MemoryMax=256M", "--property=MemorySwapMax=0", "--property=TasksMax=64",
                   "--property=CPUQuota=100%", "--property=RuntimeMaxSec=20", "--property=TimeoutStopSec=2",
                   "--property=KillMode=control-group", "--property=OOMPolicy=kill"] + namespace.namespace_argv()
        process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        try:
            process.stdin.write(json.dumps(value).encode() + b"\n")
            process.stdin.flush()
            ready = self.read_line(process.stdout)
            self.assertIsNotNone(ready, "Guard failed before READY")
            self.assertEqual(ready, {"kind": "ready", "policy": namespace.POLICY,
                                     "input-id": value["frozen"]["input-id"]})
            cgroup = subprocess.run(["systemctl", "--user", "show", unit, "-p", "ControlGroup", "--value"],
                                    check=True, capture_output=True, text=True, timeout=5).stdout.strip()
            self.assertTrue(cgroup.startswith("/user.slice/") and ".." not in Path(cgroup).parts)
            location = Path("/sys/fs/cgroup") / cgroup.lstrip("/")
            self.assertEqual((location / "memory.max").read_text().strip(), str(256 * 1024 * 1024))
            self.assertEqual((location / "memory.swap.max").read_text().strip(), "0")
            self.assertEqual((location / "pids.max").read_text().strip(), "64")
            quota, period = map(int, (location / "cpu.max").read_text().split())
            self.assertEqual(quota, period)
            if go:
                process.stdin.write(b"GO\n")
            process.stdin.close()
            result = self.read_line(process.stdout)
            process.wait(timeout=25)
            if expect_oom:
                self.assertIsNone(result, "An OOM-killed namespace must not report successful collection")
                self.assertNotEqual(process.returncode, 0)
                properties = subprocess.run(
                    ["systemctl", "--user", "show", unit, "-p", "Result", "-p", "MemoryPeak",
                     "-p", "ActiveState", "-p", "MainPID", "-p", "ControlGroup"],
                    check=True, capture_output=True, text=True, timeout=5).stdout
                properties = dict(line.split("=", 1) for line in properties.splitlines())
                self.assertEqual(properties["Result"], "oom-kill")
                self.assertEqual(properties["ActiveState"], "failed")
                self.assertEqual(properties["MainPID"], "0")
                if properties["ControlGroup"]:
                    events = Path("/sys/fs/cgroup") / properties["ControlGroup"].lstrip("/") / "cgroup.events"
                    if events.exists():
                        self.assertEqual(dict(line.split() for line in events.read_text().splitlines())["populated"], "0")
                # Kernel memory is charged too. memory.max may transiently
                # overshoot; this is not a claim of an independent inode quota.
                self.assertGreaterEqual(int(properties["MemoryPeak"]), 240 * 1024 * 1024)
            else:
                self.assertEqual(process.returncode, 0, "Physical namespace helper failed")
            # This does not create a remote shutdown receipt. A future
            # controller still needs retained exact-invocation observation.
            return result, value
        finally:
            subprocess.run(["systemctl", "--user", "stop", unit], stdout=subprocess.DEVNULL,
                           stderr=subprocess.DEVNULL, timeout=10)
            if process.poll() is None:
                process.wait(timeout=25)
            if not process.stdin.closed:
                process.stdin.close()
            process.stdout.close()
            process.stderr.close()
            subprocess.run(["systemctl", "--user", "reset-failed", unit], stdout=subprocess.DEVNULL,
                           stderr=subprocess.DEVNULL, timeout=10)

    def test_eof_before_go_never_executes_the_candidate(self):
        result, _ = self.run_guard("raise RuntimeError('must not execute')", go=False)
        self.assertIsNone(result)

    def test_detached_term_resistant_descendant_is_reaped_before_output_collection(self):
        child = "import signal; signal.signal(signal.SIGTERM,signal.SIG_IGN); print('ready',flush=True); signal.pause()"
        code = ("import subprocess,sys; from pathlib import Path; "
                "child=subprocess.Popen([sys.executable,'-I','-c'," + repr(child) + "],"
                "start_new_session=True,stdout=subprocess.PIPE,text=True); "
                "assert child.stdout.readline().strip()=='ready'; "
                "Path('out').mkdir(); Path('out/result.bin').write_bytes(b'\\x00\\xffresult'); print('done')")
        result, value = self.run_guard(code)
        self.assertEqual(result["kind"], "complete")
        self.assertEqual(result["input-id"], value["frozen"]["input-id"])
        self.assertGreaterEqual(result["reaped-descendants"], 1)
        self.assertEqual(result["output"]["exit-code"], 0)
        self.assertEqual(result["output"]["files"], {"out/result.bin": fixture.file(b"\x00\xffresult")})
        self.assertEqual(result["output"]["summary"], "done\n")

    def test_guard_fds_root_and_memory_are_inaccessible_to_the_task(self):
        code = """
import ctypes, errno, os
for path in ['/proc/1/fd/0', '/proc/1/fd/1', '/proc/1/fd/2', '/proc/1/root', '/proc/1/mem']:
    try:
        fd = os.open(path, os.O_RDONLY)
    except PermissionError:
        pass
    else:
        os.close(fd)
        raise AssertionError('guard control surface accessible')
libc = ctypes.CDLL(None, use_errno=True)
assert libc.ptrace(0x4206, 1, 0, 0) == -1 and ctypes.get_errno() == errno.EPERM
class IOVec(ctypes.Structure):
    _fields_ = [('base', ctypes.c_void_p), ('length', ctypes.c_size_t)]
byte = ctypes.c_char()
local, remote = IOVec(ctypes.addressof(byte), 1), IOVec(1, 1)
assert libc.process_vm_readv(1, ctypes.byref(local), 1, ctypes.byref(remote), 1, 0) == -1
assert ctypes.get_errno() == errno.EPERM
assert not os.path.exists('/run/user') and not os.path.exists('/sys/fs/cgroup')
assert sorted(os.listdir('/proc/self/fd')) == ['0', '1', '2', '3']
print('guard protected')
"""
        result, _ = self.run_guard(code)
        self.assertEqual(result["output"]["exit-code"], 0, result["output"]["summary"])
        self.assertEqual(result["output"]["summary"], "guard protected\n")

    def test_timeout_stops_the_task_and_reports_failure_not_success(self):
        result, _ = self.run_guard("import signal; signal.pause()", timeout=1000)
        self.assertEqual(result["output"]["exit-code"], 124)
        self.assertEqual(result["output"]["files"], {})

    def test_workspace_growth_hits_a_real_tmpfs_byte_limit(self):
        code = """
import errno
from pathlib import Path
with open('fill', 'wb', buffering=0) as stream:
    try:
        for _ in range(256):
            stream.write(b'x' * 1048576)
    except OSError as error:
        assert error.errno == errno.ENOSPC
    else:
        raise AssertionError('workspace quota did not refuse growth')
print('bounded scratch')
"""
        result, _ = self.run_guard(code)
        self.assertEqual(result["output"]["exit-code"], 0, result["output"]["summary"])
        self.assertEqual(result["output"]["summary"], "bounded scratch\n")
        self.assertEqual(result["output"]["files"], {})

    def test_empty_file_metadata_is_charged_to_the_capped_cgroup(self):
        code = """
import os
for number in range(5000000):
    fd = os.open('inode-' + str(number), os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    os.close(fd)
raise AssertionError('empty-file growth was not bounded')
"""
        self.run_guard(code, timeout=15000, expect_oom=True)
