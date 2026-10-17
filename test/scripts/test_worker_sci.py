import copy
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest
from unittest.mock import Mock, patch
import uuid

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import worker_claim as claim
import worker_controller as controller
import worker_journal as journal
import worker_runtime as runtime
import worker_sci as supervisor
import worker_unit as unit
from worker_io import Frames
import test_worker_inputs as inputs_fixture
import test_worker_journal as journal_fixture
import test_worker_unit as unit_fixture
from test_worker_controller import Backend
import test_worker_runtime as runtime_fixture


def profile(root):
    return runtime.profile(runtime_fixture.RuntimeTest().artifact(root), str(uuid.uuid4()), str(uuid.uuid4()))


class SCITest(unittest.TestCase):
    def test_closed_profile_and_opening_are_distinct_from_native_namespace(self):
        with tempfile.TemporaryDirectory() as root:
            selected = profile(root)
            opening = {"frozen": inputs_fixture.frozen(), "config": inputs_fixture.CONFIG,
                       "execution-profile": selected}
            self.assertEqual(supervisor.request(io.BytesIO(supervisor.encoded(opening)), selected), opening)
            for bad in [dict(opening, argv=["/bin/sh"]), dict(opening, **{"execution-profile": {}})]:
                with self.assertRaises(ValueError):
                    supervisor.request(io.BytesIO(supervisor.encoded(bad)), selected)
            changed = copy.deepcopy(selected)
            changed["runtime-policy"]["heap-mib"] *= 2
            with self.assertRaises(ValueError):
                runtime.validate_profile(changed)

    def test_sci_journal_recovery_does_not_need_runtime_file_or_broker_login(self):
        with tempfile.TemporaryDirectory() as root:
            selected = profile(root)
            ticket = claim.prepare(Path(root) / "claim.json", str(uuid.uuid4()), str(uuid.uuid4()))
            frozen = inputs_fixture.frozen()
            launch = journal.prepare(root, ticket, journal_fixture.ENDPOINT, frozen, frozen["config"],
                                     execution_profile=selected)
            self.assertEqual(2, launch["version"])
            self.assertNotIn("namespace-policy", launch)
            self.assertEqual(unit.SCI_POLICY, launch["unit-policy"])
            Path(selected["runtime"]["jar"]).unlink()
            self.assertEqual(launch, journal.load(root)["launch"])
            with self.assertRaises(ValueError):
                journal.prepare(root, ticket, journal_fixture.ENDPOINT, frozen, frozen["config"])

    def relay(self, selected, frozen, *, count=1, bad=False):
        children = []

        def start(_profile, mode, home, codex_home=None):
            self.assertEqual(selected, _profile)
            self.assertEqual(codex_home is not None, mode == "broker")
            self.assertEqual(0o700, Path(home).stat().st_mode & 0o777)
            command = [sys.executable, "-I", str(Path(__file__).with_name("worker_sci_endpoint_fixture.py")),
                       mode, str(count), "1" if bad else "0", selected["executor-policy-id"], selected["inference-policy-id"]]
            child = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                     stderr=subprocess.DEVNULL, cwd=home, env=runtime.environment(home), close_fds=True)
            channel = Frames(os.dup(child.stdin.fileno()), os.dup(child.stdout.fileno()))
            child.stdin.close()
            child.stdout.close()
            children.append(child)
            return child, channel

        try:
            with patch.object(supervisor, "start", side_effect=start):
                return supervisor.execute(selected, frozen, "/local/broker-only")
        finally:
            self.assertEqual(2, len(children))
            self.assertTrue(all(child.poll() is not None for child in children))

    def test_real_pipe_relay_closes_and_joins_both_scripted_children(self):
        with tempfile.TemporaryDirectory() as root:
            self.assertEqual({"files": {}, "summary": "fixture relay answer", "exit-code": 0},
                             self.relay(profile(root), inputs_fixture.frozen()))

    def test_bad_ready_or_excess_inference_kills_both_children_without_result(self):
        with tempfile.TemporaryDirectory() as root:
            selected = profile(root)
            for options in [{"bad": True}, {"count": 33}]:
                with self.subTest(options=options), self.assertRaises(ValueError):
                    self.relay(selected, inputs_fixture.frozen(), **options)

    def test_guest_start_failure_still_kills_and_joins_started_broker(self):
        with tempfile.TemporaryDirectory() as root:
            child, channel = Mock(), Mock()
            child.poll.return_value = None
            with patch.object(supervisor, "start", side_effect=[(child, channel), OSError("scripted guest start failure")]), \
                    self.assertRaises(OSError):
                supervisor.execute(profile(root), inputs_fixture.frozen(), root)
            child.kill.assert_called_once()
            child.wait.assert_called_once_with(timeout=5)
            channel.close.assert_called_once()

    def test_broker_eof_during_inference_kills_and_joins_both_children(self):
        with tempfile.TemporaryDirectory() as root:
            selected, frozen = profile(root), inputs_fixture.frozen()
            broker, guest, b, g = Mock(), Mock(), Mock(), Mock()
            broker.poll.return_value = guest.poll.return_value = None
            b.read.side_effect = [{"version": 1, "kind": "broker-ready", "config-id": str(uuid.uuid4()),
                                   "inference-policy-id": selected["inference-policy-id"]}, EOFError("scripted broker failure")]
            g.read.side_effect = [{"version": 1, "kind": "guest-ready", "input-id": frozen["input-id"],
                                   "executor-policy-id": selected["executor-policy-id"],
                                   "inference-policy-id": selected["inference-policy-id"]},
                                  {"version": 1, "kind": "inference", "request": {}}]
            with patch.object(supervisor, "start", side_effect=[(broker, b), (guest, g)]), self.assertRaises(EOFError):
                supervisor.execute(selected, frozen, root)
            for child, channel in [(broker, b), (guest, g)]:
                child.kill.assert_called_once()
                child.wait.assert_called_once_with(timeout=5)
                channel.close.assert_called_once()

    def test_unsupported_adapter_is_rejected_before_claim_or_artifact_access(self):
        with tempfile.TemporaryDirectory() as root:
            instance = controller.Controller(root, str(uuid.uuid4()), journal_fixture.ENDPOINT,
                                             dict(inputs_fixture.CONFIG, adapter="claude-code"))
            with patch.object(controller, "MCP") as network, patch.object(runtime, "verify") as verify, \
                    self.assertRaises(ValueError):
                instance.run(str(uuid.uuid4()), "never transmitted", sci={"profile": {}, "codex-home": root})
            network.assert_not_called()
            verify.assert_not_called()
            self.assertEqual([], list(instance.runs.iterdir()))

    def test_controller_seals_sci_stop_before_ack_and_reconciles_without_relaunch(self):
        with tempfile.TemporaryDirectory() as root:
            selected = profile(root)
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            backend = Backend(root, worker, run)
            backend.lose_completion = True
            instance = controller.Controller(root, worker, journal_fixture.ENDPOINT, inputs_fixture.CONFIG)
            physical, frames = Mock(), Mock()
            physical.references = set()

            def capture(name):
                actual = unit_fixture.identity()
                actual["unit"] = name
                actual["cgroup"] = actual["manager"]["root"] + "/app.slice/" + name
                return actual

            physical.capture.side_effect = capture
            physical.wait_stopped.side_effect = lambda actual, **_: {
                "version": 1, "identity": actual, "state": "inactive", "result": "success", "observed-monotonic-ns": 1}
            output = {"files": {}, "summary": "scripted controller", "exit-code": 0}
            frames.read.side_effect = [
                {"kind": "ready", "execution-profile": selected, "input-id": backend.frozen["input-id"]},
                {"kind": "complete", "input-id": backend.frozen["input-id"], "output": output}]
            def framed(writer, reader, tick):
                frames.close.side_effect = lambda: (os.close(writer), os.close(reader))
                return frames

            with patch.object(controller, "MCP", side_effect=backend.factory), \
                    patch.object(controller.unit, "UnitBus", return_value=physical) as factory, \
                    patch.object(controller, "Frames", side_effect=framed), \
                    self.assertRaises(OSError):
                instance.run(run, "synthetic", sci={"profile": selected, "codex-home": root})
            factory.assert_called_once_with(sci=True)
            physical.launch.assert_called_once()
            self.assertEqual(selected, physical.launch.call_args.kwargs["sci"]["profile"])
            chain = journal.load(instance.runs / run)
            self.assertEqual(2, chain["launch"]["version"])
            self.assertEqual(output, chain["stopped"]["output"])
            self.assertIsNone(chain["acknowledged"])
            Path(selected["runtime"]["jar"]).unlink()
            replacement = controller.Controller(root, worker, journal_fixture.ENDPOINT, inputs_fixture.CONFIG)
            with patch.object(controller, "MCP", side_effect=backend.factory), \
                    patch.object(controller.unit, "UnitBus") as factory, patch.object(runtime, "verify") as verify:
                self.assertEqual("completed", replacement.reconcile(run)["outcome"])
            factory.assert_not_called()
            verify.assert_not_called()
            self.assertEqual(1, backend.completes)

    def test_no_go_starts_no_guest_broker_or_provider(self):
        with tempfile.TemporaryDirectory() as root:
            selected = profile(root)
            frozen = inputs_fixture.frozen()
            opening = supervisor.encoded({"frozen": frozen, "config": frozen["config"], "execution-profile": selected})
            incoming = io.TextIOWrapper(io.BytesIO(opening))
            outgoing = io.TextIOWrapper(io.BytesIO())
            with patch.object(supervisor.sys, "stdin", incoming), patch.object(supervisor.sys, "stdout", outgoing), \
                    patch.object(supervisor.Path, "read_text", return_value="0::/user.slice/app.slice/" + unit.unit_name()), \
                    patch.object(supervisor, "execute") as execute:
                supervisor.main(selected, root)
            execute.assert_not_called()

    def test_ready_mismatch_never_sends_go_or_releases_unknown_launch(self):
        with tempfile.TemporaryDirectory() as root:
            selected = profile(root)
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            backend = Backend(root, worker, run)
            instance = controller.Controller(root, worker, journal_fixture.ENDPOINT, inputs_fixture.CONFIG)
            physical, frames = Mock(), Mock()
            physical.launched = set()
            physical.launch.side_effect = lambda name, *_args, **_opts: physical.launched.add(name)
            frames.read.return_value = {"kind": "ready", "execution-profile": {}, "input-id": backend.frozen["input-id"]}

            def framed(writer, reader, tick):
                frames.close.side_effect = lambda: (os.close(writer), os.close(reader))
                return frames

            with patch.object(controller, "MCP", side_effect=backend.factory), \
                    patch.object(controller.unit, "UnitBus", return_value=physical), \
                    patch.object(controller, "Frames", side_effect=framed), self.assertRaises(ValueError):
                instance.run(run, "synthetic", sci={"profile": selected, "codex-home": root})
            self.assertEqual(1, frames.write.call_count)  # opening only, never GO
            physical.capture.assert_not_called()
            physical.abort_launch.assert_called_once()
            physical.release.assert_not_called()
            self.assertEqual(0, backend.stops)
            self.assertEqual(0, backend.completes)
            self.assertIsNone(journal.load(instance.runs / run)["admitted"])


@unittest.skipUnless(os.environ.get("SIMMIS_WORKER_SCI_TESTS") == "1",
                     "Explicit physical SCI profile test: requires dbus/gi and user systemd")
class SCIPhysicalTest(unittest.TestCase):
    def setUp(self):
        self.root = tempfile.TemporaryDirectory(prefix="simmis-sci-unit-test-")
        self.selected = profile(self.root.name)
        self.observer = unit.UnitBus(sci=True)
        self.name = unit.unit_name()
        self.identity = None
        self.channels = []

    def tearDown(self):
        # Only this exact fresh test unit; no server lease or recovery ack.
        subprocess.run(["systemctl", "--user", "stop", self.name], timeout=10,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        for channel in self.channels:
            channel.close()
        if self.identity is not None:
            self.observer.wait_stopped(self.identity)
            self.observer.release(self.identity)
        elif self.name in self.observer.references:
            self.observer.manager.UnrefUnit(self.name, timeout=5)
            self.observer.references.remove(self.name)
        self.observer.close()
        self.root.cleanup()

    def launch(self):
        input_read, input_write = os.pipe()
        output_read, output_write = os.pipe()
        channel = Frames(input_write, output_read)
        self.channels.append(channel)
        try:
            with open(os.devnull, "wb") as errors:
                self.observer.launch(self.name, input_read, output_write, errors.fileno(), timeout_ms=10000,
                                     sci={"profile": self.selected, "codex-home": self.root.name})
        finally:
            os.close(input_read)
            os.close(output_write)
        frozen = inputs_fixture.frozen()
        channel.write(supervisor.encoded({"frozen": frozen, "config": frozen["config"],
                                          "execution-profile": self.selected}), time.monotonic() + 10)
        self.assertEqual({"kind": "ready", "execution-profile": self.selected, "input-id": frozen["input-id"]},
                         channel.read(time.monotonic() + 10))
        self.identity = self.observer.capture(self.name)
        return channel

    def test_real_profile_verifies_bounds_and_starts_no_jvm_before_go(self):
        self.launch()
        group = Path("/sys/fs/cgroup") / self.identity["cgroup"].lstrip("/")
        processes = (group / "cgroup.procs").read_text().splitlines()
        self.assertEqual(1, len(processes))
        self.assertEqual("python3", Path("/proc", processes[0], "comm").read_text().strip())
        self.assertEqual(str(unit.SCI_POLICY["memory-bytes"]), (group / "memory.max").read_text().strip())
        self.observer.stop(self.identity)
        self.assertEqual("inactive", self.observer.wait_stopped(self.identity)["state"])

    def test_direct_parent_exit_does_not_hide_live_descendants_in_sci_unit(self):
        # Scripted processes instead of models, but the actual SCI resource
        # envelope and physical observer are real. No runtime/login is used.
        child = "import signal; signal.signal(signal.SIGTERM,signal.SIG_IGN); print('ready',flush=True); signal.pause()"
        code = ("import json,sys,subprocess; opening=json.loads(sys.stdin.readline()); "
                "print(json.dumps({'kind':'ready','execution-profile':opening['execution-profile'],"
                "'input-id':opening['frozen']['input-id']}),flush=True); "
                "assert sys.stdin.readline()=='GO\\n'; children=[subprocess.Popen([sys.executable,'-I','-c'," + repr(child) + "],"
                "stdout=subprocess.PIPE,text=True,start_new_session=True) for _ in range(2)]; "
                "assert all(c.stdout.readline().strip()=='ready' for c in children); "
                "print('{\"kind\":\"scripted-parent-exiting\"}',flush=True)")
        with patch.object(unit.runtime, "supervisor_argv", return_value=["/usr/bin/python3", "-I", "-c", code]):
            channel = self.launch()
        channel.write(b"GO\n", time.monotonic() + 5)
        self.assertEqual({"kind": "scripted-parent-exiting"}, channel.read(time.monotonic() + 5))
        with self.assertRaises(TimeoutError):
            self.observer.wait_stopped(self.identity, timeout_seconds=0.1)
        self.observer.stop(self.identity)
        self.observer.wait_stopped(self.identity)
        self.assertTrue(unit.cgroup_empty(self.identity["cgroup"]))
