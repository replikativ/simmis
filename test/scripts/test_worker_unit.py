import os
from pathlib import Path
import subprocess
import sys
import unittest
from unittest.mock import Mock, patch
import uuid

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import worker_unit as unit
import test_worker_namespace as fixture


def identity():
    name = unit.unit_name()
    return {"version": 1, "unit": name, "boot-id": str(uuid.uuid4()),
            "invocation-id": uuid.uuid4().hex,
            "cgroup": "/user.slice/user-1000.slice/user@1000.service/app.slice/" + name}


class UnitTest(unittest.TestCase):
    def test_only_canonical_non_root_unit_identity_is_accepted(self):
        value = identity()
        self.assertEqual(unit.validate_identity(value), value)
        for field, bad in [("version", True), ("unit", "simmis-dev.service"), ("unit", "../../main"),
                           ("boot-id", "bad"), ("boot-id", None), ("invocation-id", "0" * 32),
                           ("invocation-id", ""), ("invocation-id", True), ("cgroup", "/"),
                           ("cgroup", "/user.slice/../" + value["unit"]), ("cgroup", "/user.slice/not-ours.service"),
                           ("cgroup", "/user.slice//" + value["unit"]), ("cgroup", "/user.slice/./" + value["unit"])]:
            changed = dict(value, **{field: bad})
            with self.subTest(field=field, bad=bad), self.assertRaises(ValueError):
                unit.validate_identity(changed)
        with self.assertRaises(ValueError):
            unit.validate_identity(dict(value, unknown="not accepted"))

    def test_missing_events_in_an_existing_group_is_not_empty(self):
        with patch.object(unit.Path, "read_text", side_effect=FileNotFoundError()), \
                patch.object(unit.Path, "exists", return_value=True), self.assertRaises(ValueError):
            unit.cgroup_empty(identity()["cgroup"])

    def test_cgroup_population_requires_known_kernel_state(self):
        for text, expected in [("populated 0\nfrozen 0\n", True), ("populated 1\nfrozen 0\n", False)]:
            with patch.object(unit.Path, "read_text", return_value=text):
                self.assertEqual(unit.cgroup_empty(identity()["cgroup"]), expected)
        with patch.object(unit.Path, "read_text", return_value="frozen 0\n"), self.assertRaises(ValueError):
            unit.cgroup_empty(identity()["cgroup"])
        with patch.object(unit.Path, "read_text", side_effect=PermissionError()), self.assertRaises(PermissionError):
            unit.cgroup_empty(identity()["cgroup"])

    def test_unknown_observer_or_changed_manager_owner_is_never_proof(self):
        observer = unit.UnitBus.__new__(unit.UnitBus)
        observer.bus = Mock()
        observer.daemon = Mock()
        observer.bus.get_is_connected.return_value = True
        observer.owner, observer.unknown = ":1.known", False
        observer.daemon.GetNameOwner.return_value = ":1.replacement"
        with self.assertRaises(ValueError):
            observer._check()
        observer.unknown = True
        observer.daemon.GetNameOwner.return_value = observer.owner
        with self.assertRaises(ValueError):
            observer._check()

    def test_different_cgroup_namespace_or_subtree_mount_is_not_shutdown_evidence(self):
        mount = "1 2 0:1 / /sys/fs/cgroup ro - cgroup2 cgroup rw"
        with patch.object(unit.Path, "read_text", side_effect=[mount, "0::/init.scope\n"]), self.assertRaises(ValueError):
            unit.verify_cgroup_view(123, "/user.slice/user-1000.slice/user@1000.service")
        for mount in ["1 2 0:1 /subtree /sys/fs/cgroup ro - cgroup2 cgroup rw",
                      "1 2 0:1 / /sys/fs/cgroup ro - tmpfs tmpfs rw", ""]:
            with patch.object(unit.Path, "read_text", return_value=mount), self.assertRaises(ValueError):
                unit.verify_cgroup_view(123, "/user.slice/user-1000.slice/user@1000.service")
        mount = "1 2 0:1 / /sys/fs/cgroup ro - cgroup2 cgroup rw"
        root = "/user.slice/user-1000.slice/user@1000.service"
        with patch.object(unit.Path, "read_text", side_effect=[mount, "0::" + root + "/init.scope\n", "123\n"]):
            unit.verify_cgroup_view(123, root)
        with patch.object(unit.Path, "read_text", side_effect=[mount, "0::" + root + "/init.scope\n", "456\n"]), \
                self.assertRaises(ValueError):
            unit.verify_cgroup_view(123, root)

    def test_wake_dispatched_during_synchronous_snapshot_is_not_lost(self):
        observer = unit.UnitBus.__new__(unit.UnitBus)
        observer.loop, observer.GLib, observer.generation = Mock(), Mock(), 0
        expected = identity()
        count = 0

        def snapshot(_identity):
            nonlocal count
            count += 1
            if count == 1:
                observer._wake()
            return {"ActiveState": "active" if count == 1 else "inactive", "Job": (0, "/")}, {"MainPID": 0, "Result": "success"}

        observer._snapshot = snapshot
        with patch.object(unit, "cgroup_empty", return_value=True):
            self.assertEqual(observer.wait_stopped(expected)["identity"], expected)
        observer.loop.run.assert_not_called()
        self.assertEqual(count, 3)
        observer.GLib.source_remove.assert_called_once()

    def test_lost_reference_reply_holds_unknown_and_requires_new_connection(self):
        observer = unit.UnitBus.__new__(unit.UnitBus)
        observer._check, observer._snapshot = Mock(), Mock()
        observer.references, observer.confirmed = set(), set()
        observer.manager = Mock()
        observer.manager.RefUnit.side_effect = RuntimeError("Synthetic lost reference reply")
        expected = identity()
        with self.assertRaises(RuntimeError):
            observer.retain(expected)
        self.assertIn(expected["unit"], observer.references)
        self.assertNotIn(expected["unit"], observer.confirmed)
        with self.assertRaises(ValueError):
            observer.retain(expected)
        observer.manager.RefUnit.assert_called_once()
        observer._snapshot.assert_not_called()


@unittest.skipUnless(os.environ.get("SIMMIS_WORKER_NAMESPACE_TESTS") == "1",
                     "Explicit host integration: requires dbus/gi, user systemd and bubblewrap")
class UnitIntegrationTest(unittest.TestCase):
    """Physical observations only, no durable receipt or remote lease implied."""

    def setUp(self):
        self.observer = unit.UnitBus()
        self.name = unit.unit_name()
        self.identity = None
        self.streams = []

    def tearDown(self):
        # Exact unique test unit only. This emergency cleanup is NOT a worker
        # recovery path and never acknowledges an external lease.
        subprocess.run(["systemctl", "--user", "stop", self.name], timeout=10,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        for stream in self.streams:
            stream.close()
        if self.observer.bus.get_is_connected():
            if self.identity is not None:
                self.observer.release(self.identity)
            elif self.name in self.observer.references:
                self.observer.manager.UnrefUnit(self.name, timeout=5)
                self.observer.references.remove(self.name)
            self.observer.close()
        subprocess.run(["systemctl", "--user", "reset-failed", self.name], timeout=10,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    def launch(self, code="print('complete')"):
        read_input, write_input = os.pipe()
        read_output, write_output = os.pipe()
        self.input = os.fdopen(write_input, "wb", buffering=0)
        self.output = os.fdopen(read_output, "rb", buffering=0)
        self.streams.extend([self.input, self.output])
        try:
            with open(os.devnull, "wb") as errors:
                self.observer.launch(self.name, read_input, write_output, errors.fileno(), timeout_ms=2000)
        finally:
            os.close(read_input)
            os.close(write_output)
        value = fixture.request(code)
        import json
        self.input.write(json.dumps(value).encode() + b"\n")
        self.assertEqual(fixture.NamespaceIntegrationTest.read_line(self, self.output)["kind"], "ready")
        self.identity = self.observer.capture(self.name)

    def test_natural_exit_retains_exact_invocation_until_explicit_release(self):
        self.launch()
        self.input.write(b"GO\n")
        result = fixture.NamespaceIntegrationTest.read_line(self, self.output)
        self.assertEqual(result["output"]["summary"], "complete\n")
        observed = self.observer.wait_stopped(self.identity)
        self.assertEqual(observed["identity"], self.identity)
        self.assertEqual(observed["state"], "inactive")
        self.assertEqual(observed["result"], "success")
        info, service = self.observer._snapshot(self.identity)
        self.assertEqual(bytes(info["InvocationID"]).hex(), self.identity["invocation-id"])
        self.assertIn(str(service["ControlGroup"]), {"", self.identity["cgroup"]})

    def test_populated_guard_deadline_and_release_refuse_until_exact_stop(self):
        self.launch()
        with self.assertRaises(TimeoutError):
            self.observer.wait_stopped(self.identity, timeout_seconds=0.05)
        with self.assertRaises(TimeoutError):
            self.observer.release(self.identity)
        self.assertIn(self.name, self.observer.references)
        with self.assertRaises(ValueError):
            self.observer.close()
        self.observer.stop(self.identity)
        observed = self.observer.wait_stopped(self.identity)
        self.assertEqual(observed["identity"], self.identity)

    def test_outer_unit_stops_a_go_launched_resistant_session_even_without_guard_cleanup(self):
        # Deliberately omit the production guard's own reaper in this trusted
        # physical fixture: the outer unit must provide independent containment.
        child = "import signal; signal.signal(signal.SIGTERM,signal.SIG_IGN); print('ready',flush=True); signal.pause()"
        code = ("import sys,signal,subprocess; sys.stdin.readline(); "
                "print('{\"kind\":\"ready\"}',flush=True); assert sys.stdin.readline()=='GO\\n'; "
                "child=subprocess.Popen([sys.executable,'-I','-c'," + repr(child) + "],"
                "start_new_session=True,stdout=subprocess.PIPE,text=True); "
                "assert child.stdout.readline().strip()=='ready'; "
                "signal.signal(signal.SIGTERM,lambda *_:sys.exit(0)); "
                "print('{\"kind\":\"descendant-ready\"}',flush=True); signal.pause()")
        command = unit.namespace.namespace_argv()
        command[-4:] = ["/usr/bin/python3", "-I", "-c", code]
        with patch.object(unit.namespace, "namespace_argv", return_value=command):
            self.launch()
        self.input.write(b"GO\n")
        self.assertEqual(fixture.NamespaceIntegrationTest.read_line(self, self.output), {"kind": "descendant-ready"})
        self.observer.stop(self.identity)
        observed = self.observer.wait_stopped(self.identity)
        self.assertEqual(observed["identity"], self.identity)
        self.assertTrue(unit.cgroup_empty(self.identity["cgroup"]))

    def test_wrong_invocation_and_boot_never_receive_stop_observation(self):
        self.launch()
        for field, bad in [("invocation-id", uuid.uuid4().hex), ("boot-id", str(uuid.uuid4()))]:
            with self.subTest(field=field), self.assertRaises(ValueError):
                self.observer.wait_stopped(dict(self.identity, **{field: bad}), timeout_seconds=1)
        self.observer.stop(self.identity)
        self.observer.wait_stopped(self.identity)

    def test_lost_connection_cannot_observe_and_new_owner_only_reconciles(self):
        self.launch()
        self.observer.bus.close()
        with self.assertRaises(ValueError):
            self.observer.wait_stopped(self.identity, timeout_seconds=1)
        replacement = unit.UnitBus()
        try:
            replacement.retain(self.identity)
            with self.assertRaises(ValueError):
                replacement.capture(self.name)  # reattachment is never launch admission
            replacement.stop(self.identity)
            self.assertEqual(replacement.wait_stopped(self.identity)["identity"], self.identity)
            replacement.release(self.identity)
        finally:
            replacement.close()

    def test_lost_launch_reply_keeps_unknown_exposure_and_never_authorizes_go(self):
        manager = self.observer.manager

        class LostReply:
            def StartTransientUnit(_self, *args, **kwargs):
                manager.StartTransientUnit(*args, **kwargs)
                raise RuntimeError("Synthetic committed launch reply loss")

            def __getattr__(_self, name):
                return getattr(manager, name)

        self.observer.manager = LostReply()
        with self.assertRaises(RuntimeError):
            self.launch()
        self.assertIn(self.name, self.observer.references)
        with self.assertRaises(ValueError):
            self.observer.capture(self.name)
        with self.assertRaises(ValueError):
            self.observer.close()
