import copy
import json
import os
from pathlib import Path
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
import worker_transport as transport
import test_worker_inputs as fixture
import test_worker_journal as journal_fixture
import test_worker_transport as transport_fixture


class ControllerTest(unittest.TestCase):
    def test_receipt_is_exact_and_live_not_just_a_matching_run(self):
        ticket = {"worker": str(uuid.uuid4()), "run": str(uuid.uuid4()), "nonce": str(uuid.uuid4())}
        value = {"run": ticket["run"], "lease": ticket["nonce"], "state": "claimed",
                 "cancel-requested?": False, "stop-ack?": False, "expires-at-ms": int(time.time() * 1000) + 30000}
        self.assertEqual(controller.checked_receipt(value, ticket), value)
        controller.require_live(value)
        for key, bad in [("lease", str(uuid.uuid4())), ("run", str(uuid.uuid4())), ("state", "queued"),
                         ("stop-ack?", 0), ("expires-at-ms", True), ("cancel-requested?", "false")]:
            with self.subTest(key=key), self.assertRaises(ValueError):
                controller.checked_receipt(dict(value, **{key: bad}), ticket)
        with self.assertRaises(controller.Cancelled):
            controller.require_live(dict(value, **{"cancel-requested?": True}))
        with self.assertRaises(ValueError):
            controller.checked_receipt(dict(value, state="completed"), ticket)

    def test_input_pagination_services_heartbeat_before_next_chunk(self):
        ticket = {"run": str(uuid.uuid4()), "nonce": str(uuid.uuid4())}
        client = Mock()
        receipt = {"run": ticket["run"], "lease": ticket["nonce"], "state": "claimed",
                   "cancel-requested?": False, "stop-ack?": False, "expires-at-ms": int(time.time() * 1000) + 30000}
        client.call.side_effect = lambda name, args: receipt if name == "simmis_worker-heartbeat" else "chunk"
        clock = [0]
        with patch.object(controller.time, "monotonic", side_effect=lambda: clock[0]):
            lease = controller.Lease(client, ticket)
            self.assertEqual(lease.input_call("simmis_worker-input", {}), "chunk")
            clock[0] = 1
            self.assertEqual(lease.input_call("simmis_worker-input", {}), "chunk")
            clock[0] = 6
            self.assertEqual(lease.input_call("simmis_worker-input", {}), "chunk")
        self.assertEqual([call.args[0] for call in client.call.call_args_list],
                         ["simmis_worker-heartbeat", "simmis_worker-input", "simmis_worker-input",
                          "simmis_worker-heartbeat", "simmis_worker-input"])

    def test_stale_or_expired_claimed_receipt_never_authorizes_go(self):
        ticket = {"run": str(uuid.uuid4()), "nonce": str(uuid.uuid4())}
        value = {"run": ticket["run"], "lease": ticket["nonce"], "state": "claimed",
                 "cancel-requested?": False, "stop-ack?": False, "expires-at-ms": int(time.time() * 1000) + 30000}
        with self.assertRaises(controller.Cancelled):
            controller.require_live(dict(value, **{"expires-at-ms": int(time.time() * 1000) - 1}))
        client = Mock()
        clock = [0]

        def delayed(*_args):
            clock[0] = 6
            return value

        client.call.side_effect = delayed
        with patch.object(controller.time, "monotonic", side_effect=lambda: clock[0]), self.assertRaises(controller.Cancelled):
            controller.Lease(client, ticket).tick(force=True)

    def test_frame_protocol_rejects_duplicates_nonobjects_oversize_and_eof(self):
        for encoded in [b'{"x":1,"x":2}\n', b"null\n", b"[]\n", b'{"x":NaN}\n', b"\xff\n", b"truncated"]:
            with self.subTest(encoded=encoded):
                incoming, sender = os.pipe()
                receiver, outgoing = os.pipe()
                frames = controller.Frames(outgoing, incoming, Mock())
                try:
                    os.write(sender, encoded)
                    os.close(sender)
                    with self.assertRaises((ValueError, EOFError)):
                        frames.read(time.monotonic() + 1)
                finally:
                    frames.close()
                    os.close(receiver)
        incoming, sender = os.pipe()
        receiver, outgoing = os.pipe()
        frames = controller.Frames(outgoing, incoming, Mock())
        try:
            frames.pending = bytearray(b"x" * controller.namespace.MAX_REQUEST_BYTES)
            with self.assertRaises(ValueError):
                frames.read(time.monotonic() + 1)
            with self.assertRaises(TimeoutError):
                frames.write(b"GO\n", time.monotonic() - 1)
        finally:
            frames.close()
            os.close(sender)
            os.close(receiver)

    def test_unresolved_other_run_blocks_new_claim_before_any_network(self):
        with tempfile.TemporaryDirectory() as root:
            worker, old_run, new_run = [str(uuid.uuid4()) for _ in range(3)]
            instance = controller.Controller(root, worker, journal_fixture.ENDPOINT, fixture.CONFIG)
            folder = controller._child_directory(instance.runs, old_run)
            claim.prepare(folder / "claim.json", worker, old_run)
            with patch.object(controller, "MCP") as network, self.assertRaises(ValueError):
                instance.run(new_run, "never sent", ["/usr/bin/true"])
            network.assert_not_called()
            self.assertFalse((instance.runs / new_run).exists())
            instance.close()

    def test_canonical_state_root_is_pinned_before_network_or_worker_changes(self):
        with tempfile.TemporaryDirectory() as root:
            worker = str(uuid.uuid4())
            instance = controller.Controller(root, worker, journal_fixture.ENDPOINT, fixture.CONFIG)
            instance.close()
            for other_worker, endpoint in [(str(uuid.uuid4()), journal_fixture.ENDPOINT),
                                            (worker, "http://localhost:48889/mcp")]:
                with self.assertRaises(ValueError):
                    controller.Controller(root, other_worker, endpoint, fixture.CONFIG)
            self.assertEqual(json.loads((Path(root) / "controller.json").read_bytes()),
                             {"version": 1, "worker": worker, "endpoint": journal_fixture.ENDPOINT})

    def test_seal_orders_real_observation_then_durable_record_then_reference_release(self):
        instance = controller.Controller.__new__(controller.Controller)
        instance.identity = {"unit": "synthetic"}
        instance.observer = Mock()
        events = []
        instance.observer.wait_stopped.side_effect = lambda *args, **kwargs: events.append("physical") or "observation"
        instance.observer.release.side_effect = lambda *args: events.append("release")
        with patch.object(controller.journal, "load", return_value={"stopped": None}), \
                patch.object(controller.journal, "stopped", side_effect=lambda *args: events.append("durable") or "record"):
            self.assertEqual(instance._seal("folder", "output"), "record")
        self.assertEqual(events, ["physical", "durable", "release"])

    def test_failed_sealing_never_releases_the_reference(self):
        instance = controller.Controller.__new__(controller.Controller)
        instance.identity, instance.observer = {"unit": "synthetic"}, Mock()
        with patch.object(controller.journal, "load", return_value={"stopped": None}), \
                patch.object(controller.journal, "stopped", side_effect=OSError("Uncertain durability")), self.assertRaises(OSError):
            instance._seal("folder", "output")
        instance.observer.release.assert_not_called()


class Backend:
    """Protocol fixture only; physical execution remains real in opt-in tests."""
    def __init__(self, root, worker, run):
        self.root, self.worker, self.run = Path(root), worker, run
        self.frozen = fixture.frozen()
        self.frozen["limits"]["timeout-ms"] = 10000
        self.state, self.nonce, self.output = "queued", None, None
        self.beats, self.stops, self.completes = 0, 0, 0
        self.cancel_after = None
        self.lose_completion = False
        self.calls = []
        self.input, _ = fixture.transport(self.frozen)

    def receipt(self):
        value = {"run": self.run, "lease": self.nonce, "state": self.state, "expires-at-ms": int(time.time() * 1000) + 30000,
                 "cancel-requested?": self.state == "quarantined", "stop-ack?": self.state in {"completed", "stopped"}}
        if self.state == "completed":
            value["output-id"] = "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
        return value

    def factory(self, url, token, **kwargs):
        backend = self

        class Client:
            def __enter__(self):
                return self

            def __exit__(self, *_args):
                pass

            def call(self, name, arguments):
                backend.calls.append(name)
                if name == "simmis_worker-claim":
                    backend.nonce, backend.state = arguments["nonce"], "claimed"
                    return backend.receipt()
                if name == "simmis_worker-heartbeat":
                    backend.beats += 1
                    if backend.cancel_after and backend.beats >= backend.cancel_after and backend.state == "claimed":
                        backend.state = "quarantined"
                    return backend.receipt()
                if name == "simmis_worker-input":
                    return backend.input(name, arguments)
                # A remote ack must never precede the durable physical stop.
                chain = journal.load(backend.root / "runs" / backend.run)
                if chain["stopped"] is None:
                    raise AssertionError("Remote acknowledgment preceded durable physical shutdown")
                if name == "simmis_worker-complete":
                    backend.completes += 1
                    backend.output, backend.state = copy.deepcopy(arguments["output"]), "completed"
                    if backend.lose_completion:
                        backend.lose_completion = False
                        raise OSError("Synthetic committed completion reply loss")
                    return backend.receipt()
                if name == "simmis_worker-stop":
                    backend.stops += 1
                    if backend.state != "completed":
                        backend.state = "stopped"
                    return {"run": backend.run, "stop-ack?": True}
                raise AssertionError("Unknown fixture worker operation")

        return Client()


@unittest.skipUnless(os.environ.get("SIMMIS_WORKER_NAMESPACE_TESTS") == "1",
                     "Explicit host integration: requires dbus/gi, user systemd and bubblewrap")
class ControllerIntegrationTest(unittest.TestCase):
    def run_case(self, root, code, *, lose=False, cancel_after=None):
        worker, run = str(uuid.uuid4()), str(uuid.uuid4())
        backend = Backend(root, worker, run)
        backend.lose_completion, backend.cancel_after = lose, cancel_after
        instance = controller.Controller(root, worker, journal_fixture.ENDPOINT, fixture.CONFIG, heartbeat_seconds=0.05)
        with patch.object(controller, "MCP", side_effect=backend.factory):
            try:
                result = instance.run(run, "synthetic enrollment credential", ["/usr/bin/python3", "-I", "-c", code])
            except BaseException as error:
                result = error
        return instance, backend, result

    def test_executes_selected_inputs_and_only_completes_after_real_sealed_stop(self):
        with tempfile.TemporaryDirectory() as root:
            code = ("from pathlib import Path; Path('out').mkdir(); Path('out/result.bin').write_bytes(b'\\x00\\xffresult'); "
                    "Path('bin/test').unlink(); print('complete',end='')")
            instance, backend, result = self.run_case(root, code)
            try:
                self.assertIsInstance(result, dict, repr(result))
                self.assertEqual(result["outcome"], "completed")
                self.assertEqual(backend.output, {"files": {"bin/test": None, "out/result.bin": fixture.file(b"\x00\xffresult")},
                                                  "summary": "complete", "exit-code": 0})
                self.assertGreaterEqual(backend.beats, 4)
                self.assertEqual(backend.completes, 1)
                self.assertEqual(backend.stops, 0)
                self.assertEqual(journal.unresolved(instance.runs, backend.worker), [])
            finally:
                instance.close()

    def test_lost_committed_completion_reply_is_reconciled_not_converted_to_stop(self):
        with tempfile.TemporaryDirectory() as root:
            instance, backend, result = self.run_case(root, "print('once',end='')", lose=True)
            try:
                self.assertIsInstance(result, OSError)
                self.assertEqual(backend.completes, 1)
                self.assertEqual(backend.stops, 0)
                self.assertEqual(journal.unresolved(instance.runs, backend.worker), [{"run": backend.run, "phase": "stopped"}])
                replacement = controller.Controller(root, backend.worker, journal_fixture.ENDPOINT, fixture.CONFIG)
                with patch.object(controller, "MCP", side_effect=backend.factory), patch.object(controller.unit, "UnitBus") as physical:
                    recovered = replacement.reconcile(backend.run)
                self.assertEqual(recovered["outcome"], "completed")
                physical.assert_not_called()  # no launch/replay even for recovery
                self.assertEqual(backend.completes, 1)
                self.assertEqual(backend.stops, 0)
                self.assertEqual(journal.unresolved(instance.runs, backend.worker), [])
                replacement.close()
            finally:
                instance.close()

    def test_explicit_cancellation_seals_physical_stop_without_publishing_output(self):
        with tempfile.TemporaryDirectory() as root:
            # Force cancellation after the mandatory pre-GO renewals. The
            # candidate blocks on a real signal, not a synchronization sleep.
            instance, backend, result = self.run_case(root, "import signal; signal.pause()", cancel_after=8)
            try:
                self.assertIsInstance(result, controller.Cancelled, repr(result))
                self.assertEqual(backend.completes, 0)
                self.assertEqual(backend.stops, 1)
                chain = journal.load(instance.runs / backend.run)
                self.assertIsNone(chain["stopped"]["output"])
                self.assertEqual(chain["acknowledged"]["outcome"], "stopped")
                self.assertEqual(journal.unresolved(instance.runs, backend.worker), [])
            finally:
                instance.close()

    def test_dribbling_http_after_go_stops_the_real_candidate_before_its_runtime_limit(self):
        network = transport_fixture.TransportTest("test_total_deadline_stops_dribbling_http_and_joins_the_helper")
        network.setUp()
        try:
            with tempfile.TemporaryDirectory() as root:
                worker, run = str(uuid.uuid4()), str(uuid.uuid4())
                backend = Backend(root, worker, run)
                instance = controller.Controller(root, worker, journal_fixture.ENDPOINT, fixture.CONFIG, heartbeat_seconds=0.05)
                go_sent, fault_started = [False], []
                ordering = []
                original_write = controller.Frames.write
                original_stop = instance._request_stop
                original_terminate = transport.MCP._terminate

                def stop_first():
                    ordering.append("candidate-stop-request")
                    original_stop()

                def terminate(http):
                    ordering.append("helper-join")
                    original_terminate(http)

                def write(frames, encoded, deadline):
                    original_write(frames, encoded, deadline)
                    if encoded == b"GO\n":
                        go_sent[0] = True

                def factory(*args, **kwargs):
                    client = backend.factory(*args, **kwargs)
                    original_call = client.call

                    def call(name, arguments):
                        if name == "simmis_worker-heartbeat" and go_sent[0] and not fault_started:
                            with transport.MCP(network.url, "synthetic-token", on_failure=instance._request_stop) as http:
                                network.dribble = True
                                http.timeout = 0.2
                                fault_started.append(time.monotonic())
                                http.call(name, arguments)
                        return original_call(name, arguments)

                    client.call = call
                    return client

                try:
                    with patch.object(controller, "MCP", side_effect=factory), \
                            patch.object(controller.Frames, "write", new=write), \
                            patch.object(instance, "_request_stop", side_effect=stop_first), \
                            patch.object(transport.MCP, "_terminate", new=terminate), self.assertRaises(TimeoutError):
                        instance.run(run, "synthetic-enrollment", ["/usr/bin/python3", "-I", "-c", "import signal; signal.pause()"])
                    self.assertTrue(go_sent[0])
                    self.assertEqual(len(fault_started), 1)
                    self.assertLess(time.monotonic() - fault_started[0], 5)  # task's hard runtime is 10s
                    self.assertEqual(backend.completes, 0)
                    self.assertEqual(backend.stops, 1)
                    self.assertEqual(ordering[:2], ["candidate-stop-request", "helper-join"])
                    chain = journal.load(instance.runs / run)
                    self.assertTrue(controller.unit.cgroup_empty(chain["stopped"]["observation"]["identity"]["cgroup"]))
                    self.assertIsNone(chain["stopped"]["output"])
                    self.assertEqual(chain["acknowledged"]["outcome"], "stopped")
                finally:
                    instance.close()
        finally:
            network.tearDown()
