import copy
import io
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import Mock, patch
import uuid

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import mcp_client
import worker_driver as driver
import worker_runtime as runtime
import test_worker_runtime as runtime_fixture


ENDPOINT = "http://127.0.0.1:48889/mcp"
ROOM = "88888888-8888-4888-8888-888888888888"


def selected(root):
    return runtime.profile(runtime_fixture.RuntimeTest().artifact(root), str(uuid.uuid4()), str(uuid.uuid4()))


def reply(arguments):
    return {"worker": arguments["worker"], "room": ROOM, "config": arguments["config"],
            "config-id": "99999999-9999-4999-8999-999999999999", "generation": 1,
            "credential": "simmis-worker." + arguments["worker"] + "." + "x" * 43}


class DriverTest(unittest.TestCase):
    def test_benchmark_cli_preserves_explicit_evaluation_split(self):
        args = ["worker_driver.py", "--state", "/private/fixture-worker", "--session", "/private/session",
                "benchmark", "--request-id", str(uuid.uuid4()), "--name", "echo",
                "--expected-bundle", str(uuid.uuid4()), "--split", "development"]
        with patch.object(sys, "argv", args), \
                patch.object(driver, "state_root", return_value=Path("/private/fixture-worker")), \
                patch.object(driver, "operate", return_value={}) as operate, patch("sys.stdout", new_callable=io.StringIO):
            driver.main()
            self.assertEqual("development", operate.call_args.args[3]["split"])

    def test_benchmark_cli_is_explicit_serial_discard_only_and_finite(self):
        request, bundle = str(uuid.uuid4()), str(uuid.uuid4())
        args = ["worker_driver.py", "--state", "/private/fixture-worker", "--session", "/private/session",
                "benchmark", "--request-id", request, "--name", "echo", "--expected-bundle", bundle]
        with patch.object(sys, "argv", args + ["--case", "one", "--case", "two", "--repetitions", "2"]), \
                patch.object(driver, "state_root", return_value=Path("/private/fixture-worker")), \
                patch.object(driver, "operate", return_value={}) as operate, patch("sys.stdout", new_callable=io.StringIO):
            driver.main()
            self.assertEqual("benchmark", operate.call_args.args[2])
            self.assertEqual({"request-id": request, "name": "echo", "expected-bundle": bundle,
                              "repetitions": 2, "cases": ["one", "two"],
                              "limits": {"timeout-ms": 300000, "settlement": "discard"}},
                             operate.call_args.args[3])
        for invalid in [["--repetitions", "3"], ["--case", "one", "--case", "one"],
                        ["--timeout-ms", "600001"], ["--settlement", "review"],
                        sum((["--case", str(i)] for i in range(5)), [])]:
            with patch.object(sys, "argv", args + invalid), \
                    patch.object(driver, "state_root", return_value=Path("/private/fixture-worker")), \
                    patch.object(driver, "operate") as operate, patch("sys.stderr", new_callable=io.StringIO):
                with self.assertRaises(SystemExit):
                    driver.main()
                operate.assert_not_called()

    def test_workflow_cli_uses_exact_bundle_and_literal_string_maps(self):
        request, bundle = str(uuid.uuid4()), str(uuid.uuid4())
        args = ["worker_driver.py", "--state", "/private/fixture-worker", "--session", "/private/session",
                "workflow", "--request-id", request, "--name", "echo", "--expected-bundle", bundle,
                "--params", '{"subject":"literal {subject}"}', "--inputs", '{"/docs/input.md":"text"}']
        with patch.object(sys, "argv", args), patch.object(driver, "state_root", return_value=Path("/private/fixture-worker")), \
                patch.object(driver, "operate", return_value={}) as operate, patch("sys.stdout", new_callable=io.StringIO):
            driver.main()
            self.assertEqual("workflow", operate.call_args.args[2])
            fields = operate.call_args.args[3]
            self.assertEqual(bundle, fields["expected-bundle"])
            self.assertEqual({"subject": "literal {subject}"}, fields["params"])
            self.assertEqual({"/docs/input.md": "text"}, fields["inputs"])
            self.assertEqual(300000, fields["limits"]["timeout-ms"])

    def test_cli_deadline_includes_cold_start_and_keeps_the_finite_maximum(self):
        args = ["worker_driver.py", "--state", "/private/fixture-worker", "--session", "/private/session",
                "task", "--request-id", str(uuid.uuid4()), "--task", "Bounded task"]
        with patch.object(sys, "argv", args), patch.object(driver, "state_root", return_value=Path("/private/fixture-worker")), \
                patch.object(driver, "operate", return_value={}) as operate, patch("sys.stdout", new_callable=io.StringIO):
            driver.main()
            self.assertEqual(operate.call_args.args[-1]["limits"]["timeout-ms"], 300000)
        with patch.object(sys, "argv", args + ["--timeout-ms", "600001"]), \
                patch.object(driver, "state_root", return_value=Path("/private/fixture-worker")), \
                patch.object(driver, "operate") as operate, patch("sys.stderr", new_callable=io.StringIO):
            with self.assertRaises(SystemExit):
                driver.main()
            operate.assert_not_called()

    def prepare(self, root):
        selection = selected(root)
        with patch.object(driver, "preflight", return_value=selection), \
                patch.object(driver, "human_call", side_effect=lambda _endpoint, _session, name, args: reply(args)):
            result = driver.enroll(root, ENDPOINT, "/private/fixture-session", "mutable-room-slug",
                                   selection["runtime"], "fixture-codex-model")
        return selection, result

    def test_enrollment_persists_exact_intent_before_network_and_recovers_lost_reply(self):
        with tempfile.TemporaryDirectory() as root:
            selection = selected(root)
            calls = []

            def network(endpoint, session, name, args):
                persisted = driver.record(root, "worker-intent")
                self.assertEqual(persisted["worker"], args["worker"])
                self.assertEqual({"version": 1, "worker": persisted["worker"], "endpoint": endpoint},
                                 driver.record(root, "controller"))
                calls.append(copy.deepcopy(args))
                if len(calls) == 1:
                    raise OSError("scripted committed enrollment reply loss")
                return reply(args)

            with patch.object(driver, "preflight", return_value=selection) as preflight, \
                    patch.object(driver, "human_call", side_effect=network):
                with self.assertRaises(OSError):
                    driver.enroll(root, ENDPOINT, "private-session", "room", selection["runtime"], "fixture-model")
                result = driver.enroll(root, ENDPOINT, "private-session", "room", selection["runtime"], "fixture-model")
                self.assertEqual(calls[0], calls[1])
                self.assertTrue(result["enrolled"])
                self.assertNotIn("credential", result)
                preflight.assert_called_once()
            for file in ["worker-intent.json", "worker-enrollment.json", "controller.json"]:
                self.assertEqual(0o600, (Path(root) / file).stat().st_mode & 0o777)
            self.assertNotIn("simmis-worker.", (Path(root) / "worker-intent.json").read_text())

    def test_enrollment_retry_cannot_change_destination_room_model_or_worker(self):
        with tempfile.TemporaryDirectory() as root:
            selection, result = self.prepare(root)
            for endpoint, room, model, worker in [
                    ("https://different.example/mcp", "mutable-room-slug", "fixture-codex-model", None),
                    (ENDPOINT, "other-room", "fixture-codex-model", None),
                    (ENDPOINT, "mutable-room-slug", "other-model", None),
                    (ENDPOINT, "mutable-room-slug", "fixture-codex-model", str(uuid.uuid4()))]:
                with patch.object(driver, "human_call") as network, self.assertRaises(ValueError):
                    driver.enroll(root, endpoint, "private-session", room, selection["runtime"], model, worker)
                network.assert_not_called()
            self.assertEqual(result["worker"], driver.enrollment(root)[0]["worker"])

    def test_post_enrollment_controls_pin_canonical_room_and_never_fund_implicitly(self):
        with tempfile.TemporaryDirectory() as root:
            _, issued = self.prepare(root)
            calls = []
            with patch.object(driver, "human_call", side_effect=lambda endpoint, session, name, args:
                              calls.append((name, args)) or {"id": str(uuid.uuid4()), "credential": "never print"}):
                allowance = driver.operate(root, "private-session", "allowance",
                                           {"request-id": str(uuid.uuid4()), "max-worker-admissions": 1})
                task = driver.operate(root, "private-session", "task", {"request-id": str(uuid.uuid4()), "task": "fixture"})
                driver.operate(root, "private-session", "workflow",
                               {"request-id": str(uuid.uuid4()), "name": "echo", "expected-bundle": str(uuid.uuid4())})
                driver.operate(root, "private-session", "benchmark",
                               {"request-id": str(uuid.uuid4()), "name": "echo", "expected-bundle": str(uuid.uuid4()),
                                "repetitions": 2, "cases": ["one", "two"],
                                "limits": {"timeout-ms": 300000, "settlement": "discard"}})
                driver.operate(root, "private-session", "revoke", {})
            self.assertEqual(["simmis_local-worker-allowance", "simmis_local-worker-task",
                              "simmis_local-worker-workflow", "simmis_local-worker-benchmark", "simmis_local-worker-revoke"],
                             [name for name, _ in calls])
            self.assertTrue(all(args["room"] == ROOM for _, args in calls))
            self.assertEqual(issued["worker"], calls[1][1]["worker"])
            self.assertEqual(issued["worker"], calls[2][1]["worker"])
            self.assertEqual(issued["worker"], calls[3][1]["worker"])
            self.assertEqual(2, calls[3][1]["repetitions"])
            self.assertEqual("discard", calls[3][1]["limits"]["settlement"])
            self.assertNotIn("credential", allowance)
            self.assertNotIn("credential", task)

    def test_artifact_and_login_absence_do_not_block_existing_run_reconciliation(self):
        with tempfile.TemporaryDirectory() as root:
            selection, issued = self.prepare(root)
            Path(selection["runtime"]["jar"]).unlink()
            instance = Mock()
            instance.reconcile.return_value = {"outcome": "stopped"}
            run = str(uuid.uuid4())
            with patch.object(driver, "Controller", return_value=instance) as controller, \
                    patch.object(runtime, "verify", side_effect=AssertionError("Recovery must not open runtime")), \
                    patch.object(runtime, "environment", side_effect=AssertionError("Recovery must not open login")), \
                    patch.object(driver, "WorkerMCP") as worker_network, patch.object(driver, "human_call") as human:
                self.assertEqual({"run": run, "outcome": "stopped"}, driver.run_one(root, run=run, reconcile=True))
            instance.reconcile.assert_called_once_with(run)
            instance.run.assert_not_called()
            instance.close.assert_called_once()
            worker_network.assert_not_called()
            human.assert_not_called()
            self.assertEqual(issued["worker"], controller.call_args.args[1])

    def test_one_run_passes_login_path_only_to_local_controller_not_mcp_or_output(self):
        with tempfile.TemporaryDirectory() as root:
            selection, issued = self.prepare(root)
            run = str(uuid.uuid4())
            instance = Mock()
            instance.run.return_value = {"outcome": "completed"}
            with patch.object(driver, "Controller", return_value=instance), patch.object(driver, "WorkerMCP") as network:
                network.return_value.__enter__.return_value.call.return_value = {"runs": [run]}
                result = driver.run_one(root, codex_home=root)
            self.assertEqual({"run": run, "outcome": "completed"}, result)
            network.assert_called_once_with(ENDPOINT, "simmis-worker." + issued["worker"] + "." + "x" * 43, timeout=5)
            self.assertEqual(root, instance.run.call_args.kwargs["sci"]["codex-home"])
            self.assertEqual(selection, instance.run.call_args.kwargs["sci"]["profile"])
            self.assertNotIn("credential", result)

    def test_session_destination_and_mode_are_bound_before_any_auth_network(self):
        with tempfile.TemporaryDirectory() as root:
            path = Path(root) / "session.json"
            session = {"origin": "https://dev.simm.is", "mcp_mode": "worker-control",
                       "access_token": "fixture-secret", "mcp_endpoint": ENDPOINT}
            mcp_client.save(path, session)
            with patch.object(mcp_client, "exchange", return_value={"token": "fixture-control"}) as exchange:
                for endpoint in ["https://different.example/mcp", "http://127.0.0.1:48890/mcp", "https://dev.simm.is/mcp"]:
                    with self.assertRaises(ValueError):
                        mcp_client.session_credential(path, expected_mode="worker-control", expected_endpoint=endpoint)
                exchange.assert_not_called()
                self.assertEqual("fixture-control", mcp_client.session_credential(
                    path, expected_mode="worker-control", expected_endpoint=ENDPOINT))
                exchange.assert_called_once()
            session.pop("mcp_endpoint")
            mcp_client.save(path, session)
            with patch.object(mcp_client, "exchange", return_value={"token": "fixture-control"}) as exchange:
                with self.assertRaises(ValueError):
                    mcp_client.session_credential(path, expected_mode="worker-control", expected_endpoint=ENDPOINT)
                exchange.assert_not_called()
                self.assertEqual("fixture-control", mcp_client.session_credential(
                    path, expected_mode="worker-control", expected_endpoint="https://dev.simm.is/mcp"))
            session["mcp_mode"] = "work"
            mcp_client.save(path, session)
            with patch.object(mcp_client, "exchange") as exchange, self.assertRaises(ValueError):
                mcp_client.session_credential(path, expected_mode="worker-control", expected_endpoint="https://dev.simm.is/mcp")
            exchange.assert_not_called()
