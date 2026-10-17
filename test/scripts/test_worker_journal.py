import copy
import json
import os
from pathlib import Path
import selectors
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch
import uuid

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import worker_claim as claim
import worker_journal as journal
import worker_outputs as outputs
import test_worker_inputs as fixture
import test_worker_unit as unit_fixture


ENDPOINT = "http://127.0.0.1:48889/mcp"


def setup(folder):
    ticket = claim.prepare(Path(folder) / "claim.json", str(uuid.uuid4()), str(uuid.uuid4()))
    frozen = fixture.frozen()
    launch = journal.prepare(folder, ticket, ENDPOINT, frozen, fixture.CONFIG)
    identity = unit_fixture.identity()
    identity["unit"] = launch["unit"]
    identity["cgroup"] = identity["manager"]["root"] + "/app.slice/" + launch["unit"]
    observation = {"version": 1, "identity": identity, "state": "inactive", "result": "success",
                   "observed-monotonic-ns": 1000000000}
    output = {"files": {"docs/知识.md": None, "out/结果.bin": fixture.file(b"\x00\xffresult", 0o100755)},
              "summary": "bounded output", "exit-code": 0}
    return ticket, frozen, launch, identity, observation, output


def receipt(ticket):
    return {"run": ticket["run"], "lease": ticket["nonce"], "state": "completed",
            "cancel-requested?": False, "stop-ack?": True, "expires-at-ms": 1792227043000,
            "output-id": str(uuid.uuid4())}


class JournalTest(unittest.TestCase):
    def test_exact_immutable_phases_reopen_without_credentials_or_reexecution(self):
        with tempfile.TemporaryDirectory() as folder:
            ticket, frozen, launch, identity, observation, output = setup(folder)
            self.assertEqual(journal.prepare(folder, ticket, ENDPOINT, frozen, fixture.CONFIG), launch)
            admitted = journal.admit(folder, identity)
            stopped = journal.stopped(folder, observation, output)
            acknowledged = journal.acknowledge(folder, "completed", receipt(ticket))
            chain = journal.load(folder)
            self.assertEqual(chain, dict(zip(journal.PHASES, [launch, admitted, stopped, acknowledged])))
            self.assertEqual(admitted["previous"], journal.digest(launch))
            self.assertEqual(stopped["previous"], journal.digest(admitted))
            self.assertEqual(stopped["output-sha256"], journal.digest(output))
            for phase in journal.PHASES:
                path = Path(folder) / (phase + ".json")
                self.assertEqual(path.stat().st_mode & 0o777, 0o600)
                self.assertEqual(path.read_bytes(), journal.encode(chain[phase]))
                self.assertNotIn(ticket["proof"].encode(), path.read_bytes())
            # An independent interpreter can recover only data, not a replay.
            script = ("import sys,json; sys.path.insert(0,sys.argv[1]); import worker_journal as j; "
                      "print(json.dumps(j.load(sys.argv[2]),sort_keys=True))")
            result = subprocess.run([sys.executable, "-I", "-c", script, str(Path(journal.__file__).parent), folder],
                                    check=True, capture_output=True, text=True, timeout=5)
            self.assertEqual(json.loads(result.stdout), chain)

    def test_phases_cannot_skip_admission_or_physical_stop(self):
        with tempfile.TemporaryDirectory() as folder:
            ticket, _, _, identity, observation, output = setup(folder)
            with self.assertRaises(ValueError):
                journal.stopped(folder, observation, output)
            with self.assertRaises(ValueError):
                journal.acknowledge(folder, "completed", receipt(ticket))
            wrong = copy.deepcopy(identity)
            wrong["unit"] = journal.unit.unit_name()
            wrong["cgroup"] = wrong["manager"]["root"] + "/app.slice/" + wrong["unit"]
            with self.assertRaises(ValueError):
                journal.admit(folder, wrong)
            journal.admit(folder, identity)
            with self.assertRaises(ValueError):
                journal.acknowledge(folder, "stopped", {"run": ticket["run"], "stop-ack?": True})

    def test_changed_intent_identity_and_output_never_replace_phase(self):
        with tempfile.TemporaryDirectory() as folder:
            ticket, frozen, launch, identity, observation, output = setup(folder)
            original = (Path(folder) / "launch.json").read_bytes()
            changed = copy.deepcopy(frozen)
            changed["task"] += " different"
            for endpoint, selected in [("http://localhost:48889/mcp", frozen), (ENDPOINT, changed),
                                       (ENDPOINT + "\n", frozen), ("http://remote.example/mcp", frozen),
                                       ("https://token@remote.example/mcp", frozen)]:
                with self.assertRaises(ValueError):
                    journal.prepare(folder, ticket, endpoint, selected, fixture.CONFIG)
                self.assertEqual((Path(folder) / "launch.json").read_bytes(), original)
            journal.admit(folder, identity)
            wrong = copy.deepcopy(observation)
            wrong["identity"]["invocation-id"] = uuid.uuid4().hex
            with self.assertRaises(ValueError):
                journal.stopped(folder, wrong, output)
            stopped = journal.stopped(folder, observation, output)
            changed = dict(output, summary="changed after completion")
            with self.assertRaises(ValueError):
                journal.stopped(folder, observation, changed)
            self.assertEqual(journal.load(folder)["stopped"], stopped)
            self.assertEqual(journal.load(folder)["launch"], launch)

    def test_server_shutdown_only_is_not_completion_or_positive_unit_success(self):
        with tempfile.TemporaryDirectory() as folder:
            ticket, _, _, identity, observation, output = setup(folder)
            journal.admit(folder, identity)
            failed = dict(observation, state="failed", result="oom-kill")
            with self.assertRaises(ValueError):
                journal.stopped(folder, failed, output)
            journal.stopped(folder, failed)
            with self.assertRaises(ValueError):
                journal.acknowledge(folder, "completed", receipt(ticket))
            accepted = journal.acknowledge(folder, "stopped", {"run": ticket["run"], "stop-ack?": True})
            self.assertEqual(accepted["outcome"], "stopped")
            self.assertIsNone(journal.load(folder)["stopped"]["output"])

    def test_malformed_or_wrong_server_acknowledgment_is_refused(self):
        with tempfile.TemporaryDirectory() as folder:
            ticket, _, _, identity, observation, output = setup(folder)
            journal.admit(folder, identity)
            journal.stopped(folder, observation, output)
            for key, value in [("run", str(uuid.uuid4())), ("lease", str(uuid.uuid4())), ("stop-ack?", 1),
                               ("state", "claimed"), ("output-id", "bad"), ("expires-at-ms", True)]:
                bad = dict(receipt(ticket), **{key: value})
                with self.subTest(key=key), self.assertRaises(ValueError):
                    journal.acknowledge(folder, "completed", bad)
            with self.assertRaises(ValueError):
                journal.acknowledge(folder, "stopped", {"run": ticket["run"], "stop-ack?": False})
            self.assertIsNone(journal.load(folder)["acknowledged"])

    def test_duplicate_noncanonical_unknown_and_broken_chain_records_fail_closed(self):
        for corruption in ["duplicate", "nan", "null", "array", "whitespace", "unknown", "bad-link", "oversize", "missing-prior"]:
            with self.subTest(corruption=corruption), tempfile.TemporaryDirectory() as folder:
                _, _, _, identity, _, _ = setup(folder)
                admitted = journal.admit(folder, identity)
                path = Path(folder) / "admitted.json"
                if corruption == "duplicate":
                    path.write_bytes(b'{"version":1,"version":1}')
                elif corruption == "nan":
                    path.write_bytes(b'{"version":NaN}')
                elif corruption == "null":
                    path.write_bytes(b"null")
                elif corruption == "array":
                    path.write_bytes(b"[]")
                elif corruption == "whitespace":
                    path.write_bytes(b" " + journal.encode(admitted))
                elif corruption == "unknown":
                    path.write_bytes(journal.encode(dict(admitted, extra="not accepted")))
                elif corruption == "bad-link":
                    path.write_bytes(journal.encode(dict(admitted, previous="0" * 64)))
                elif corruption == "oversize":
                    path.write_bytes(b"x" * (journal.MAX_RECORD_BYTES + 1))
                else:
                    (Path(folder) / "launch.json").unlink()
                with self.assertRaises(ValueError):
                    journal.load(folder)
                # Malformed existing data must never masquerade as an absent
                # phase during no-overwrite publication either.
                if corruption != "missing-prior":
                    with self.assertRaises(ValueError):
                        journal.admit(folder, identity)

    def test_symlinks_fifos_public_files_and_directories_are_not_journals(self):
        for target in ["symlink", "fifo", "public-file", "public-dir"]:
            with self.subTest(target=target), tempfile.TemporaryDirectory() as folder:
                setup(folder)
                path = Path(folder) / "admitted.json"
                if target == "symlink":
                    path.symlink_to(Path(folder) / "launch.json")
                elif target == "fifo":
                    os.mkfifo(path, 0o600)
                elif target == "public-file":
                    (Path(folder) / "launch.json").chmod(0o644)
                else:
                    Path(folder).chmod(0o755)
                with self.assertRaises(ValueError):
                    journal.load(folder)

    def test_restrictive_umask_and_crash_left_staging_never_replace_authority(self):
        with tempfile.TemporaryDirectory() as folder:
            previous = os.umask(0o777)
            try:
                _, _, launch, identity, _, _ = setup(folder)
                journal.admit(folder, identity)
            finally:
                os.umask(previous)
            residue = Path(folder) / ".phase-crash"
            os.link(Path(folder) / "launch.json", residue)
            self.assertEqual(journal.load(folder)["launch"], launch)
            self.assertEqual(residue.read_bytes(), journal.encode(launch))
            self.assertEqual((Path(folder) / "admitted.json").stat().st_mode & 0o777, 0o600)

    def test_failed_write_or_directory_fsync_never_returns_an_admission(self):
        # Existing launch read+directory barriers precede new phase's file
        # and publication-directory barriers. Fault every relevant position.
        for fail_on in [1, 2, 3, 4]:
            with self.subTest(fail_on=fail_on), tempfile.TemporaryDirectory() as folder:
                _, _, _, identity, _, _ = setup(folder)
                sync, calls = os.fsync, []

                def fsync(fd):
                    calls.append(fd)
                    if len(calls) == fail_on:
                        raise OSError("Synthetic durability failure")
                    return sync(fd)

                with patch.object(journal.os, "fsync", fsync), self.assertRaises(OSError):
                    journal.admit(folder, identity)
                # On exact retry, even a phase published before a failed dir
                # fsync must be resynced before the caller can send GO.
                admitted = journal.admit(folder, identity)
                self.assertEqual(journal.load(folder)["admitted"], admitted)
                self.assertFalse(any(path.name.startswith(".phase-") for path in Path(folder).iterdir()))

    def test_load_resync_failure_never_adopts_a_stopped_record(self):
        with tempfile.TemporaryDirectory() as folder:
            _, _, _, identity, observation, output = setup(folder)
            journal.admit(folder, identity)
            journal.stopped(folder, observation, output)
            with patch.object(journal.os, "fsync", side_effect=OSError("Uncertain durability")), self.assertRaises(OSError):
                journal.load(folder)

    def test_worker_wide_gate_covers_other_runs_claim_only_and_shutdown_without_ack(self):
        with tempfile.TemporaryDirectory() as root:
            worker, runs = str(uuid.uuid4()), [str(uuid.uuid4()), str(uuid.uuid4())]
            paths, tickets = [], []
            for run in runs:
                path = Path(root) / run
                path.mkdir(mode=0o700)
                tickets.append(claim.prepare(path / "claim.json", worker, run))
                paths.append(path)
            self.assertEqual(journal.unresolved(root, worker), sorted([{"run": run, "phase": "claim-only"} for run in runs],
                                                                    key=lambda value: value["run"]))
            frozen = fixture.frozen()
            launch = journal.prepare(paths[0], tickets[0], ENDPOINT, frozen, fixture.CONFIG)
            identity = unit_fixture.identity()
            identity["unit"] = launch["unit"]
            identity["cgroup"] = identity["manager"]["root"] + "/app.slice/" + launch["unit"]
            journal.admit(paths[0], identity)
            observation = {"version": 1, "identity": identity, "state": "failed", "result": "signal", "observed-monotonic-ns": 123}
            journal.stopped(paths[0], observation)
            self.assertIn({"run": runs[0], "phase": "stopped"}, journal.unresolved(root, worker))
            journal.acknowledge(paths[0], "stopped", {"run": runs[0], "stop-ack?": True})
            self.assertEqual(journal.unresolved(root, worker), [{"run": runs[1], "phase": "claim-only"}])
            with self.assertRaises(ValueError):
                journal.unresolved(root, str(uuid.uuid4()))

    def test_worker_gate_refuses_symlinked_or_incomplete_run_directories(self):
        for invalid in ["symlink", "incomplete", "unknown-entry"]:
            with self.subTest(invalid=invalid), tempfile.TemporaryDirectory() as root:
                run, worker = str(uuid.uuid4()), str(uuid.uuid4())
                path = Path(root) / run
                if invalid == "symlink":
                    path.symlink_to(root, target_is_directory=True)
                elif invalid == "incomplete":
                    path.mkdir(mode=0o700)
                else:
                    (Path(root) / "unknown").mkdir(mode=0o700)
                with self.assertRaises((ValueError, OSError)):
                    journal.unresolved(root, worker)

    def test_portable_outputs_are_bounded_selected_and_collision_free(self):
        frozen = fixture.frozen()
        for files in [{"unselected": fixture.file()}, {"out/missing": None}, {"../escape": fixture.file()},
                      {"out/.codex/hooks": fixture.file()}, {"out": fixture.file()},
                      {"out/x": fixture.file(), "out/x/child": fixture.file()},
                      {"docs/知识.md/child": fixture.file()},
                      {"out/bad": dict(fixture.file(), sha256="0" * 64)},
                      {"out/" + str(n): fixture.file() for n in range(101)},
                      {"out/" + str(n): fixture.file(b"x" * 1048576) for n in range(3)}]:
            with self.subTest(paths=list(files)[:3]), self.assertRaises(ValueError):
                outputs.validate(frozen, fixture.CONFIG, {"files": files, "summary": "", "exit-code": 0})
        # A removed selected parent can become a directory in the final tree.
        frozen["files"]["out"] = fixture.file()
        valid = {"files": {"out": None, "out/new": fixture.file()}, "summary": "", "exit-code": 0}
        self.assertEqual(outputs.validate(frozen, fixture.CONFIG, valid), valid)
        for invalid in [{"files": {}, "summary": "x" * 16001, "exit-code": 0},
                        {"files": {}, "summary": "", "exit-code": True},
                        {"files": {}, "summary": "", "exit-code": 256},
                        {"files": {}, "summary": "", "exit-code": 0, "unknown": True}]:
            with self.assertRaises(ValueError):
                outputs.validate(frozen, fixture.CONFIG, invalid)


@unittest.skipUnless(os.environ.get("SIMMIS_WORKER_NAMESPACE_TESTS") == "1",
                     "Explicit host integration: requires dbus/gi, user systemd and bubblewrap")
class JournalCrashIntegrationTest(unittest.TestCase):
    def test_abrupt_controller_death_preserves_admission_and_requires_exact_stop(self):
        with tempfile.TemporaryDirectory(prefix="simmis-journal-crash-") as root:
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            runs = Path(root) / "runs"
            runs.mkdir(mode=0o700)
            folder = runs / run
            folder.mkdir(mode=0o700)
            # These fresh directory entries are part of the fixture's durable
            # state ownership prerequisite, not created by the journal itself.
            for location in [folder, runs, Path(root), Path(root).parent]:
                fd = os.open(location, os.O_RDONLY | os.O_DIRECTORY)
                try:
                    os.fsync(fd)
                finally:
                    os.close(fd)
            frozen = fixture.frozen()
            frozen["limits"]["timeout-ms"] = 10000
            options = {"root": root, "folder": str(folder), "worker": worker, "run": run,
                       "endpoint": ENDPOINT, "frozen": frozen, "config": fixture.CONFIG}
            command = [sys.executable, "-I", str(Path(__file__).with_name("worker_controller_crash_fixture.py"))]
            process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            name = None
            try:
                process.stdin.write(json.dumps(options).encode() + b"\n")
                process.stdin.flush()
                with selectors.DefaultSelector() as ready:
                    ready.register(process.stdout, selectors.EVENT_READ)
                    self.assertTrue(ready.select(timeout=25), "Controller fixture did not report GO")
                message = json.loads(process.stdout.readline(4097))
                self.assertEqual(message["kind"], "go-sent")
                name = message["unit"]
                with self.assertRaises(ValueError):
                    with claim.controller_lock(root, worker):
                        self.fail("Second controller acquired the live worker lock")
                process.kill()
                self.assertEqual(process.wait(timeout=5), -9)
                with claim.controller_lock(root, worker):
                    chain = journal.load(folder)
                    self.assertEqual(chain["launch"]["unit"], name)
                    self.assertIsNone(chain["stopped"])
                    self.assertEqual(journal.unresolved(runs, worker), [{"run": run, "phase": "admitted"}])
                    observer = journal.unit.UnitBus()
                    identity = chain["admitted"]["identity"]
                    observer.retain(identity)
                    with self.assertRaises(ValueError):
                        observer.capture(name)  # recovery never gives GO
                    observer.stop(identity)
                    observation = observer.wait_stopped(identity)
                    self.assertIsNone(journal.load(folder)["stopped"])
                    stopped = journal.stopped(folder, observation)
                    self.assertIsNone(stopped["output"])  # never collect/replay lost candidate output
                    observer.release(identity)  # only AFTER durable stop phase
                    observer.close()
                    self.assertEqual(journal.load(folder)["stopped"], stopped)
                    # No server was contacted: physical stop alone must still
                    # leave the worker-wide remote-reconciliation gate held.
                    self.assertEqual(journal.unresolved(runs, worker), [{"run": run, "phase": "stopped"}])
            finally:
                if process.poll() is None:
                    process.kill()
                    process.wait(timeout=5)
                process.stdin.close()
                process.stdout.close()
                process.stderr.close()
                if name is None:
                    chain = journal.load(folder)
                    name = chain["launch"]["unit"] if chain["launch"] else None
                if name:
                    subprocess.run(["systemctl", "--user", "stop", name], timeout=10,
                                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                    subprocess.run(["systemctl", "--user", "reset-failed", name], timeout=10,
                                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
