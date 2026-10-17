from concurrent.futures import ThreadPoolExecutor
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


class ClaimTest(unittest.TestCase):
    def test_identity_is_private_durable_and_exactly_reused_after_client_restart(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "claim.json"
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            first = claim.prepare(path, worker, run)
            self.assertEqual(first, claim.load(path))
            self.assertEqual(first, claim.prepare(path, worker, run))
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
            self.assertEqual(len(first["proof"]), 43)
            self.assertEqual(claim.lease_token(first),
                             "simmis-lease." + run + "." + first["nonce"] + "." + first["proof"])
            self.assertEqual(list(Path(folder).iterdir()), [path])

    def test_concurrent_preparation_never_rotates_the_winning_identity(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "claim.json"
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            with ThreadPoolExecutor(max_workers=8) as pool:
                tickets = list(pool.map(lambda _: claim.prepare(path, worker, run), range(24)))
            self.assertTrue(all(ticket == tickets[0] for ticket in tickets))
            self.assertEqual(list(Path(folder).iterdir()), [path])

    def test_restrictive_umask_still_publishes_a_reloadable_private_ticket(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "claim.json"
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            previous = os.umask(0o777)
            try:
                ticket = claim.prepare(path, worker, run)
            finally:
                os.umask(previous)
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
            self.assertEqual(claim.load(path), ticket)
            self.assertEqual(claim.prepare(path, worker, run), ticket)

    def test_different_run_race_refuses_the_loser_without_overwriting(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "claim.json"
            worker = str(uuid.uuid4())
            runs = [str(uuid.uuid4()), str(uuid.uuid4())]

            def prepare(run):
                try:
                    return claim.prepare(path, worker, run)
                except ValueError:
                    return None

            with ThreadPoolExecutor(max_workers=2) as pool:
                tickets = list(pool.map(prepare, runs))
            winners = [ticket for ticket in tickets if ticket is not None]
            self.assertEqual(len(winners), 1)
            self.assertEqual(claim.load(path), winners[0])
            self.assertEqual(list(Path(folder).iterdir()), [path])

    def test_crash_left_staging_links_do_not_rotate_the_published_ticket(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "claim.json"
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            ticket = claim.prepare(path, worker, run)
            residue = Path(folder) / ".claim-crash-residue"
            os.link(path, residue)
            unrelated = Path(folder) / ".claim-another-crash"
            unrelated.write_bytes(b"unpublished and non-authoritative")
            unrelated.chmod(0o600)
            self.assertEqual(claim.prepare(path, worker, run), ticket)
            self.assertEqual(claim.load(path), ticket)
            self.assertEqual(residue.read_bytes(), path.read_bytes())
            self.assertEqual(unrelated.read_bytes(), b"unpublished and non-authoritative")

    def test_existing_other_work_corrupt_or_public_tickets_are_never_replaced(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "claim.json"
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            original = claim.prepare(path, worker, run)
            for wrong_worker, wrong_run in [(str(uuid.uuid4()), run), (worker, str(uuid.uuid4()))]:
                with self.assertRaises(ValueError):
                    claim.prepare(path, wrong_worker, wrong_run)
                self.assertEqual(claim.load(path), original)
            for encoded in ["not-json", "{\"version\":1,\"version\":2}", "x" * (claim.MAX_TICKET_BYTES + 1)]:
                path.write_text(encoded)
                with self.assertRaises(ValueError):
                    claim.prepare(path, worker, run)
                self.assertEqual(path.read_text(), encoded)
            path.write_text(json.dumps(original))
            path.chmod(0o644)
            with self.assertRaises(ValueError):
                claim.prepare(path, worker, run)

    def test_symlinked_paths_and_unprivate_state_directories_are_refused(self):
        with tempfile.TemporaryDirectory() as parent:
            private = Path(parent) / "private"
            private.mkdir(mode=0o700)
            target = private / "original.json"
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            original = claim.prepare(target, worker, run)
            alias = private / "alias.json"
            alias.symlink_to(target)
            with self.assertRaises(ValueError):
                claim.prepare(alias, worker, run)
            link = Path(parent) / "linked-directory"
            link.symlink_to(private, target_is_directory=True)
            with self.assertRaises(OSError):
                claim.prepare(link / "new.json", worker, run)
            private.chmod(0o755)
            with self.assertRaises(ValueError):
                claim.prepare(private / "new.json", worker, run)
            private.chmod(0o700)
            self.assertEqual(claim.load(target), original)

    def test_failed_file_or_directory_fsync_never_returns_a_new_identity(self):
        for fail_on in [1, 2]:
            with self.subTest(fail_on=fail_on), tempfile.TemporaryDirectory() as folder:
                path = Path(folder) / "claim.json"
                worker, run = str(uuid.uuid4()), str(uuid.uuid4())
                sync, calls = os.fsync, []

                def fsync(fd):
                    calls.append(fd)
                    if len(calls) == fail_on:
                        raise OSError("Synthetic durable-write failure")
                    return sync(fd)

                with patch.object(claim.os, "fsync", fsync), self.assertRaises(OSError):
                    claim.prepare(path, worker, run)
                existing = claim.load(path) if path.exists() else None
                recovered = claim.prepare(path, worker, run)
                if existing:
                    self.assertEqual(existing, recovered)
                self.assertEqual(list(Path(folder).iterdir()), [path])

    def test_controller_lock_is_exclusive_and_separate_enrollments_do_not_entangle(self):
        with tempfile.TemporaryDirectory() as folder:
            first, second = str(uuid.uuid4()), str(uuid.uuid4())
            with claim.controller_lock(folder, first):
                with self.assertRaises(ValueError):
                    with claim.controller_lock(folder, first):
                        self.fail("Second controller acquired the same physical slot")
                with claim.controller_lock(folder, second):
                    pass
            with claim.controller_lock(folder, first):
                pass

    def test_controller_death_releases_os_lock_but_preserves_claim_identity(self):
        with tempfile.TemporaryDirectory() as folder:
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            path = Path(folder) / "claim.json"
            ticket = claim.prepare(path, worker, run)
            script = ("import sys; sys.path.insert(0, sys.argv[1]); import worker_claim; "
                      "lock=worker_claim.controller_lock(sys.argv[2], sys.argv[3]); "
                      "lock.__enter__(); print('locked', flush=True); sys.stdin.read()")
            process = subprocess.Popen([sys.executable, "-c", script,
                                        str(Path(claim.__file__).parent), folder, worker],
                                       stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                       stderr=subprocess.DEVNULL)
            try:
                with selectors.DefaultSelector() as ready:
                    ready.register(process.stdout, selectors.EVENT_READ)
                    self.assertTrue(ready.select(timeout=10), "Controller failed to signal ownership")
                self.assertEqual(process.stdout.readline(), b"locked\n")
                with self.assertRaises(ValueError):
                    with claim.controller_lock(folder, worker):
                        self.fail("Concurrent process acquired a live controller lock")
                process.kill()
                process.wait(timeout=10)
                with claim.controller_lock(folder, worker):
                    self.assertEqual(claim.prepare(path, worker, run), ticket)
            finally:
                if process.poll() is None:
                    process.kill()
                    process.wait(timeout=10)
                process.stdin.close()
                process.stdout.close()

    def test_fifo_claim_and_controller_lock_paths_are_refused(self):
        with tempfile.TemporaryDirectory() as folder:
            worker, run = str(uuid.uuid4()), str(uuid.uuid4())
            path = Path(folder) / "claim.json"
            os.mkfifo(path, 0o600)
            with self.assertRaises(ValueError):
                claim.prepare(path, worker, run)
            os.mkfifo(Path(folder) / (worker + ".lock"), 0o600)
            with self.assertRaises(ValueError):
                with claim.controller_lock(folder, worker):
                    self.fail("Special-file controller lock was accepted")


if __name__ == "__main__":
    unittest.main()
