"""Explicit packaged-runtime check; no login, inference or native task calls."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
import uuid

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import worker_runtime as runtime


@unittest.skipUnless(os.environ.get("SIMMIS_WORKER_JAR"), "Requires explicitly built worker artifact")
class PackagedRuntimeTest(unittest.TestCase):
    def test_wire_loading_does_not_initialize_guest_or_database(self):
        jar = Path(os.environ["SIMMIS_WORKER_JAR"]).resolve(strict=True)
        descriptor = {"jar": str(jar), "sha256": hashlib.sha256(jar.read_bytes()).hexdigest()}
        with tempfile.TemporaryDirectory(prefix="simmis-worker-wire-smoke-") as home:
            # This trusted packaging test, not the production driver, evaluates
            # a fixed expression in a fresh credential-free JVM. In-process
            # tests cannot detect accidental dependencies already loaded there.
            expression = """(do
              (require 'is.simm.ops.worker-wire)
              (doseq [n '[is.simm.ops.worker-files datahike.api sci.core
                          dvergr.rooms.files org.replikativ.spindel.engine.impl.simple]]
                (assert (nil? (find-ns n)) (str "Unexpected wire dependency: " n)))
              (println "wire-ready") (shutdown-agents))"""
            argv = runtime.argv(descriptor, "check", home)
            self.assertEqual(["-m", runtime.ENTRYPOINT, "check"], argv[-3:])
            result = subprocess.run(argv[:-3] + ["-e", expression],
                                    cwd=home, env=runtime.environment(home),
                                    stdin=subprocess.DEVNULL, capture_output=True, timeout=30)
            self.assertEqual(0, result.returncode, result.stderr[-2048:].decode("utf-8", errors="replace"))
            self.assertEqual(b"wire-ready\n", result.stdout)

    def test_cold_preflight_loads_real_endpoints_without_login_or_dev_startup(self):
        jar = Path(os.environ["SIMMIS_WORKER_JAR"]).resolve(strict=True)
        descriptor = {"jar": str(jar), "sha256": hashlib.sha256(jar.read_bytes()).hexdigest()}
        with tempfile.TemporaryDirectory(prefix="simmis-worker-smoke-") as home:
            # Deliberately run outside the checkout. Neither project deps nor
            # dev/user.clj nor caller environment can participate in startup.
            result = subprocess.run(runtime.argv(descriptor, "check", home),
                                    cwd=home, env=runtime.environment(home),
                                    stdin=subprocess.DEVNULL, capture_output=True, timeout=120)
            # Check mode emits no task/provider data, only bounded startup
            # diagnostics. Make clean-CI failures actionable without native
            # auth or a privileged arbitrary-eval debug escape in the runner.
            self.assertEqual(0, result.returncode,
                             "Packaged runtime preflight failed: " + result.stderr[-2048:].decode("utf-8", errors="replace"))
            self.assertLess(len(result.stdout), 4096)
            value = json.loads(result.stdout)
            self.assertEqual({"version", "kind", "executor-policy-id", "inference-policy-id"}, set(value))
            self.assertEqual(1, value["version"])
            self.assertEqual("runtime-ready", value["kind"])
            for field in ["executor-policy-id", "inference-policy-id"]:
                self.assertEqual(value[field], str(uuid.UUID(value[field])))
            self.assertFalse((Path(home) / "codex" / "auth.json").exists())
