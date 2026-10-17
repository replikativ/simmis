import hashlib
import os
from pathlib import Path
import sys
import tempfile
import unittest
import zipfile

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import worker_runtime as runtime


class RuntimeTest(unittest.TestCase):
    def artifact(self, folder, additions=None):
        path = Path(folder) / "worker.jar"
        with zipfile.ZipFile(path, "w") as jar:
            for name in sorted(runtime.REQUIRED):
                jar.writestr(name, b"fixture, not executable")
            for name, value in (additions or {}).items():
                jar.writestr(name, value)
        path.chmod(0o600)
        return {"jar": str(path), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}

    def test_runtime_is_pinned_local_and_closed(self):
        with tempfile.TemporaryDirectory() as root:
            descriptor = self.artifact(root)
            self.assertEqual(runtime.verify(descriptor), descriptor)
            for bad in [dict(descriptor, sha256="0" * 64), dict(descriptor, extra=True),
                        dict(descriptor, jar="worker.jar"), dict(descriptor, sha256=True)]:
                with self.subTest(bad=bad), self.assertRaises(ValueError):
                    runtime.verify(bad)
            Path(descriptor["jar"]).chmod(0o666)
            with self.assertRaises(ValueError):
                runtime.verify(descriptor)

    def test_ambient_startup_and_classpath_are_rejected(self):
        with tempfile.TemporaryDirectory() as root:
            for name in ["user.clj", "user.cljc", "user__init.class"]:
                with self.subTest(name=name), self.assertRaises(ValueError):
                    runtime.verify(self.artifact(root, {name: b"no"}))
            for field in ["Class-Path: /tmp/dev\n", "cLaSs-PaTh: bad\n"]:
                with self.assertRaises(ValueError):
                    runtime.verify(self.artifact(root, {"META-INF/MANIFEST.MF": field}))
            descriptor = self.artifact(root)
            alias = Path(root) / "alias.jar"
            alias.symlink_to(descriptor["jar"])
            with self.assertRaises(ValueError):
                runtime.verify(dict(descriptor, jar=str(alias)))

    def test_fixed_bootstrap_and_clean_guest_environment(self):
        with tempfile.TemporaryDirectory() as root:
            descriptor = self.artifact(root)
            command = runtime.argv(descriptor, "guest", root)
            self.assertEqual(command[-4:], ["clojure.main", "-m", runtime.ENTRYPOINT, "guest"])
            self.assertEqual(command[command.index("-cp") + 1], descriptor["jar"])
            self.assertIn("-Duser.home=" + root, command)
            with self.assertRaises(ValueError):
                runtime.argv(descriptor, "shell", root)
            env = runtime.environment(root)
            self.assertEqual(env["CODEX_HOME"], root + "/codex")
            self.assertNotIn("OPENAI_API_KEY", env)
            self.assertNotIn("JAVA_TOOL_OPTIONS", env)
            self.assertEqual(runtime.environment(root, codex_home=root)["CODEX_HOME"], root)
