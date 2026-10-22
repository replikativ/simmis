"""Exercise CI setup without sudo, networking or package changes."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts" / "install-test-helpers.sh"


class InstallTestHelpers(unittest.TestCase):
    def run_setup(self, present, apt_failure=False, broken=None):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for name in present:
                if name == broken:
                    (root / name).write_text("#!/bin/bash\nexit 17\n", encoding="utf-8")
                    (root / name).chmod(0o755)
                else:
                    (root / name).symlink_to("/bin/true")
            sudo = root / "sudo"
            sudo.write_text(
                '#!/bin/bash\n'
                'printf "%s\\n" "$*" >> "$CI_TEST_LOG"\n'
                'if [[ "${CI_APT_FAILURE:-}" == yes ]]; then exit 42; fi\n'
                'if [[ " $* " == *" install "* ]]; then\n'
                '  for helper in ffmpeg python3; do\n'
                '    if [[ " $* " == *" $helper "* ]]; then\n'
                '      /bin/ln -s /bin/true "$CI_TEST_BIN/$helper"\n'
                '    fi\n'
                '  done\n'
                'fi\n', encoding="utf-8")
            sudo.chmod(0o755)
            log = root / "apt.log"
            environment = dict(os.environ, PATH=str(root), CI_TEST_LOG=str(log),
                               CI_TEST_BIN=str(root), CI_APT_FAILURE="yes" if apt_failure else "no")
            result = subprocess.run(["/bin/bash", str(SCRIPT)], env=environment,
                                    capture_output=True, text=True, timeout=10)
            return result, log.read_text() if log.exists() else ""

    def test_existing_helpers_need_no_package_network(self):
        result, log = self.run_setup(["ffmpeg", "python3"])
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual("", log)

    def test_install_only_missing_helpers_with_bounded_requests(self):
        for present, missing in [([], ["ffmpeg", "python3"]),
                                 (["ffmpeg"], ["python3"]),
                                 (["python3"], ["ffmpeg"])]:
            with self.subTest(present=present):
                result, log = self.run_setup(present)
                self.assertEqual(0, result.returncode, result.stderr)
                update, install = log.splitlines()
                self.assertTrue(update.endswith(" update"))
                self.assertTrue(install.endswith(" install -y --no-install-recommends " + " ".join(missing)))
                for command in (update, install):
                    self.assertIn("Acquire::Retries=2", command)
                    self.assertIn("Acquire::http::Timeout=30", command)
                    self.assertIn("Acquire::https::Timeout=30", command)

    def test_package_failure_is_not_hidden_or_retried_as_a_pass(self):
        result, log = self.run_setup([], apt_failure=True)
        self.assertEqual(42, result.returncode)
        self.assertEqual(1, len(log.splitlines()))

    def test_existing_but_broken_helper_fails_without_package_network(self):
        result, log = self.run_setup(["ffmpeg", "python3"], broken="ffmpeg")
        self.assertEqual(17, result.returncode)
        self.assertEqual("", log)


if __name__ == "__main__":
    unittest.main()
