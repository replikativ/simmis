import importlib.util
from pathlib import Path
import unittest


spec = importlib.util.spec_from_file_location(
    "ci_ubuntu_mirror", Path(__file__).resolve().parents[2] / "scripts/ci_ubuntu_mirror.py")
mirror = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mirror)


class UbuntuMirrorTests(unittest.TestCase):
    def test_exact_uri_preserves_suite_components_and_signing_key(self):
        source = ("Types: deb\nURIs: http://azure.archive.ubuntu.com/ubuntu/\n"
                  "Suites: noble noble-updates noble-backports\nComponents: main universe\n"
                  "Architectures: amd64\nSigned-By: /usr/share/keyrings/ubuntu-archive-keyring.gpg\n")
        self.assertEqual(source.replace("http://azure.archive.ubuntu.com/ubuntu/",
                                        "https://archive.ubuntu.com/ubuntu"),
                         mirror.canonical_archive(source))

    def test_second_run_is_identical(self):
        first = mirror.canonical_archive("URIs: http://azure.archive.ubuntu.com/ubuntu\n")
        self.assertEqual(first, mirror.canonical_archive(first))

    def test_security_https_and_other_repositories_are_unchanged(self):
        source = ("URIs: http://security.ubuntu.com/ubuntu/\n"
                  "URIs: https://azure.archive.ubuntu.com/ubuntu\n"
                  "URIs: https://example.org/ubuntu\n")
        self.assertEqual(source, mirror.canonical_archive(source))

    def test_only_complete_tokens_in_uri_fields_are_changed(self):
        source = ("# URIs: http://azure.archive.ubuntu.com/ubuntu\n"
                  "Signed-By: http://azure.archive.ubuntu.com/ubuntu\n"
                  "URIs: http://azure.archive.ubuntu.com/ubuntu.evil "
                  "http://azure.archive.ubuntu.com/ubuntu/other\n")
        self.assertEqual(source, mirror.canonical_archive(source))

    def test_multiple_uris_and_horizontal_whitespace_are_preserved(self):
        source = "URIs:\t http://azure.archive.ubuntu.com/ubuntu  https://example.org/ubuntu\n"
        self.assertEqual("URIs:\t https://archive.ubuntu.com/ubuntu  https://example.org/ubuntu\n",
                         mirror.canonical_archive(source))

    def test_crlf_and_unfamiliar_layout_are_preserved(self):
        source = "Types: deb\r\nURIs: http://azure.archive.ubuntu.com/ubuntu\r\nSuites: noble\r\n"
        self.assertEqual(source.replace("http://azure.archive.ubuntu.com/ubuntu",
                                        "https://archive.ubuntu.com/ubuntu"),
                         mirror.canonical_archive(source))
        other = "deb http://azure.archive.ubuntu.com/ubuntu noble main\n"
        self.assertEqual(other, mirror.canonical_archive(other))

    def test_runner_mirror_list_is_replaced_only_in_uri_fields(self):
        source = ("Types: deb\r\nURIs: mirror+file:/etc/apt/apt-mirrors.txt\r\n"
                  "Suites: noble noble-updates noble-backports\r\n"
                  "Signed-By: /usr/share/keyrings/ubuntu-archive-keyring.gpg\r\n"
                  "\r\nURIs: https://security.ubuntu.com/ubuntu/\r\nSuites: noble-security\r\n")
        expected = source.replace("mirror+file:/etc/apt/apt-mirrors.txt",
                                  "https://archive.ubuntu.com/ubuntu")
        self.assertEqual(expected, mirror.canonical_archive(source))
        self.assertEqual(expected, mirror.canonical_archive(expected))

    def test_other_local_lists_and_partial_paths_are_unchanged(self):
        source = ("URIs: mirror+file:/etc/apt/other-mirrors.txt\n"
                  "URIs: mirror+file:/etc/apt/apt-mirrors.txt.other\n"
                  "Signed-By: mirror+file:/etc/apt/apt-mirrors.txt\n"
                  "# URIs: mirror+file:/etc/apt/apt-mirrors.txt\n")
        self.assertEqual(source, mirror.canonical_archive(source))
