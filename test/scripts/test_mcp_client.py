import importlib.util
import json
import os
from pathlib import Path
import tempfile
import subprocess
import sys
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import unittest
from unittest.mock import patch
import urllib.error

spec = importlib.util.spec_from_file_location(
    "mcp_client", Path(__file__).resolve().parents[2] / "scripts/mcp_client.py")
client = importlib.util.module_from_spec(spec)
spec.loader.exec_module(client)


class ClientTest(unittest.TestCase):
    def test_actual_headers_cli_renews_and_keeps_secrets_out_of_diagnostics(self):
        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *args):
                pass

            def do_POST(self):
                body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
                if self.path == "/auth/refresh" and body == {"refresh_token": "refresh-secret"}:
                    status, result = 200, {"access_token": "new-web-secret"}
                elif (self.path == "/auth/mcp-token" and
                      self.headers.get("Authorization") == "Bearer new-web-secret"):
                    status, result = 200, {"token": "read-mcp-secret"}
                else:
                    status, result = 401, {"error": "expired"}
                self.send_response(status)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(json.dumps(result).encode())

        with ThreadingHTTPServer(("127.0.0.1", 0), Handler) as server:
            thread = threading.Thread(target=server.serve_forever, daemon=True)
            thread.start()
            try:
                with tempfile.TemporaryDirectory() as directory:
                    path = Path(directory) / "session.json"
                    client.save(path, {"origin": "http://127.0.0.1:" + str(server.server_port),
                                       "access_token": "expired", "refresh_token": "refresh-secret"})
                    result = subprocess.run([sys.executable, str(Path(client.__file__)),
                                             "headers", "--session", str(path)],
                                            capture_output=True, text=True, timeout=10)
                    self.assertEqual(0, result.returncode, result.stderr)
                    self.assertEqual({"Authorization": "Bearer read-mcp-secret"},
                                     json.loads(result.stdout))
                    self.assertEqual("", result.stderr)
                    self.assertEqual("new-web-secret", json.loads(path.read_text())["access_token"])
                    self.assertEqual(0o600, path.stat().st_mode & 0o777)
            finally:
                server.shutdown()
                thread.join()

    def test_origin_rejects_insecure_and_ambiguous_destinations(self):
        for url in ("http://dev.simm.is", "https://user:secret@dev.simm.is",
                    "https://dev.simm.is/path", "https://dev.simm.is?token=x",
                    "https://dev.simm.is#x", "file:///tmp/token"):
            with self.subTest(url=url), self.assertRaises(ValueError):
                client.origin(url)
        self.assertEqual("https://dev.simm.is", client.origin("https://dev.simm.is/"))
        self.assertEqual("http://127.0.0.1:8080", client.origin("http://127.0.0.1:8080"))

    def test_redirects_never_forward_credentials(self):
        with self.assertRaises(ValueError):
            client.NoRedirects().redirect_request(None, None, 307, "", {}, "https://other.org")

    def test_private_storage_and_unsafe_file_refusal(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "session.json"
            client.save(path, {"secret": "test"})
            self.assertEqual(0o600, path.stat().st_mode & 0o777)
            os.close(client.private_file(path, os.O_RDONLY))
            path.chmod(0o644)
            with self.assertRaises(ValueError):
                client.private_file(path, os.O_RDONLY)
            link = Path(directory) / "link"
            link.symlink_to(path)
            with self.assertRaises(OSError):
                client.private_file(link, os.O_RDONLY)

    def test_refresh_rotation_is_saved_before_retry(self):
        old = {"origin": "https://dev.simm.is", "access_token": "old", "refresh_token": "r1"}
        events = []

        def post(base, route, body=None, token=None):
            events.append((route, token))
            if route == "/auth/refresh":
                self.assertEqual({"refresh_token": "r1"}, body)
                return {"access_token": "new", "refresh_token": "r2"}
            if token == "old":
                raise urllib.error.HTTPError("", 401, "expired", {}, None)
            return {"token": "read-only"}

        def save(path, session):
            events.append(("saved", session["refresh_token"]))

        with patch.object(client, "post", post), patch.object(client, "save", save):
            self.assertEqual("read-only", client.exchange(None, old)["token"])
        self.assertEqual([("/auth/mcp-token", "old"), ("/auth/refresh", None),
                          ("saved", "r2"), ("/auth/mcp-token", "new")], events)

    def test_non_auth_failure_does_not_rotate_login(self):
        with patch.object(client, "post", side_effect=urllib.error.HTTPError(
                "", 503, "unavailable", {}, None)) as request:
            with self.assertRaises(urllib.error.HTTPError):
                client.exchange(None, {"origin": "https://dev.simm.is", "access_token": "old"})
            self.assertEqual(1, request.call_count)

    def test_non_rotating_kabel_refresh_keeps_refresh_token(self):
        session = {"origin": "https://dev.simm.is", "access_token": "old", "refresh_token": "r1"}
        with patch.object(client, "post", side_effect=[
                urllib.error.HTTPError("", 401, "expired", {}, None),
                {"access_token": "new"}, {"token": "mcp"}]), patch.object(client, "save"):
            self.assertEqual("mcp", client.exchange(None, session)["token"])
        self.assertEqual("r1", session["refresh_token"])


if __name__ == "__main__":
    unittest.main()
