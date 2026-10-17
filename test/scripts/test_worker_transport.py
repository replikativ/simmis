from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import select
import selectors
import subprocess
import sys
import threading
import time
import unittest
from unittest.mock import Mock

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import worker_transport as transport


class TransportTest(unittest.TestCase):
    def setUp(self):
        self.dribble = False
        self.hold_final_byte = False
        self.release = threading.Event()
        self.call_seen = threading.Event()
        self.requests = []
        owner = self

        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *_args):
                pass

            def do_POST(self):
                request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
                owner.requests.append((dict(self.headers), request))
                if request["method"].startswith("notifications/"):
                    self.send_response(204)
                    self.end_headers()
                    return
                result = {} if request["method"] == "initialize" else {"structuredContent": {"result": {"received": True}}}
                if request["method"] == "tools/call":
                    owner.call_seen.set()
                data = json.dumps({"jsonrpc": "2.0", "id": request["id"], "result": result}).encode()
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.send_header("Mcp-Session-Id", "synthetic-worker-session")
                self.send_header("Content-Length", str(len(data)))
                self.end_headers()
                try:
                    if owner.dribble and request["method"] == "tools/call":
                        for index, byte in enumerate(data):
                            if owner.hold_final_byte and index == len(data) - 1:
                                owner.release.wait(timeout=10)
                            self.wfile.write(bytes([byte]))
                            self.wfile.flush()
                            # Intentional network-fault fixture: bytes arrive
                            # frequently enough to defeat inactivity timeouts.
                            if owner.release.wait(timeout=0.02):
                                return
                    else:
                        self.wfile.write(data)
                except (BrokenPipeError, ConnectionResetError):
                    pass

            def do_DELETE(self):
                self.send_response(204)
                self.end_headers()

        self.server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.url = "http://127.0.0.1:" + str(self.server.server_port) + "/mcp"

    def tearDown(self):
        self.release.set()
        self.server.shutdown()
        self.server.server_close()
        self.thread.join()

    def test_real_worker_requests_keep_credentials_out_of_argv_and_environment(self):
        token, proof = "synthetic-transport-private-token", "A" * 43
        with transport.MCP(self.url, token, lease_proof=proof) as client:
            self.assertEqual(client.call("simmis_worker-heartbeat", {"run": "synthetic"}), {"received": True})
            self.assertNotIn(token, repr(client.process.args))
            self.assertNotIn(proof, repr(client.process.args))
            environment = Path("/proc", str(client.process.pid), "environ").read_bytes()
            self.assertNotIn(token.encode(), environment)
            with self.assertRaises(ValueError):
                client.call("clojure_eval", {})
        self.assertIsNotNone(client.process.returncode)
        for headers, body in self.requests:
            self.assertEqual(headers["Authorization"], "Bearer " + token)
            self.assertEqual(headers["Simmis-Lease-Proof"], proof)
            self.assertNotIn(token, json.dumps(body))
            self.assertNotIn(proof, json.dumps(body))

    def test_total_deadline_stops_dribbling_http_and_joins_the_helper(self):
        failure = Mock()
        with transport.MCP(self.url, "synthetic-token", on_failure=failure) as client:
            self.dribble = True
            client.timeout = 0.15  # after startup, deterministic fault deadline
            started = time.monotonic()
            with self.assertRaises(TimeoutError):
                client.call("simmis_worker-heartbeat", {"run": "synthetic"})
            self.assertLess(time.monotonic() - started, 2)
            self.assertIsNotNone(client.process.returncode)
            self.assertIsNone(client.frames)
            failure.assert_called_once()

    def test_parent_death_kills_even_a_stalled_http_helper(self):
        self.dribble = True
        self.hold_final_byte = True
        script = ("import sys;sys.path.insert(0,sys.argv[1]);from worker_transport import MCP;"
                  "client=MCP(sys.argv[2],'synthetic-token');client.__enter__();"
                  "print(client.process.pid,flush=True);client.call('simmis_worker-heartbeat',{'run':'synthetic'})")
        parent = subprocess.Popen([sys.executable, "-I", "-c", script, str(Path(transport.__file__).parent), self.url],
                                  stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        descriptor = None
        try:
            with selectors.DefaultSelector() as ready:
                ready.register(parent.stdout, selectors.EVENT_READ)
                self.assertTrue(ready.select(timeout=10))
            helper = int(parent.stdout.readline(64))
            descriptor = os.pidfd_open(helper)
            self.assertTrue(self.call_seen.wait(timeout=5), "Fixture helper never entered the blocked HTTP request")
            parent.kill()
            self.assertEqual(parent.wait(timeout=5), -9)
            self.assertTrue(select.select([descriptor], [], [], 2)[0], "Blocked HTTP helper survived its owning controller")
        finally:
            if descriptor is not None:
                os.close(descriptor)
            if parent.poll() is None:
                parent.kill()
                parent.wait(timeout=5)
            parent.stdin.close()
            parent.stdout.close()
