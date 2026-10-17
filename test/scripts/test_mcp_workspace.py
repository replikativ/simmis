import base64
import hashlib
import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest
import uuid

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
spec = importlib.util.spec_from_file_location("mcp_workspace", Path(__file__).resolve().parents[2] / "scripts/mcp_workspace.py")
workspace = importlib.util.module_from_spec(spec)
spec.loader.exec_module(workspace)


class WorkspaceTest(unittest.TestCase):
    def fixture(self, change=None):
        data = b"synthetic bundle bytes\x00\xff" * 1000
        descriptor = {"snapshot": str(uuid.uuid4()), "bytes": len(data), "files": 1,
                      "history": False, "commit": "a" * 40, "sha256": hashlib.sha256(data).hexdigest()}
        calls = []

        def call(name, args):
            calls.append((name, args))
            if name == "simmis_workspace-bundle":
                return descriptor.copy()
            self.assertEqual(args["expected-snapshot"], descriptor["snapshot"])
            offset = args["offset"]
            end = min(len(data), offset + workspace.CHUNK)
            part = {**descriptor, "offset": offset,
                    "base64": base64.b64encode(data[offset:end]).decode()}
            if end < len(data):
                part["next-offset"] = end
            if change:
                change(part)
            return part
        return data, call, calls

    def test_verified_multiple_chunks_publish_private_destination(self):
        data, call, calls = self.fixture()
        with tempfile.TemporaryDirectory() as root:
            output = Path(root) / "room.bundle"
            result = workspace.download(call, "my-room", output)
            self.assertEqual(output.read_bytes(), data)
            self.assertEqual(output.stat().st_mode & 0o777, 0o600)
            self.assertEqual(result["bytes"], len(data))
            self.assertEqual(len(calls), 3)
            self.assertEqual(list(Path(root).iterdir()), [output])

    def test_conflicts_corruption_offsets_and_sizes_never_publish(self):
        changes = [lambda p: p.update(snapshot=str(uuid.uuid4())),
                   lambda p: p.update(offset=p["offset"] + 1),
                   lambda p: p.update(**{"next-offset": 1}),
                   lambda p: p.update(base64="not-base64"),
                   lambda p: p.update(base64=base64.b64encode(b"x" * (workspace.CHUNK + 1)).decode()),
                   lambda p: p.update(base64=""),
                   lambda p: p.update(base64=base64.b64encode(b"x" * len(base64.b64decode(p["base64"]))).decode())]
        for change in changes:
            with self.subTest(change=change), tempfile.TemporaryDirectory() as root:
                _, call, _ = self.fixture(change)
                with self.assertRaises(ValueError):
                    workspace.download(call, "my-room", Path(root) / "room.bundle")
                self.assertEqual(list(Path(root).iterdir()), [])

    def test_existing_destination_and_symlink_are_never_overwritten(self):
        _, call, _ = self.fixture()
        with tempfile.TemporaryDirectory() as root:
            output = Path(root) / "room.bundle"
            output.write_bytes(b"keep")
            with self.assertRaises(FileExistsError):
                workspace.download(call, "my-room", output)
            self.assertEqual(output.read_bytes(), b"keep")
            link = Path(root) / "link.bundle"
            link.symlink_to(output)
            with self.assertRaises(FileExistsError):
                workspace.download(call, "my-room", link)
            self.assertTrue(link.is_symlink())
            self.assertEqual(output.read_bytes(), b"keep")
            self.assertEqual(set(Path(root).iterdir()), {output, link})

    def test_descriptor_bounds_refused_before_creating_a_file(self):
        for extra in [{"bytes": workspace.MAX_BUNDLE + 1}, {"bytes": True}, {"history": True},
                      {"files": 513}, {"commit": "not-hex"}, {"snapshot": "not-uuid"}]:
            with self.subTest(extra=extra), tempfile.TemporaryDirectory() as root:
                _, original, _ = self.fixture()

                def call(name, args):
                    return {**original(name, args), **extra}
                with self.assertRaises(ValueError):
                    workspace.download(call, "my-room", Path(root) / "room.bundle")
                self.assertEqual(list(Path(root).iterdir()), [])

    def test_mcp_rejects_unsafe_endpoints_without_connecting(self):
        for url in ["http://public.example/mcp", "https://user:pass@example.com/mcp",
                    "https://example.com/not-mcp", "http://localhost:47990/mcp?token=x"]:
            with self.subTest(url=url), self.assertRaises(ValueError):
                workspace.MCP(url, "secret")

    def test_claim_proof_is_a_session_header_never_a_tool_argument(self):
        proof = "A" * 43
        requests = []

        class Response:
            headers = {"Mcp-Session-Id": "synthetic-session"}

            def __enter__(self):
                return self

            def __exit__(self, *args):
                pass

            def read(self, limit):
                request = requests[-1]
                body = json.loads(request.data)
                if body["method"].startswith("notifications/"):
                    return b""
                result = {} if body["method"] == "initialize" else {"structuredContent": {"result": {"ok": True}}}
                return json.dumps({"jsonrpc": "2.0", "id": body["id"], "result": result}).encode()

        class Opener:
            def open(self, request, **kwargs):
                requests.append(request)
                return Response()

        for proposed in [None, proof]:
            with self.subTest(proposed=proposed is not None):
                requests.clear()
                client = workspace.MCP("http://127.0.0.1:47990/mcp", "synthetic-token", lease_proof=proposed)
                client.opener = Opener()
                with client:
                    self.assertEqual(client.call("simmis_worker-claim", {"run": "synthetic"}), {"ok": True})
                self.assertEqual(len(requests), 4)
                for request in requests:
                    self.assertEqual(request.get_header("Simmis-lease-proof"), proposed)
                    if request.data:
                        self.assertNotIn(proof, request.data.decode())
                self.assertEqual(requests[-1].method, "DELETE")
        for invalid in [True, "", "x" * 42, "x" * 44, "x" * 42 + "\n"]:
            with self.subTest(invalid=invalid), self.assertRaises(ValueError):
                workspace.MCP("http://127.0.0.1:47990/mcp", "synthetic-token", lease_proof=invalid)


if __name__ == "__main__":
    unittest.main()
