import base64
import hashlib
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch
import uuid

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import worker_inputs as inputs


CONFIG = {"adapter": "codex", "version": "fixture-1", "model": "fixture-model", "policy": "local-trusted-v1"}


def file(data=b"input\x00\xff", mode=0o100644):
    return {"base64": base64.b64encode(data).decode(), "bytes": len(data),
            "sha256": hashlib.sha256(data).hexdigest(), "mode": mode}


def frozen():
    return {"input-id": str(uuid.uuid4()), "task": "Work on explicit inputs",
            "config": CONFIG.copy(), "files": {"docs/知识.md": file(), "bin/test": file(b"#!/bin/sh\n", 0o100755)},
            "limits": {"timeout-ms": 60000, "settlement": "review"}, "policy": inputs.POLICY.copy()}


def transport(bundle, mutate=None, encoded=None):
    encoded = encoded if encoded is not None else json.dumps(bundle, ensure_ascii=False).encode()
    calls = []

    def call(name, args):
        calls.append((name, args))
        offset = args["offset"]
        end = min(len(encoded), offset + args["length"])
        part = {"input-id": bundle["input-id"], "bytes": len(encoded),
                "sha256": hashlib.sha256(encoded).hexdigest(), "offset": offset,
                "base64": base64.b64encode(encoded[offset:end]).decode()}
        if end < len(encoded):
            part["next-offset"] = end
        if mutate:
            mutate(part)
        return part
    return call, calls


class InputTest(unittest.TestCase):
    def test_chunked_frozen_input_materializes_only_selected_binary_and_executable_files(self):
        bundle = frozen()
        bundle["files"]["docs/知识.md"] = file(b"binary\x00\xff" * 6000)
        call, calls = transport(bundle)
        received = inputs.fetch(call, str(uuid.uuid4()), CONFIG)
        self.assertEqual(received, bundle)
        self.assertGreater(len(calls), 1)
        self.assertTrue(all(name == "simmis_worker-input" for name, _ in calls))
        with tempfile.TemporaryDirectory() as parent:
            root = inputs.materialize(received, CONFIG, parent)
            self.assertEqual(root.stat().st_mode & 0o777, 0o700)
            self.assertEqual((root / "docs/知识.md").read_bytes(), b"binary\x00\xff" * 6000)
            self.assertEqual((root / "docs/知识.md").stat().st_mode & 0o777, 0o600)
            self.assertEqual((root / "bin/test").stat().st_mode & 0o777, 0o700)
            self.assertEqual({str(p.relative_to(root)) for p in root.rglob("*") if p.is_file()},
                             set(bundle["files"]))

    def test_chunks_refuse_identity_offsets_size_digest_and_encoding_changes(self):
        changes = [lambda p: p.update(offset=True), lambda p: p.update(offset=1),
                   lambda p: p.update(**{"input-id": str(uuid.uuid4())}),
                   lambda p: p.update(bytes=inputs.POLICY["max-input-json-bytes"] + 1),
                   lambda p: p.update(bytes=True), lambda p: p.update(sha256="0" * 64),
                   lambda p: p.update(base64="bad!"), lambda p: p.update(base64=""),
                   lambda p: p.update(**{"next-offset": 0}),
                   lambda p: p.update(unexpected="field")]
        for change in changes:
            with self.subTest(change=change):
                bundle = frozen()
                call, _ = transport(bundle, change)
                with self.assertRaises(ValueError):
                    inputs.fetch(call, str(uuid.uuid4()), CONFIG)

    def test_every_chunk_must_keep_the_first_identity(self):
        bundle = frozen()
        bundle["files"]["large"] = file(b"x" * 20000)
        call, _ = transport(bundle, lambda p: p.update(**{"input-id": str(uuid.uuid4())}) if p["offset"] else None)
        with self.assertRaises(ValueError):
            inputs.fetch(call, str(uuid.uuid4()), CONFIG)

    def test_duplicate_json_keys_constants_and_non_utf8_are_refused(self):
        bundle = frozen()
        for encoded in [b'{"input-id":"one","input-id":"two"}', b'{"x":NaN}', b'\xff']:
            with self.subTest(encoded=encoded):
                call, _ = transport(bundle, encoded=encoded)
                with self.assertRaises(ValueError):
                    inputs.fetch(call, str(uuid.uuid4()), CONFIG)

    def test_runtime_control_and_noncanonical_paths_are_refused_before_any_write(self):
        paths = ["../outside", "/absolute", "a/../b", "a/./b", "a//b", "a/", "a\\b", "a\x00b",
                 ".git/config", ".codex/config.toml", ".Codex/config.toml", "a/.claude/settings.json",
                 ".agents/skills/evil/SKILL.md", ".ssh/id", ".aws/credentials", "a/ /b",
                 ".mcp.json", "nested/.mcp.json", ".claude.json", "bad\ud800path"]
        for path in paths:
            with self.subTest(path=path), tempfile.TemporaryDirectory() as parent:
                bundle = frozen()
                bundle["files"] = {path: file()}
                with self.assertRaises(ValueError):
                    inputs.materialize(bundle, CONFIG, parent)
                self.assertEqual(list(Path(parent).iterdir()), [])

    def test_closed_bundle_and_local_config_pin_are_checked(self):
        changes = [lambda b: b.update(extra=True), lambda b: b["config"].update(model="other"),
                   lambda b: b["limits"].update(settlement="automatic"),
                   lambda b: b["limits"].update(**{"timeout-ms": True}),
                   lambda b: b["policy"].update(version=True), lambda b: b["policy"].update(extra=1),
                   lambda b: b["files"].update({"bin": file()}),
                   lambda b: b["files"].update({"bad": {**file(), "sha256": "0" * 64}}),
                   lambda b: b["files"].update({"bad": {**file(), "mode": True}}),
                   lambda b: b["files"].update({"bad": {**file(), "mode": 0o120000}}),
                   lambda b: b.update(files={str(i): file() for i in range(101)})]
        for change in changes:
            with self.subTest(change=change), tempfile.TemporaryDirectory() as parent:
                bundle = frozen()
                change(bundle)
                with self.assertRaises(ValueError):
                    inputs.materialize(bundle, CONFIG, parent)
                self.assertEqual(list(Path(parent).iterdir()), [])

    def test_total_and_per_file_bounds(self):
        for files in [{"one": file(b"x" * (inputs.POLICY["max-file-bytes"] + 1))},
                      {str(i): file(b"x" * 800000) for i in range(3)}]:
            bundle = frozen()
            bundle["files"] = files
            with self.assertRaises(ValueError):
                inputs.validate(bundle, CONFIG)

    def test_symlink_inserted_during_directory_creation_cannot_escape(self):
        with tempfile.TemporaryDirectory() as parent, tempfile.TemporaryDirectory() as outside:
            sentinel = Path(outside) / "知识.md"
            sentinel.write_bytes(b"keep")
            mkdir = os.mkdir

            def raced(path, *args, **kwargs):
                if path == "docs":
                    os.symlink(outside, "docs", dir_fd=kwargs["dir_fd"])
                else:
                    return mkdir(path, *args, **kwargs)

            with patch.object(inputs.os, "mkdir", raced), self.assertRaises(OSError):
                inputs.materialize(frozen(), CONFIG, parent)
            self.assertEqual(sentinel.read_bytes(), b"keep")
            self.assertEqual(list(Path(parent).iterdir()), [])

    def test_each_materialization_has_its_own_private_directory(self):
        with tempfile.TemporaryDirectory() as parent:
            bundle = frozen()
            first = inputs.materialize(bundle, CONFIG, parent)
            second = inputs.materialize(bundle, CONFIG, parent)
            self.assertNotEqual(first, second)
            (first / "bin/test").write_bytes(b"changed first workspace")
            self.assertEqual((second / "bin/test").read_bytes(), b"#!/bin/sh\n")


if __name__ == "__main__":
    unittest.main()
