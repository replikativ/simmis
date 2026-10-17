import base64
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import worker_inputs as inputs
import worker_outputs as outputs
from test_worker_inputs import CONFIG, file, frozen


class OutputTest(unittest.TestCase):
    def test_only_changes_deletions_and_out_deliverables_are_collected(self):
        bundle = frozen()
        with tempfile.TemporaryDirectory() as parent:
            root = inputs.materialize(bundle, CONFIG, parent)
            (root / "docs/知识.md").unlink()
            (root / "out").mkdir()
            (root / "out/结果.bin").write_bytes(b"\x00\xffchanged")
            (root / "out/结果.bin").chmod(0o700)
            (root / "unselected.txt").write_text("not a selected input or deliverable")
            result = outputs.collect(root, bundle, CONFIG, "done", 0)
            self.assertEqual(result, {"files": {"docs/知识.md": None,
                                                "out/结果.bin": file(b"\x00\xffchanged", 0o100755)},
                                       "summary": "done", "exit-code": 0})

    def test_unchanged_selected_files_and_unchanged_selected_out_files_are_omitted(self):
        bundle = frozen()
        bundle["files"]["out/existing"] = file(b"already present")
        with tempfile.TemporaryDirectory() as parent:
            root = inputs.materialize(bundle, CONFIG, parent)
            self.assertEqual(outputs.collect(root, bundle, CONFIG, "", 0)["files"], {})
            (root / "out/existing").write_bytes(b"changed")
            self.assertEqual(outputs.collect(root, bundle, CONFIG, "", 0)["files"],
                             {"out/existing": file(b"changed")})

    def test_chmod_only_changes_are_preserved(self):
        bundle = frozen()
        with tempfile.TemporaryDirectory() as parent:
            root = inputs.materialize(bundle, CONFIG, parent)
            (root / "docs/知识.md").chmod(0o700)
            self.assertEqual(outputs.collect(root, bundle, CONFIG, "chmod", 0)["files"],
                             {"docs/知识.md": {**bundle["files"]["docs/知识.md"], "mode": 0o100755}})

    def test_symlinked_selected_files_parents_and_out_tree_are_refused(self):
        for location in ["file", "parent", "out-root", "out-child"]:
            with self.subTest(location=location), tempfile.TemporaryDirectory() as parent:
                bundle = frozen()
                root = inputs.materialize(bundle, CONFIG, parent)
                outside = Path(parent) / "outside"
                outside.mkdir()
                (outside / "知识.md").write_bytes(b"not task data")
                if location == "file":
                    (root / "docs/知识.md").unlink()
                    (root / "docs/知识.md").symlink_to(outside / "知识.md")
                elif location == "parent":
                    (root / "docs/知识.md").unlink()
                    (root / "docs").rmdir()
                    (root / "docs").symlink_to(outside, target_is_directory=True)
                elif location == "out-root":
                    (root / "out").symlink_to(outside, target_is_directory=True)
                else:
                    (root / "out").mkdir()
                    (root / "out/link").symlink_to(outside, target_is_directory=True)
                with self.assertRaises((OSError, ValueError)):
                    outputs.collect(root, bundle, CONFIG, "no", 0)
                self.assertEqual((outside / "知识.md").read_bytes(), b"not task data")

    def test_hardlinks_and_fifo_outputs_are_refused_without_reading(self):
        for kind in ["hardlink", "fifo"]:
            with self.subTest(kind=kind), tempfile.TemporaryDirectory() as parent:
                bundle = frozen()
                root = inputs.materialize(bundle, CONFIG, parent)
                (root / "out").mkdir()
                destination = root / "out/result"
                if kind == "fifo":
                    os.mkfifo(destination)
                else:
                    outside = Path(parent) / "outside.txt"
                    outside.write_bytes(b"not authorized")
                    os.link(outside, destination)
                with self.assertRaises(ValueError):
                    outputs.collect(root, bundle, CONFIG, "no", 0)

    def test_per_file_total_count_and_directory_walk_bounds(self):
        for kind in ["file", "total", "count", "walk"]:
            with self.subTest(kind=kind), tempfile.TemporaryDirectory() as parent:
                bundle = frozen()
                root = inputs.materialize(bundle, CONFIG, parent)
                (root / "out").mkdir()
                if kind == "file":
                    (root / "out/large").write_bytes(b"x" * (inputs.POLICY["max-file-bytes"] + 1))
                elif kind == "total":
                    for n in range(3):
                        (root / "out" / str(n)).write_bytes(b"x" * 800000)
                elif kind == "count":
                    for n in range(101):
                        (root / "out" / str(n)).write_bytes(b"")
                else:
                    with patch.object(outputs, "MAX_ENTRIES", 2):
                        for n in range(3):
                            (root / "out" / str(n)).mkdir()
                        with self.assertRaises(ValueError):
                            outputs.collect(root, bundle, CONFIG, "too many directories", 0)
                    continue
                with self.assertRaises(ValueError):
                    outputs.collect(root, bundle, CONFIG, "too much", 0)

    def test_selected_special_files_and_privileged_modes_are_refused(self):
        for kind in ["fifo", "hardlink", "setuid", "setgid"]:
            with self.subTest(kind=kind), tempfile.TemporaryDirectory() as parent:
                bundle = frozen()
                root = inputs.materialize(bundle, CONFIG, parent)
                target = root / "docs/知识.md"
                if kind == "fifo":
                    target.unlink()
                    os.mkfifo(target)
                elif kind == "hardlink":
                    target.unlink()
                    outside = Path(parent) / "outside.txt"
                    outside.write_bytes(b"not a selected input")
                    os.link(outside, target)
                else:
                    target.chmod(0o4600 if kind == "setuid" else 0o2600)
                with self.assertRaises(ValueError):
                    outputs.collect(root, bundle, CONFIG, "no", 0)

    def test_fifo_is_rejected_before_any_open_of_the_special_file(self):
        with tempfile.TemporaryDirectory() as parent:
            os.mkfifo(Path(parent) / "pipe")
            directory = os.open(parent, os.O_RDONLY | os.O_DIRECTORY)
            try:
                with patch.object(outputs.os, "open", side_effect=AssertionError("Special file was opened")):
                    with self.assertRaises(ValueError):
                        outputs._read_file(directory, "pipe")
            finally:
                os.close(directory)

    def test_runtime_configs_and_unprivate_workspace_are_refused(self):
        bundle = frozen()
        with tempfile.TemporaryDirectory() as parent:
            root = inputs.materialize(bundle, CONFIG, parent)
            root.chmod(0o755)
            with self.assertRaises(ValueError):
                outputs.collect(root, bundle, CONFIG, "no", 0)
            root.chmod(0o700)
            (root / "out").mkdir()
            (root / "out/.mcp.json").write_text("{}")
            with self.assertRaises(ValueError):
                outputs.collect(root, bundle, CONFIG, "no", 0)

    def test_selected_regular_file_named_out_is_not_a_directory(self):
        bundle = frozen()
        bundle["files"]["out"] = file(b"input named out")
        with tempfile.TemporaryDirectory() as parent:
            root = inputs.materialize(bundle, CONFIG, parent)
            (root / "out").write_bytes(b"changed")
            self.assertEqual(outputs.collect(root, bundle, CONFIG, "done", 7)["files"],
                             {"out": file(b"changed")})

    def test_missing_selected_parent_is_a_deletion_not_a_recursive_upload(self):
        bundle = frozen()
        with tempfile.TemporaryDirectory() as parent:
            root = inputs.materialize(bundle, CONFIG, parent)
            (root / "docs/知识.md").unlink()
            (root / "docs").rmdir()
            self.assertEqual(outputs.collect(root, bundle, CONFIG, "deleted", 0)["files"],
                             {"docs/知识.md": None})


if __name__ == "__main__":
    unittest.main()
