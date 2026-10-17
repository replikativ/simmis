"""No-model MCP integration fixture; credentials arrive only on stdin.

Launched by mcp-workers-test, not a coding-CLI worker or a model benchmark.
Default CI mode makes fixed file changes without task execution. The explicit
--physical host mode runs that fixed program through the isolated controller.
"""
import json
import os
from pathlib import Path
import sys
import tempfile

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
from mcp_workspace import MCP
import worker_claim
import worker_inputs
import worker_outputs


def physical(private, parent):
    """Explicit no-model host integration, never the default CI path."""
    import worker_controller
    import worker_journal
    program = """
from pathlib import Path
assert {str(p) for p in Path('.').rglob('*') if p.is_file()} == {'docs/input.md','docs/资料.bin','bin/run.sh'}
assert Path('docs/资料.bin').read_bytes() == (bytes([0,255,128,42]) * 12500)
assert Path('bin/run.sh').stat().st_mode & 0o777 == 0o700
Path('docs/input.md').write_bytes(b'changed selected input')
Path('bin/run.sh').unlink()
Path('out/report').mkdir(parents=True)
Path('out/report/结果.bin').write_bytes(b'\\x00\\xff\\x80result')
Path('out/report/结果.bin').chmod(0o700)
print('bounded round trip',end='')
"""
    client = worker_controller.Controller(parent, private["worker"], private["url"], private["config"], heartbeat_seconds=0.05)
    try:
        acknowledged = client.run(private["run"], private["credential"], ["/usr/bin/python3", "-I", "-c", program])
        assert acknowledged["outcome"] == "completed"
        chain = worker_journal.load(Path(parent) / "runs" / private["run"])
        assert all(chain[phase] is not None for phase in worker_journal.PHASES)
        assert worker_journal.unresolved(client.runs, private["worker"]) == []
        assert chain["stopped"]["output"]["summary"] == 'bounded round trip'
        assert chain["stopped"]["output"]["exit-code"] == 0
        print(json.dumps({"verified": True, "client-claimed": True, "completion-acknowledged": True,
                          "files": ['bin/run.sh','docs/input.md','docs/资料.bin'],
                          "physically-stopped": True, "journal-acknowledged": True}))
    finally:
        client.close()


def main():
    private = json.load(sys.stdin)
    with tempfile.TemporaryDirectory(prefix="simmis-worker-roundtrip-") as parent:
        # Persist the freshly created state directory's entry before admission.
        fd = os.open(Path(parent).parent, os.O_RDONLY | os.O_DIRECTORY)
        try:
            os.fsync(fd)
        finally:
            os.close(fd)
        if sys.argv[1:] == ["--physical"]:
            return physical(private, parent)
        if sys.argv[1:]:
            raise ValueError("Unknown synthetic fixture mode")
        with worker_claim.controller_lock(parent, private["worker"]):
            path = Path(parent) / "claim.json"
            ticket = worker_claim.prepare(path, private["worker"], private["run"])
            with MCP(private["url"], private["credential"], lease_proof=ticket["proof"]) as enrollment:
                receipt = enrollment.call("simmis_worker-claim", {"run": ticket["run"], "nonce": ticket["nonce"]})
            assert receipt["run"] == ticket["run"] and receipt["lease"] == ticket["nonce"]
            assert receipt["state"] == "claimed" and receipt["stop-ack?"] is False
            assert receipt["cancel-requested?"] is False
            # Reconstruct authority from disk, not the returned lease ID.
            token = worker_claim.lease_token(worker_claim.load(path))
            with MCP(private["url"], token) as client:
                frozen = worker_inputs.fetch(client.call, private["run"], private["config"])
            root = worker_inputs.materialize(frozen, private["config"], parent)
            actual = {str(p.relative_to(root)) for p in root.rglob("*") if p.is_file()}
            assert actual == set(frozen["files"])
            for name, file in frozen["files"].items():
                assert (root / name).read_bytes() == worker_inputs.file_bytes(file)
                assert (root / name).stat().st_mode & 0o777 == worker_inputs.MODES[file["mode"]]
            (root / "docs/input.md").write_bytes(b"changed selected input")
            (root / "bin/run.sh").unlink()
            (root / "out/report").mkdir(parents=True)
            (root / "out/report/结果.bin").write_bytes(b"\x00\xff\x80result")
            (root / "out/report/结果.bin").chmod(0o700)
            output = worker_outputs.collect(root, frozen, private["config"], "bounded round trip", 0)
            assert set(output["files"]) == {"docs/input.md", "bin/run.sh", "out/report/结果.bin"}
            with MCP(private["url"], token) as client:
                receipt = client.call("simmis_worker-complete", {"run": private["run"], "output": output})
            assert receipt["stop-ack?"] is True and receipt["state"] == "completed"
            print(json.dumps({"verified": True, "client-claimed": True,
                              "files": sorted(actual), "completion-acknowledged": True}))


if __name__ == "__main__":
    try:
        main()
    except Exception:
        sys.exit("Synthetic worker HTTP round trip failed")
