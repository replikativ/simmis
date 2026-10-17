"""No-model physical crash fixture, NOT an MCP or coding worker controller.

The parent deliberately SIGKILLs this process after durable admission and GO.
Its replacement must reconcile the recorded unit, never execute a new task.
All identities and content are synthetic; no server lease is claimed here.
"""
import json
import os
from pathlib import Path
import selectors
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import worker_claim as claim
import worker_journal as journal
import worker_namespace as namespace
import worker_unit as unit


def main():
    options = json.loads(sys.stdin.buffer.readline())
    folder = Path(options["folder"])
    with claim.controller_lock(options["root"], options["worker"]):
        ticket = claim.prepare(folder / "claim.json", options["worker"], options["run"])
        launch = journal.prepare(folder, ticket, options["endpoint"], options["frozen"], options["config"])
        observer = unit.UnitBus()
        input_read, input_write = os.pipe()
        output_read, output_write = os.pipe()
        with open(os.devnull, "wb") as errors:
            try:
                observer.launch(launch["unit"], input_read, output_write, errors.fileno(), timeout_ms=10000)
            finally:
                os.close(input_read)
                os.close(output_write)
        with os.fdopen(input_write, "wb", buffering=0) as control, os.fdopen(output_read, "rb", buffering=0) as output:
            request = {"frozen": options["frozen"], "config": options["config"],
                       "argv": ["/usr/bin/python3", "-I", "-c", "import signal; signal.pause()"]}
            control.write(json.dumps(request).encode() + b"\n")
            with selectors.DefaultSelector() as ready:
                ready.register(output, selectors.EVENT_READ)
                if not ready.select(timeout=25):
                    raise RuntimeError("Fixture guard did not become ready")
            expected = {"kind": "ready", "policy": namespace.POLICY, "input-id": options["frozen"]["input-id"]}
            if json.loads(output.readline(namespace.MAX_REQUEST_BYTES + 1)) != expected:
                raise RuntimeError("Fixture guard ready identity differs")
            identity = observer.capture(launch["unit"])
            journal.admit(folder, identity)  # actual fsync barrier BEFORE GO
            control.write(b"GO\n")
            print(json.dumps({"kind": "go-sent", "unit": launch["unit"]}), flush=True)
            sys.stdin.buffer.readline()  # parent kills us, not a timing delay
            raise RuntimeError("Fixture parent must simulate abrupt controller death")


if __name__ == "__main__":
    main()
