"""Private local SCI worker driver. Never expose this program as an agent tool.

Enroll with an explicit worker-control web session. Fund only with `allowance`;
submit selected-file tasks with `task`; `run` executes ONE queued assignment.
`reconcile` never executes, needs no runtime/login, and preserves ambiguity.
No native/provider credential is printed, uploaded or given to the SCI guest.
Requires Linux user systemd/cgroup-v2, python3-dbus/python3-gi and Java21+ at
/usr/bin/java. SCI is a soft interpreter boundary, not an OS sandbox.
"""
import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import uuid

# Support both normal Python invocation and the trusted -I bootstrap.
sys.path.insert(0, str(Path(__file__).resolve().parent))
import mcp_client
from mcp_workspace import MCP
import worker_claim as claim
from worker_controller import Controller
import worker_inputs as inputs
import worker_journal as journal
import worker_runtime as runtime
from worker_transport import MCP as WorkerMCP


def state_root(value, *, create=False):
    path = Path(value).expanduser().absolute()
    if str(path.resolve(strict=False)) != str(path):
        raise ValueError("Worker state must have a canonical non-aliased path")
    if create and not path.exists():
        # Parent installation is trusted local administration. Require it to
        # exist so this function can establish the exact durability barrier.
        path.mkdir(mode=0o700)
        parent = os.open(path.parent, os.O_RDONLY | os.O_DIRECTORY)
        try:
            os.fsync(parent)
        finally:
            os.close(parent)
    directory = claim._directory(path)
    os.fsync(directory)
    os.close(directory)
    return path


def record(root, phase, value=None):
    directory = claim._directory(root)
    try:
        if value is not None:
            return journal._publish(directory, phase, value)
        result = journal._read(directory, phase)
        if result is None:
            raise ValueError("Private worker enrollment has not completed")
        os.fsync(directory)
        return result
    finally:
        os.close(directory)


def configuration(selected, model):
    runtime.validate_profile(selected)
    if not isinstance(model, str) or not 1 <= len(model) <= 200:
        raise ValueError("Expected an explicit registered native Codex model")
    # Bind the same local package/policies across enrollment, fetched frozen
    # inputs and later invocation. Local installation paths are not uploaded.
    version = "sci1/" + "/".join([selected["runtime"]["sha256"], selected["executor-policy-id"], selected["inference-policy-id"]])
    return {"adapter": "codex", "version": version, "model": model, "policy": "local-trusted-v1"}


def preflight(descriptor):
    runtime.verify(descriptor)
    with tempfile.TemporaryDirectory(prefix="simmis-worker-preflight-") as home:
        result = subprocess.run(runtime.argv(descriptor, "check", home), cwd=home, env=runtime.environment(home),
                                stdin=subprocess.DEVNULL, capture_output=True, timeout=120)
    if result.returncode != 0 or len(result.stdout) > 4096:
        raise ValueError("Packaged worker preflight failed")
    value = json.loads(result.stdout, object_pairs_hook=inputs._unique_object, parse_constant=inputs._invalid_constant)
    inputs._closed(value, {"version", "kind", "executor-policy-id", "inference-policy-id"})
    if type(value["version"]) is not int or value["version"] != 1 or value["kind"] != "runtime-ready":
        raise ValueError("Packaged worker preflight differs")
    return runtime.profile(descriptor, value["executor-policy-id"], value["inference-policy-id"])


def human_call(endpoint, session, name, arguments):
    token = mcp_client.session_credential(session, expected_mode="worker-control", expected_endpoint=endpoint)
    with MCP(endpoint, token) as client:
        return client.call(name, arguments)


def checked_enrollment(intent, result):
    inputs._closed(result, {"worker", "room", "generation", "config-id", "config", "credential"})
    if (result["worker"] != intent["worker"] or result["config"] != intent["config"]
            or type(result["generation"]) is not int or not 0 < result["generation"] < 2 ** 63
            or not isinstance(result["credential"], str)
            or not re.fullmatch(r"simmis-worker\." + re.escape(intent["worker"]) + r"\.[A-Za-z0-9_-]{43}", result["credential"])):
        raise ValueError("Enrollment receipt differs from durable local intent")
    inputs.canonical_uuid(result["room"])
    inputs.canonical_uuid(result["config-id"])
    return result


def enroll(root, endpoint, session, room, descriptor, model, worker=None):
    """Durable intent BEFORE network; exact retry recovers the same credential."""
    MCP(endpoint, "")  # validate destination before preflight or credentials
    directory = claim._directory(root)
    try:
        old = journal._read(directory, "worker-intent")
    finally:
        os.close(directory)
    if old is None:
        selected = preflight(descriptor)
        intent = {"version": 1, "worker": inputs.canonical_uuid(worker or str(uuid.uuid4())),
                  "endpoint": endpoint, "room": room, "profile": selected, "config": configuration(selected, model)}
    else:
        selected = runtime.validate_profile(old["profile"])
        runtime.verify(descriptor)
        intent = {"version": 1, "worker": inputs.canonical_uuid(worker or old["worker"]),
                  "endpoint": endpoint, "room": room, "profile": selected, "config": configuration(selected, model)}
        if selected["runtime"] != descriptor:
            raise ValueError("Runtime differs from durable enrollment intent")
    if not isinstance(room, str) or not 1 <= len(room) <= 200:
        raise ValueError("Expected a bounded room handle")
    intent = record(root, "worker-intent", intent)
    # Share the existing permanent controller identity, rather than creating
    # an enrollment in a root already owned by a different physical worker.
    record(root, "controller", {"version": 1, "worker": intent["worker"], "endpoint": endpoint})
    reply = human_call(endpoint, session, "simmis_local-worker-enroll",
                       {"room": room, "worker": intent["worker"], "config": intent["config"]})
    checked_enrollment(intent, reply)
    record(root, "worker-enrollment", reply)
    return {"worker": reply["worker"], "room": reply["room"], "generation": reply["generation"], "enrolled": True}


def enrollment(root):
    intent = record(root, "worker-intent")
    inputs._closed(intent, {"version", "worker", "endpoint", "room", "profile", "config"})
    if type(intent["version"]) is not int or intent["version"] != 1:
        raise ValueError("Unknown local worker state")
    inputs.canonical_uuid(intent["worker"])
    MCP(intent["endpoint"], "")
    if intent["config"] != configuration(intent["profile"], intent["config"]["model"]):
        raise ValueError("Local configuration differs from pinned runtime")
    return intent, checked_enrollment(intent, record(root, "worker-enrollment"))


def run_one(root, *, codex_home=None, run=None, reconcile=False):
    intent, issued = enrollment(root)
    # Recovery deliberately does not verify an artifact or open a login.
    if not reconcile:
        runtime.verify(intent["profile"]["runtime"])
        runtime.environment("/unused-private-home", codex_home=codex_home)
        if codex_home is None:
            raise ValueError("Select the broker-only Codex home explicitly")
        if run is None:
            with WorkerMCP(intent["endpoint"], issued["credential"], timeout=5) as client:
                pending = client.call("simmis_worker-pending", {})
            inputs._closed(pending, {"runs"})
            if type(pending["runs"]) is not list or len(pending["runs"]) > 10:
                raise ValueError("Invalid pending worker response")
            for selected in pending["runs"]:
                inputs.canonical_uuid(selected)
            if not pending["runs"]:
                return {"worker": intent["worker"], "queued": 0}
            run = pending["runs"][0]
    inputs.canonical_uuid(run)
    controller = Controller(root, intent["worker"], intent["endpoint"], intent["config"])
    try:
        result = (controller.reconcile(run) if reconcile else
                  controller.run(run, issued["credential"], sci={"profile": intent["profile"], "codex-home": codex_home}))
        return {"run": run, "outcome": result["outcome"]}
    finally:
        # A close refusal leaves physical ownership unresolved. Never delete
        # its state or convert it into a server stopped acknowledgement.
        controller.close()


def operate(root, session, command, arguments):
    intent, issued = enrollment(root)
    fields = {**arguments, "room": issued["room"]}
    if command in {"revoke", "task"}:
        fields["worker"] = intent["worker"]
    if command in {"allowance", "task"}:
        inputs.canonical_uuid(fields["request-id"])
    response = human_call(intent["endpoint"], session, "simmis_local-worker-" + command, fields)
    # Never print arbitrary server fields or worker credentials. Full execution
    # and captured-file evidence remain available through room MCP/UI surfaces.
    if type(response) is not dict:
        raise ValueError("Invalid human worker-control response")
    allowed = {"worker", "revoked", "room", "id", "client-id", "job", "state", "status",
               "max-worker-admissions", "timeout-ms", "settlement", "return-pending?"}
    return {key: value for key, value in response.items() if key in allowed and
            (type(value) in {bool, int} or (isinstance(value, str) and len(value) <= 200))}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--state", required=True, help="Dedicated private controller directory; parent must exist")
    parser.add_argument("--session", type=Path, help="Private human worker-control session (enroll/allowance/task/revoke only)")
    commands = parser.add_subparsers(dest="command", required=True)
    init = commands.add_parser("enroll")
    init.add_argument("--endpoint", required=True)
    init.add_argument("--room", required=True)
    init.add_argument("--jar", required=True)
    init.add_argument("--sha256", required=True)
    init.add_argument("--model", required=True)
    init.add_argument("--worker")
    fund = commands.add_parser("allowance")
    fund.add_argument("--request-id", required=True, help="Stable UUID; reuse on retry")
    fund.add_argument("--admissions", type=int, required=True)
    task = commands.add_parser("task")
    task.add_argument("--request-id", required=True, help="Stable UUID; reuse on retry")
    task.add_argument("--task", required=True)
    task.add_argument("--path", action="append", default=[])
    task.add_argument("--timeout-ms", type=int, default=300000,
                      help="Total task deadline including cold JVM startup; default 300000, maximum 600000")
    task.add_argument("--settlement", choices=["review", "discard"], default="review")
    commands.add_parser("revoke")
    run = commands.add_parser("run")
    run.add_argument("--codex-home", required=True, help="Canonical broker-only local login directory; never uploaded")
    run.add_argument("--run")
    recover = commands.add_parser("reconcile")
    recover.add_argument("--run", required=True)
    args = parser.parse_args()
    if args.command in {"enroll", "allowance", "task", "revoke"} and args.session is None:
        parser.error("This control requires an explicitly selected private worker-control session")
    root = state_root(args.state, create=args.command == "enroll")
    if args.command == "enroll":
        result = enroll(root, args.endpoint, args.session, args.room,
                        {"jar": str(Path(args.jar).expanduser().absolute()), "sha256": args.sha256}, args.model, args.worker)
    elif args.command == "run":
        result = run_one(root, codex_home=str(Path(args.codex_home).expanduser().absolute()), run=args.run)
    elif args.command == "reconcile":
        result = run_one(root, run=args.run, reconcile=True)
    elif args.command == "allowance":
        if not 1 <= args.admissions <= 32:
            parser.error("Select 1..32 explicit admissions")
        result = operate(root, args.session, "allowance", {"request-id": args.request_id, "max-worker-admissions": args.admissions})
    elif args.command == "task":
        if not 1000 <= args.timeout_ms <= 600000 or len(args.path) > 100 or not 1 <= len(args.task) <= 16000:
            parser.error("Task exceeds finite worker bounds")
        for path in args.path:
            inputs.path_parts(path)
        result = operate(root, args.session, "task", {"request-id": args.request_id, "task": args.task, "paths": args.path,
                                                    "limits": {"timeout-ms": args.timeout_ms, "settlement": args.settlement}})
    else:
        result = operate(root, args.session, "revoke", {})
    print(json.dumps(result, separators=(",", ":")))


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit("Local worker interrupted; preserve private state and reconcile unresolved Runs.")
    except Exception:
        sys.exit("Local worker operation failed; preserve private state and reconcile unresolved Runs. "
                 "Check the private login mode, installed runtime and server permissions; no task was automatically retried.")
