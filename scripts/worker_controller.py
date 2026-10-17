"""Single-use LOCAL execution controller for the existing worker MCP protocol.

This is a trusted host API, not a room command runner or coding-CLI adapter.
The caller supplies a locally pinned argv/config and one canonical private
state root for this origin/enrollment. Never copy tickets or choose independent
lock roots for the same physical worker. Tasks see neither this root nor MCP
credentials. Existing Dvergr Jobs/Runs remain the workflow engine.

Restart recovery never executes or sends GO. Pre-admission ambiguity is held
for operator reconciliation; it is not fabricated into a stopped receipt.
"""
import io
import json
import os
from pathlib import Path
import time

from mcp_workspace import MCP as Destination
from worker_io import Frames
from worker_transport import MCP, NETWORK_SECONDS
import worker_claim as claim
import worker_inputs as inputs
import worker_journal as journal
import worker_namespace as namespace
import worker_outputs as outputs
import worker_unit as unit


class Cancelled(Exception):
    pass


def checked_receipt(value, ticket):
    required = {"run", "lease", "state", "cancel-requested?", "stop-ack?", "expires-at-ms"}
    if (type(value) is not dict or set(value) not in (required, required | {"output-id"})
            or value["run"] != ticket["run"] or value["lease"] != ticket["nonce"]
            or value["state"] not in {"claimed", "quarantined", "stopped", "completed"}
            or type(value["cancel-requested?"]) is not bool or type(value["stop-ack?"]) is not bool
            or type(value["expires-at-ms"]) is not int or not 0 <= value["expires-at-ms"] < 2 ** 63):
        raise ValueError("Worker receipt does not identify the exact bounded lease")
    if "output-id" in value:
        inputs.canonical_uuid(value["output-id"])
    if (value["state"] == "completed") != ("output-id" in value):
        raise ValueError("Worker completion identity is missing or unexpected")
    return value


def require_live(receipt):
    if (receipt["state"] != "claimed" or receipt["cancel-requested?"] or receipt["stop-ack?"]
            or receipt["expires-at-ms"] <= time.time() * 1000 + 1000):
        raise Cancelled("Worker lease is no longer live")


class Lease:
    def __init__(self, client, ticket, *, heartbeat_seconds=5):
        if type(heartbeat_seconds) not in {int, float} or not 0 < heartbeat_seconds <= 5:
            raise ValueError("Invalid worker heartbeat interval")
        self.client, self.ticket = client, ticket
        self.interval, self.next = heartbeat_seconds, 0
        self.valid_until = 0

    def status(self):
        started = time.monotonic()
        value = checked_receipt(self.client.call("simmis_worker-heartbeat", {"run": self.ticket["run"]}), self.ticket)
        now = time.monotonic()
        if now - started >= NETWORK_SECONDS:
            raise Cancelled("Worker receipt exceeded its freshness bound")
        # The pinned frozen policy binds the server's lease TTL. Also check wall
        # expiry conservatively; skew ahead fails closed, never widens access.
        # Monotonic age prevents a later wall-clock rollback extending a receipt.
        remaining = value["expires-at-ms"] / 1000 - time.time() - 1
        self.valid_until = min(started + inputs.POLICY["lease-ttl-ms"] / 1000 - 1,
                               now + max(0, remaining))
        self.next = min(now + self.interval, self.valid_until - 0.5)
        return value

    def tick(self, *, force=False):
        if self.next and time.monotonic() >= self.valid_until:
            raise Cancelled("Worker receipt is no longer fresh")
        if force or time.monotonic() >= self.next:
            require_live(self.status())

    def input_call(self, name, arguments):
        # Frozen input may require hundreds of chunks. Each call must service
        # liveness too, not just the later execution event loop.
        self.tick()
        return self.client.call(name, arguments)


def _child_directory(parent, name):
    """Durably create one fixed/private child, never follow an alias."""
    directory = claim._directory(parent)
    child = None
    try:
        try:
            os.mkdir(name, mode=0o700, dir_fd=directory)
        except FileExistsError:
            pass
        child = os.open(name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=directory)
        info = os.fstat(child)
        if info.st_uid != os.getuid() or info.st_mode & 0o077:
            raise ValueError("Worker child state directory must be private and owned by you")
        os.fsync(child)
        os.fsync(directory)
        return Path(parent) / name
    finally:
        if child is not None:
            os.close(child)
        os.close(directory)


class Controller:
    """One locally owned controller; keep this object on unresolved failures.

    Root must already be durably created/private. An error may leave retained
    physical metadata and an unresolved lease; close refuses that exposure.
    No credential or output is printed. There is no enrollment or CLI entry
    point here; an embedding host deliberately supplies its trusted adapter.
    """
    def __init__(self, root, worker, endpoint, config, *, heartbeat_seconds=5):
        inputs.canonical_uuid(worker)
        Destination(endpoint, "", timeout=5)  # destination validation only
        directory = claim._directory(root)
        try:
            # Bind this canonical lock/state root before any network request;
            # changing an origin/enrollment never reuses another worker's state.
            journal._publish(directory, "controller", {"version": 1, "worker": worker, "endpoint": endpoint})
        finally:
            os.close(directory)
        self.root, self.worker, self.endpoint, self.config = Path(root), worker, endpoint, config
        self.runs = _child_directory(root, "runs")
        self.heartbeat_seconds = heartbeat_seconds
        self.observer = None
        self.frames = None
        self.identity = None
        self.launch = None
        self.publication_attempted = False
        self.used = False

    def _seal(self, folder, output=None, *, tick=None):
        observation = self.observer.wait_stopped(self.identity, on_tick=tick)
        existing = journal.load(folder)["stopped"]
        record = existing if existing is not None else journal.stopped(folder, observation, output)
        # A stopped phase is durable BEFORE metadata becomes collectible.
        self.observer.release(self.identity)
        return record

    def _abort(self, folder, client):
        if self.observer is None:
            return  # no physical allocation; no fabricated journal/remote ack
        if self.identity is None:
            if self.launch and self.launch["unit"] in self.observer.launched:
                self.observer.abort_launch(self.launch["unit"])
            return  # startup/capture ambiguity holds metadata and server lease
        existing = journal.load(folder)["stopped"]
        if existing is None:
            self.observer.stop(self.identity)
            # If the original admission fsync failed after capture, retry that
            # exact record before any stop sealing. A permanent failure holds
            # authority; neither local stop nor a failed disk grants remote ack.
            journal.admit(folder, self.identity)
            self._seal(folder)
        elif self.identity["unit"] in self.observer.references:
            self.observer.release(self.identity)
        if not self.publication_attempted:
            receipt = client.call("simmis_worker-stop", {"run": folder.name})
            journal.acknowledge(folder, "stopped", receipt)

    def _request_stop(self):
        """Absolute HTTP failure requests candidate stop before helper join."""
        if self.observer is None:
            return
        if self.identity and self.identity["unit"] in self.observer.references:
            self.observer.stop(self.identity)
        elif self.launch and self.launch["unit"] in self.observer.launched:
            self.observer.abort_launch(self.launch["unit"])

    def run(self, run, enrollment_credential, argv):
        """Fresh successful claim only; argv is chosen by trusted LOCAL code."""
        inputs.canonical_uuid(run)
        if self.used:
            raise ValueError("Controller instances are single use")
        self.used = True
        with claim.controller_lock(self.root, self.worker):
            if journal.unresolved(self.runs, self.worker):
                raise ValueError("Reconcile every unresolved worker Run before claiming new work")
            if os.path.lexists(self.runs / run):
                raise ValueError("Existing Run journals may only be reconciled, never executed again")
            folder = _child_directory(self.runs, run)
            ticket = claim.prepare(folder / "claim.json", self.worker, run)
            with MCP(self.endpoint, enrollment_credential, lease_proof=ticket["proof"], timeout=5) as enrollment:
                receipt = checked_receipt(enrollment.call("simmis_worker-claim", {"run": run, "nonce": ticket["nonce"]}), ticket)
                require_live(receipt)
            with MCP(self.endpoint, claim.lease_token(ticket), timeout=5, on_failure=self._request_stop) as client:
                lease = Lease(client, ticket, heartbeat_seconds=self.heartbeat_seconds)
                lease.tick(force=True)
                try:
                    frozen = inputs.fetch(lease.input_call, run, self.config)
                    encoded = json.dumps({"frozen": frozen, "config": self.config, "argv": argv},
                                         separators=(",", ":")).encode() + b"\n"
                    namespace.read_request(io.BytesIO(encoded))  # all validation precedes allocation
                    lease.tick(force=True)
                    self.launch = journal.prepare(folder, ticket, self.endpoint, frozen, self.config)
                    self.observer = unit.UnitBus()
                    input_read, input_write = os.pipe()
                    output_read, output_write = os.pipe()
                    self.frames = Frames(input_write, output_read, lease.tick)
                    try:
                        with open(os.devnull, "wb") as errors:
                            self.observer.launch(self.launch["unit"], input_read, output_write, errors.fileno(),
                                                 timeout_ms=frozen["limits"]["timeout-ms"])
                    finally:
                        os.close(input_read)
                        os.close(output_write)
                    startup = time.monotonic() + 25
                    self.frames.write(encoded, startup)
                    ready = self.frames.read(startup)
                    if ready != {"kind": "ready", "policy": namespace.POLICY, "input-id": frozen["input-id"]}:
                        raise ValueError("Worker guard ready identity differs")
                    self.identity = self.observer.capture(self.launch["unit"])
                    journal.admit(folder, self.identity)
                    lease.tick(force=True)  # current permission/cancellation before GO
                    self.frames.write(b"GO\n", time.monotonic() + 5)
                    complete = self.frames.read(time.monotonic() + frozen["limits"]["timeout-ms"] / 1000 + 10)
                    inputs._closed(complete, {"kind", "input-id", "output", "reaped-descendants"})
                    if (complete["kind"] != "complete" or complete["input-id"] != frozen["input-id"]
                            or type(complete["reaped-descendants"]) is not int or complete["reaped-descendants"] < 0):
                        raise ValueError("Worker guard completion identity differs")
                    output = outputs.validate(frozen, self.config, complete["output"])
                    self._seal(folder, output, tick=lease.tick)
                    lease.tick(force=True)
                    # A lost reply must leave the immutable stopped/output
                    # record for reconciliation, not silently publish stop.
                    self.publication_attempted = True
                    receipt = client.call("simmis_worker-complete", {"run": run, "output": output})
                    return journal.acknowledge(folder, "completed", receipt)
                except BaseException as error:
                    if not self.publication_attempted:
                        try:
                            self._abort(folder, client)
                        except BaseException as cleanup_error:
                            raise RuntimeError("Worker failed and cleanup remains unresolved") from cleanup_error
                    raise error
                finally:
                    if self.frames is not None:
                        self.frames.close()

    def reconcile(self, run):
        """Existing admitted/stopped Run only; NEVER claim, launch, GO or replay."""
        inputs.canonical_uuid(run)
        if self.used:
            raise ValueError("Controller instances are single use")
        self.used = True
        with claim.controller_lock(self.root, self.worker):
            folder = self.runs / run
            ticket = claim.load(folder / "claim.json")
            if ticket["worker"] != self.worker or ticket["run"] != run:
                raise ValueError("Worker claim belongs to another controller")
            chain = journal.load(folder)
            launch = chain["launch"]
            if (launch is None or launch["endpoint"] != self.endpoint or launch["frozen"]["config"] != self.config
                    or launch["owner"] != journal._owner(ticket) or chain["admitted"] is None):
                raise ValueError("Pre-admission ambiguity requires operator reconciliation")
            if chain["acknowledged"] is not None:
                return chain["acknowledged"]
            self.launch, self.identity = launch, chain["admitted"]["identity"]
            if chain["stopped"] is None:
                self.observer = unit.UnitBus()
                self.observer.retain(self.identity)
                self.observer.stop(self.identity)
                self._seal(folder)  # never import surviving/lost task output
                chain = journal.load(folder)
            with MCP(self.endpoint, claim.lease_token(ticket), timeout=5, on_failure=self._request_stop) as client:
                status = Lease(client, ticket).status()
                if status["state"] == "completed":
                    # Same unique lease/proof and immutable publication intent;
                    # this is explicit server acceptance, not exception fallback.
                    return journal.acknowledge(folder, "completed", status)
                stopped = chain["stopped"]
                if (stopped["output"] is not None and status["state"] == "claimed"
                        and not status["cancel-requested?"] and not status["stop-ack?"]):
                    self.publication_attempted = True
                    receipt = client.call("simmis_worker-complete", {"run": run, "output": stopped["output"]})
                    return journal.acknowledge(folder, "completed", receipt)
                receipt = client.call("simmis_worker-stop", {"run": run})
                return journal.acknowledge(folder, "stopped", receipt)

    def close(self):
        if self.observer is not None:
            self.observer.close()  # refuses unsealed retained physical exposure
