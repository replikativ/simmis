"""Retained Linux execution-unit observations, not durable worker admission.

This single-threaded host primitive uses a private, persistent D-Bus connection
and the default GLib context. It does not run an event loop in another thread.
Callers must journal a never-reused unit name BEFORE launch, persist the exact
captured identity BEFORE GO, and durably seal a stopped observation BEFORE
release. Missing metadata, manager replacement and deadlines grant no proof.
No MCP calls, CLI credentials, workflow engine or automatic restart live here.
"""
from pathlib import Path, PurePosixPath
import re
import time
import uuid

import worker_namespace as namespace


MANAGER = "org.freedesktop.systemd1"
UNIT = MANAGER + ".Unit"
SERVICE = MANAGER + ".Service"
MEMORY_BYTES = 256 * 1024 * 1024
MAX_TASKS = 64


def verify_cgroup_view(manager_pid, manager_root):
    """A missing group is meaningful only in the manager's real hierarchy."""
    mounts = [line.split(" - ", 1) for line in Path("/proc/self/mountinfo").read_text().splitlines()]
    roots = [(before.split()[3], after.split()[0]) for before, after in mounts
             if before.split()[4] == "/sys/fs/cgroup"]
    if roots != [("/", "cgroup2")]:
        raise ValueError("Observer lacks the execution manager's full cgroup-v2 hierarchy")
    # The protected manager's namespace symlink is ptrace-restricted even for
    # this same UID. Instead verify the kernel's public coordinates against
    # the manager's own root and actual membership in that mounted hierarchy.
    if (not manager_root.startswith("/user.slice/") or ".." in PurePosixPath(manager_root).parts
            or str(PurePosixPath(manager_root)) != manager_root):
        raise ValueError("Unexpected execution manager root")
    paths = [line[3:] for line in Path("/proc", str(manager_pid), "cgroup").read_text().splitlines()
             if line.startswith("0::")]
    if len(paths) != 1 or not (paths[0] == manager_root or paths[0].startswith(manager_root + "/")):
        raise ValueError("Observer and execution manager use different cgroup coordinates")
    members = (Path("/sys/fs/cgroup") / paths[0].lstrip("/") / "cgroup.procs").read_text().splitlines()
    if str(manager_pid) not in members:
        raise ValueError("Execution manager is not in the observer's mounted hierarchy")


def unit_name():
    """Caller must durably journal this identity before launch; never reuse it."""
    return "simmis-worker-" + uuid.uuid4().hex + ".service"


def validate_name(name):
    if not isinstance(name, str) or not re.fullmatch(r"simmis-worker-[0-9a-f]{32}\.service", name):
        raise ValueError("Invalid private execution-unit identity")


def validate_identity(identity):
    if (type(identity) is not dict
            or set(identity) != {"version", "unit", "boot-id", "invocation-id", "cgroup"}
            or type(identity["version"]) is not int or identity["version"] != 1):
        raise ValueError("Invalid execution-unit observation")
    validate_name(identity["unit"])
    if any(not isinstance(identity[key], str) for key in ["boot-id", "invocation-id", "cgroup"]):
        raise ValueError("Execution identities must be strings")
    if str(uuid.UUID(identity["boot-id"])) != identity["boot-id"]:
        raise ValueError("Invalid machine boot identity")
    if not re.fullmatch(r"[0-9a-f]{32}", identity["invocation-id"]) or identity["invocation-id"] == "0" * 32:
        raise ValueError("Missing execution invocation identity")
    group = identity["cgroup"]
    parts = PurePosixPath(group).parts
    if (not group.startswith("/user.slice/") or str(PurePosixPath(group)) != group
            or ".." in parts or parts[-1] != identity["unit"]):
        raise ValueError("Invalid non-root execution cgroup")
    return identity


def cgroup_empty(group):
    """Only meaningful alongside terminal metadata for the exact retained unit."""
    location = Path("/sys/fs/cgroup") / group.lstrip("/")
    try:
        events = (location / "cgroup.events").read_text()
    except FileNotFoundError:
        # A missing file in an existing group is unknown, not empty.
        if location.exists():
            raise ValueError("Missing execution cgroup state")
        return True
    values = dict(line.split() for line in events.splitlines())
    if values.get("populated") not in {"0", "1"}:
        raise ValueError("Malformed execution cgroup state")
    return values["populated"] == "0"


class UnitBus:
    """Explicit-lifetime physical observer; release only after durable sealing.

    Requires host python3-dbus/python3-gi, Linux cgroup v2 and user systemd.
    It intentionally is not a context manager: an exception is not permission
    to release the retained metadata or acknowledge a server lease.
    """

    def __init__(self):
        import dbus
        from dbus.mainloop.glib import DBusGMainLoop
        from gi.repository import GLib
        self.dbus, self.GLib = dbus, GLib
        self.bus = dbus.SessionBus(private=True, mainloop=DBusGMainLoop())
        # libdbus otherwise terminates the controller when an old disconnected
        # connection's event is later dispatched by the shared GLib context.
        self.bus.set_exit_on_disconnect(False)
        self.loop = GLib.MainLoop()
        self.unknown = False
        self.generation = 0
        self.references = set()
        self.confirmed = set()
        self.launched = set()
        self.daemon = dbus.Interface(self.bus.get_object("org.freedesktop.DBus", "/org/freedesktop/DBus"),
                                     "org.freedesktop.DBus")
        self.owner = str(self.daemon.GetNameOwner(MANAGER, timeout=5))
        self.manager_pid = int(self.daemon.GetConnectionUnixProcessID(self.owner, timeout=5))
        manager_object = self.bus.get_object(self.owner, "/org/freedesktop/systemd1")
        self.manager_root = str(dbus.Interface(manager_object, "org.freedesktop.DBus.Properties").Get(
            MANAGER + ".Manager", "ControlGroup", timeout=5))
        self.manager = dbus.Interface(manager_object, MANAGER + ".Manager")
        self.matches = [
            self.bus.add_signal_receiver(self._wake, signal_name="PropertiesChanged",
                                         dbus_interface="org.freedesktop.DBus.Properties", bus_name=self.owner),
            self.bus.add_signal_receiver(self._wake, signal_name="JobRemoved",
                                         dbus_interface=MANAGER + ".Manager", bus_name=self.owner),
            self.bus.add_signal_receiver(self._owner_changed, signal_name="NameOwnerChanged",
                                         dbus_interface="org.freedesktop.DBus", arg0=MANAGER)]
        self.bus.call_on_disconnection(self._disconnected)
        self.manager.Subscribe(timeout=5)
        self._check()

    def _wake(self, *_args):
        self.generation += 1
        self.loop.quit()

    def _owner_changed(self, _name, _before, after):
        if str(after) != self.owner:
            self.unknown = True
            self._wake()

    def _disconnected(self, _connection):
        self.unknown = True
        self._wake()

    def _check(self):
        if self.unknown or not self.bus.get_is_connected() or str(self.daemon.GetNameOwner(MANAGER, timeout=5)) != self.owner:
            raise ValueError("Execution manager ownership is unknown")
        verify_cgroup_view(self.manager_pid, self.manager_root)

    def launch(self, name, stdin, stdout, stderr, *, timeout_ms):
        """Start only the fixed namespace guard, with atomic bus-reference hold.

        The name must already be journaled. Successful return does not permit
        GO: the caller must receive READY, capture/verify and persist identity.
        A lost launch reply is unknown, never permission to launch again.
        """
        validate_name(name)
        if name in self.references:
            raise ValueError("Execution-unit names must never be reused")
        if type(timeout_ms) is not int or not 1000 <= timeout_ms <= 600000:
            raise ValueError("Invalid finite task deadline")
        self._check()
        d = self.dbus
        command = namespace.namespace_argv()
        command[0] = "/usr/bin/bwrap"
        properties = [("AddRef", d.Boolean(True)), ("Type", "exec"), ("ExitType", "cgroup"),
                      ("Restart", "no"), ("KillMode", "control-group"), ("SendSIGKILL", d.Boolean(True)),
                      ("OOMPolicy", "kill"), ("MemoryMax", d.UInt64(MEMORY_BYTES)),
                      ("MemorySwapMax", d.UInt64(0)), ("TasksMax", d.UInt64(MAX_TASKS)),
                      ("CPUQuotaPerSecUSec", d.UInt64(1000000)),
                      ("RuntimeMaxUSec", d.UInt64((timeout_ms + 30000) * 1000)),
                      ("TimeoutStopUSec", d.UInt64(2000000)),
                      ("StandardInputFileDescriptor", d.types.UnixFd(stdin)),
                      ("StandardOutputFileDescriptor", d.types.UnixFd(stdout)),
                      ("StandardErrorFileDescriptor", d.types.UnixFd(stderr)),
                      ("ExecStart", d.Array([(command[0], d.Array(command, signature="s"), False)], signature="(sasb)"))]
        # Keep an unresolved exposure even if the method commits but its reply
        # is lost. Such a name cannot be captured for GO or silently released.
        self.references.add(name)
        self.manager.StartTransientUnit(name, "fail", d.Array(properties, signature="(sv)"),
                                        d.Array([], signature="(sa(sv))"), timeout=5)
        self.confirmed.add(name)
        self.launched.add(name)

    def retain(self, identity):
        """Reattach to existing exact metadata; missing/replaced units fail closed."""
        validate_identity(identity)
        self._check()
        if identity["unit"] in self.references and identity["unit"] not in self.confirmed:
            raise ValueError("An ambiguous reference needs a fresh observer connection")
        if identity["unit"] not in self.references:
            self.references.add(identity["unit"])
            self.manager.RefUnit(identity["unit"], timeout=5)
            self.confirmed.add(identity["unit"])
        self._snapshot(identity)

    def _properties(self, name):
        self._check()
        if name not in self.confirmed:
            raise ValueError("Execution unit is not retained by this connection")
        path = self.manager.GetUnit(name, timeout=5)
        properties = self.dbus.Interface(self.bus.get_object(self.owner, path), "org.freedesktop.DBus.Properties")
        before = properties.GetAll(UNIT, timeout=5)
        service = properties.GetAll(SERVICE, timeout=5)
        after = properties.GetAll(UNIT, timeout=5)
        self._check()
        if (str(before["Id"]) != name or str(after["Id"]) != name
                or bytes(before["InvocationID"]) != bytes(after["InvocationID"])):
            raise ValueError("Execution unit changed during observation")
        return after, service

    def _snapshot(self, identity):
        validate_identity(identity)
        if Path("/proc/sys/kernel/random/boot_id").read_text().strip() != identity["boot-id"]:
            raise ValueError("Execution observation belongs to another machine boot")
        unit, service = self._properties(identity["unit"])
        if bytes(unit["InvocationID"]).hex() != identity["invocation-id"]:
            raise ValueError("Execution invocation changed")
        group = str(service["ControlGroup"])
        if group and group != identity["cgroup"]:
            raise ValueError("Execution cgroup changed")
        return unit, service

    def capture(self, name):
        """After guard READY, verify effective kernel bounds before any GO."""
        validate_name(name)
        if name not in self.launched:
            raise ValueError("A lost launch reply never authorizes execution")
        unit, service = self._properties(name)
        identity = validate_identity({"version": 1, "unit": name,
                                      "boot-id": Path("/proc/sys/kernel/random/boot_id").read_text().strip(),
                                      "invocation-id": bytes(unit["InvocationID"]).hex(),
                                      "cgroup": str(service["ControlGroup"])})
        if str(unit["ActiveState"]) != "active" or int(unit["Job"][0]) != 0 or int(service["MainPID"]) <= 0:
            raise ValueError("Execution guard is not in its ready running unit")
        if (str(service["Type"]) != "exec" or str(service["ExitType"]) != "cgroup"
                or str(service["Restart"]) != "no" or str(service["KillMode"]) != "control-group"
                or not bool(service["SendSIGKILL"]) or str(service["OOMPolicy"]) != "kill"
                or not 0 < int(service["RuntimeMaxUSec"]) <= 630000000
                or not 0 < int(service["TimeoutStopUSec"]) <= 2000000):
            raise ValueError("Execution unit shutdown policy differs from its envelope")
        group = Path("/sys/fs/cgroup") / identity["cgroup"].lstrip("/")
        if (not 0 < int((group / "memory.max").read_text()) <= MEMORY_BYTES
                or (group / "memory.swap.max").read_text().strip() != "0"
                or not 0 < int((group / "pids.max").read_text()) <= MAX_TASKS
                or (group / "memory.oom.group").read_text().strip() != "1"):
            raise ValueError("Effective execution memory/process bounds differ")
        quota, period = map(int, (group / "cpu.max").read_text().split())
        if not 0 < quota <= period:
            raise ValueError("Effective execution CPU bound differs")
        actual = Path("/proc", str(int(service["MainPID"])), "cgroup").read_text().splitlines()
        if "0::" + identity["cgroup"] not in actual:
            raise ValueError("Execution process is not in its claimed cgroup")
        self._snapshot(identity)
        return identity

    def stop(self, identity):
        """Request exact-unit stop; return is NOT physical shutdown proof."""
        self._snapshot(identity)
        self.manager.StopUnit(identity["unit"], "fail", timeout=5)

    def wait_stopped(self, identity, *, timeout_seconds=35):
        """Event-driven retained observation; deadline/error never means stopped.

        The returned local observation must be durably sealed before release.
        It is not a server stop acknowledgement or result-import certificate.
        The event-wait deadline has bounded synchronous-call slop: each bus
        property call has a five-second timeout; no call is retried on error.
        """
        if type(timeout_seconds) not in {int, float} or not 0 < timeout_seconds <= 660:
            raise ValueError("Invalid finite observation deadline")
        deadline = time.monotonic() + timeout_seconds
        expired = False

        def expire():
            nonlocal expired
            expired = True
            self.loop.quit()
            return False

        timer = self.GLib.timeout_add(max(1, int(timeout_seconds * 1000)), expire)
        try:
            while True:
                generation = self.generation
                unit, service = self._snapshot(identity)
                if (str(unit["ActiveState"]) in {"inactive", "failed"} and int(unit["Job"][0]) == 0
                        and int(service["MainPID"]) == 0 and cgroup_empty(identity["cgroup"])):
                    final, _ = self._snapshot(identity)
                    if str(final["ActiveState"]) not in {"inactive", "failed"} or int(final["Job"][0]) != 0:
                        raise ValueError("Execution unit changed during shutdown observation")
                    return {"version": 1, "identity": dict(identity), "state": str(final["ActiveState"]),
                            "result": str(service["Result"]), "observed-monotonic-ns": time.monotonic_ns()}
                if expired or time.monotonic() >= deadline:
                    raise TimeoutError("Physical execution shutdown remains unknown")
                # A synchronous bus call may dispatch a wake reentrantly.
                # Never lose it by starting a fresh loop after its quit.
                if generation == self.generation:
                    self.loop.run()
        finally:
            if not expired:
                self.GLib.source_remove(timer)

    def release(self, identity):
        """Caller must first durably seal its exact stopped observation."""
        # Reobserve; caller discipline/journaling is still required. Releasing
        # retained metadata neither clears a lease nor recovers an orphan.
        self.wait_stopped(identity, timeout_seconds=1)
        self.manager.UnrefUnit(identity["unit"], timeout=5)
        self.references.remove(identity["unit"])
        self.confirmed.remove(identity["unit"])
        self.launched.discard(identity["unit"])

    def close(self):
        if self.references:
            raise ValueError("Retained units need sealed shutdown observations before close")
        for match in self.matches:
            match.remove()
        self.bus.close()
