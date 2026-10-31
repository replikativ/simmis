"""Trusted LOCAL selection of a pinned packaged SCI runtime; no execution.

The descriptor is operator configuration, never room/task input. A SHA256
pins the selected bytes, not their safety or provenance. This is not an OS
sandbox, a physical-stop certificate, or permission to read a native login.
"""
import hashlib
import json
import os
from pathlib import Path
import re
import stat
import zipfile
import uuid


POLICY = {"version": 1, "profile": "dvergr-sci", "max-jar-bytes": 536870912,
          "heap-mib": 512, "metaspace-mib": 256, "direct-mib": 64}
ENTRYPOINT = "is.simm.ops.worker-process"
REQUIRED = {"clojure/main.class", "is/simm/ops/worker_process.clj",
            "is/simm/ops/worker_guest.clj", "is/simm/ops/worker_broker.clj",
            "is/simm/ops/worker_executor.clj", "is/simm/ops/worker_subscription.clj"}


def profile(descriptor, executor_policy_id, inference_policy_id):
    value = {"version": 1, "kind": "dvergr-sci", "runtime": dict(descriptor),
             "bootstrap": ENTRYPOINT, "runtime-policy": dict(POLICY),
             "executor-policy-id": executor_policy_id, "inference-policy-id": inference_policy_id}
    return validate_profile(value)


def validate_profile(value):
    """Pure journal validation: recovery must not need an installed artifact."""
    if (type(value) is not dict
            or set(value) != {"version", "kind", "runtime", "bootstrap", "runtime-policy",
                              "executor-policy-id", "inference-policy-id"}
            or type(value["version"]) is not int or value["version"] != 1
            or value["kind"] != "dvergr-sci" or value["bootstrap"] != ENTRYPOINT
            or json.dumps(value["runtime-policy"], sort_keys=True) != json.dumps(POLICY, sort_keys=True)):
        raise ValueError("Unsupported local SCI execution profile")
    descriptor = value["runtime"]
    if (type(descriptor) is not dict or set(descriptor) != {"jar", "sha256"}
            or not isinstance(descriptor["jar"], str) or not Path(descriptor["jar"]).is_absolute()
            or str(Path(descriptor["jar"])) != descriptor["jar"] or ".." in Path(descriptor["jar"]).parts
            or not isinstance(descriptor["sha256"], str)
            or not re.fullmatch(r"[0-9a-f]{64}", descriptor["sha256"])):
        raise ValueError("Invalid journaled SCI runtime descriptor")
    for field in ["executor-policy-id", "inference-policy-id"]:
        if not isinstance(value[field], str) or str(uuid.UUID(value[field])) != value[field]:
            raise ValueError("Invalid SCI endpoint policy identity")
    return value


def supervisor_argv(selected, codex_home):
    """Only trusted LOCAL code selects this fixed supervisor, never a task."""
    validate_profile(selected)
    verify(selected["runtime"])
    environment("/unused-private-home", codex_home=codex_home)
    return ["/usr/bin/python3", "-I", str(Path(__file__).resolve().with_name("worker_sci.py")),
            json.dumps(selected, sort_keys=True, separators=(",", ":")), codex_home]


def verify(descriptor):
    """Verify a closed operator descriptor before allocation or credentials.

    Runtime installation and its parent directories remain trusted local
    administration. Do not install a runtime in the candidate's workspace.
    """
    if (type(descriptor) is not dict or set(descriptor) != {"jar", "sha256"}
            or not isinstance(descriptor["jar"], str)
            or not isinstance(descriptor["sha256"], str)
            or not re.fullmatch(r"[0-9a-f]{64}", descriptor["sha256"])):
        raise ValueError("Expected a pinned local SCI runtime descriptor")
    path = Path(descriptor["jar"])
    if not path.is_absolute() or str(path.resolve(strict=True)) != descriptor["jar"]:
        raise ValueError("Runtime must be an absolute canonical local file")
    fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
    with os.fdopen(fd, "rb") as stream:
        info = os.fstat(stream.fileno())
        if (not stat.S_ISREG(info.st_mode) or info.st_uid not in {0, os.getuid()}
                or info.st_mode & 0o022 or not 0 < info.st_size <= POLICY["max-jar-bytes"]):
            raise ValueError("Runtime is not a bounded trusted local regular file")
        # Hash the entire trusted descriptor with bounded memory using APIs
        # available on system Python 3.10 as well as newer interpreters.
        digest = hashlib.sha256()
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
        if digest.hexdigest() != descriptor["sha256"]:
            raise ValueError("Local SCI runtime digest differs")
        stream.seek(0)
        with zipfile.ZipFile(stream) as jar:
            names = jar.namelist()
            if len(names) != len(set(names)) or not REQUIRED.issubset(names):
                raise ValueError("Runtime bootstrap inventory differs")
            if {"user.clj", "user.cljc", "user__init.class"}.intersection(names):
                raise ValueError("Runtime contains ambient Clojure startup code")
            if "META-INF/MANIFEST.MF" in names:
                header = jar.getinfo("META-INF/MANIFEST.MF")
                if header.file_size > 65536:
                    raise ValueError("Runtime manifest exceeds its bound")
                manifest = jar.read(header).decode("utf-8")
                if any(line.lower().startswith("class-path:") for line in manifest.splitlines()):
                    raise ValueError("Runtime declares an ambient classpath")
        after = os.fstat(stream.fileno())
        if (info.st_size, info.st_mtime_ns, info.st_ctime_ns) != (after.st_size, after.st_mtime_ns, after.st_ctime_ns):
            raise ValueError("Runtime changed during verification")
    return dict(descriptor)


def argv(descriptor, mode, private_home):
    """Fixed bootstrap only: no task argv, aliases, JVM options or extra paths.

    Caller supplies a fresh private home and verifies again immediately before
    launch. The containing unit, not these flags, imposes physical limits.
    """
    if mode not in {"check", "guest", "broker"}:
        raise ValueError("Unknown local SCI runtime mode")
    if (not isinstance(private_home, str) or not Path(private_home).is_absolute()
            or str(Path(private_home).resolve(strict=True)) != private_home):
        raise ValueError("Expected a canonical private runtime home")
    checked = verify(descriptor)
    return ["/usr/bin/java", "-Xms32m", "-Xmx" + str(POLICY["heap-mib"]) + "m",
            "-XX:MaxMetaspaceSize=" + str(POLICY["metaspace-mib"]) + "m",
            "-XX:MaxDirectMemorySize=" + str(POLICY["direct-mib"]) + "m",
            "-XX:ActiveProcessorCount=1", "-XX:+ExitOnOutOfMemoryError",
            "-Duser.home=" + private_home, "-cp", checked["jar"],
            "clojure.main", "-m", ENTRYPOINT, mode]


def environment(private_home, *, codex_home=None):
    """Empty-by-construction child environment. Broker alone gets login path.

    No PATH-selected wrappers, Java injection flags, API keys, startup hooks,
    proxies or controller credential variables are inherited. SCI itself is
    not filesystem isolation against an interpreter/JVM escape.
    """
    if not isinstance(private_home, str) or not Path(private_home).is_absolute():
        raise ValueError("Expected a private runtime home")
    env = {"PATH": "/usr/bin:/bin", "HOME": private_home,
           "DVERGR_HOME": str(Path(private_home) / "dvergr"),
           "CODEX_HOME": str(Path(private_home) / "codex"),
           "CLAUDE_CONFIG_DIR": str(Path(private_home) / "claude"),
           "TMPDIR": private_home}
    if codex_home is not None:
        if (not isinstance(codex_home, str) or not Path(codex_home).is_absolute()
                or str(Path(codex_home).resolve(strict=True)) != codex_home):
            raise ValueError("Expected a canonical broker-only Codex directory")
        env["CODEX_HOME"] = codex_home
    return env
