"""Fixed trusted local SCI supervisor, run INSIDE the retained worker unit.

Both JVMs are ordinary children in the SAME cgroup. This is not a namespace
sandbox. No lease/MCP credential, room lookup, native tool argv or host file
export enters here. Output is untrusted data; only the outer observer proves
whole-unit physical shutdown. Never invoke this as an unaccounted model runner.
"""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time

sys.path.insert(0, str(Path(__file__).resolve().parent))
import worker_inputs as inputs
import worker_outputs as outputs
import worker_runtime as runtime
from worker_io import Frames


def encoded(value):
    return json.dumps(value, separators=(",", ":"), allow_nan=False).encode("utf-8") + b"\n"


def request(stream, expected_profile):
    raw = stream.readline(inputs.POLICY["max-input-json-bytes"] + 16385)
    if not raw.endswith(b"\n") or len(raw) > inputs.POLICY["max-input-json-bytes"] + 16384:
        raise ValueError("Invalid bounded SCI opening")
    value = json.loads(raw.decode("utf-8"), object_pairs_hook=inputs._unique_object,
                       parse_constant=inputs._invalid_constant)
    inputs._closed(value, {"frozen", "config", "execution-profile"})
    inputs.validate(value["frozen"], value["config"])
    runtime.validate_profile(value["execution-profile"])
    if value["execution-profile"] != expected_profile or value["config"]["adapter"] != "codex":
        raise ValueError("SCI local configuration differs")
    return value


def start(profile, mode, home, codex_home=None):
    child = subprocess.Popen(runtime.argv(profile["runtime"], mode, home), cwd=home,
                             env=runtime.environment(home, codex_home=codex_home),
                             stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                             close_fds=True)
    try:
        # Frames owns only these duplicates. Popen retains process ownership,
        # not additional pipe writers which could accidentally prevent EOF.
        channel = Frames(os.dup(child.stdin.fileno()), os.dup(child.stdout.fileno()))
    except BaseException:
        child.kill()
        child.wait(timeout=5)
        raise
    finally:
        child.stdin.close()
        child.stdout.close()
    return child, channel


def close_writer(channel):
    if channel.writer is not None:
        os.close(channel.writer)
        channel.writer = None


def finish(child, channel, deadline):
    close_writer(channel)
    if child.wait(timeout=max(0.001, deadline - time.monotonic())) != 0:
        raise ValueError("SCI endpoint failed")


def execute(profile, frozen, codex_home):
    """Called only after the controller's durable physical admission and GO.

    Direct-child waits are protocol cleanup, NOT a whole-unit stop certificate.
    The outer systemd observer independently retains/kills every descendant.
    """
    deadline = time.monotonic() + frozen["limits"]["timeout-ms"] / 1000
    children = []
    with tempfile.TemporaryDirectory(prefix="simmis-sci-") as root:
        try:
            homes = [str(Path(root) / name) for name in ["guest", "broker"]]
            for home in homes:
                Path(home).mkdir(mode=0o700)
            broker, b = start(profile, "broker", homes[1], codex_home)
            children.append((broker, b))
            guest, g = start(profile, "guest", homes[0])
            children.append((guest, g))
            b.write(encoded({"version": 1, "kind": "configure", "config": frozen["config"]}), deadline)
            ready = b.read(deadline)
            inputs._closed(ready, {"version", "kind", "config-id", "inference-policy-id"})
            # This trusted parent supplied the sole config opening. Its hash
            # is diagnostic metadata, not guest-provided executor attestation.
            inputs.canonical_uuid(ready["config-id"])
            if (type(ready["version"]) is not int or ready["version"] != 1
                    or ready["kind"] != "broker-ready"
                    or ready["inference-policy-id"] != profile["inference-policy-id"]):
                raise ValueError("SCI broker policy differs")
            g.write(encoded({"version": 1, "kind": "execute", "frozen": frozen}), deadline)
            if g.read(deadline) != {"version": 1, "kind": "guest-ready", "input-id": frozen["input-id"],
                                    "executor-policy-id": profile["executor-policy-id"],
                                    "inference-policy-id": profile["inference-policy-id"]}:
                raise ValueError("SCI guest policy differs")
            calls = 0
            while True:
                frame = g.read(deadline)
                if frame.get("kind") == "inference":
                    inputs._closed(frame, {"version", "kind", "request"})
                    if type(frame["version"]) is not int or frame["version"] != 1 or calls >= 32:
                        raise ValueError("SCI inference envelope differs")
                    calls += 1
                    b.write(encoded(frame), deadline)
                    reply = b.read(deadline)
                    inputs._closed(reply, {"version", "kind", "reply"})
                    if type(reply["version"]) is not int or reply["version"] != 1 or reply["kind"] != "inference-result":
                        raise ValueError("SCI broker reply differs")
                    g.write(encoded(reply), deadline)
                else:
                    inputs._closed(frame, {"version", "kind", "input-id", "output"})
                    if (type(frame["version"]) is not int or frame["version"] != 1
                            or frame["kind"] != "result" or frame["input-id"] != frozen["input-id"]):
                        raise ValueError("SCI result differs")
                    output = outputs.validate(frozen, frozen["config"], frame["output"])
                    finish(guest, g, deadline)
                    b.write(encoded({"version": 1, "kind": "close"}), deadline)
                    finish(broker, b, deadline)
                    return output
        finally:
            for child, channel in children:
                if child.poll() is None:
                    child.kill()
            for child, channel in children:
                try:
                    child.wait(timeout=5)
                finally:
                    channel.close()


def main(profile, codex_home):
    runtime.validate_profile(profile)
    runtime.verify(profile["runtime"])
    runtime.environment("/unused-private-home", codex_home=codex_home)  # validate only; do not read login
    # Refuse accidental unaccounted execution outside a uniquely named unit.
    import worker_unit as unit
    memberships = [line[3:] for line in Path("/proc/self/cgroup").read_text().splitlines() if line.startswith("0::")]
    if len(memberships) != 1 or not memberships[0].startswith("/user.slice/"):
        raise ValueError("SCI supervisor needs its owned execution unit")
    unit.validate_name(Path(memberships[0]).name)
    opening = request(sys.stdin.buffer, profile)
    frozen = opening["frozen"]
    sys.stdout.buffer.write(encoded({"kind": "ready", "execution-profile": profile, "input-id": frozen["input-id"]}))
    sys.stdout.buffer.flush()
    if sys.stdin.buffer.readline(4) != b"GO\n":
        return
    output = execute(profile, frozen, codex_home)
    sys.stdout.buffer.write(encoded({"kind": "complete", "input-id": frozen["input-id"], "output": output}))
    sys.stdout.buffer.flush()


if __name__ == "__main__":
    try:
        if len(sys.argv) != 3:
            raise ValueError("Invalid supervisor invocation")
        main(json.loads(sys.argv[1]), sys.argv[2])
    except BaseException:
        # No raw native error, prompt, credentials or model response in logs.
        sys.stderr.write("Local SCI supervisor failed\n")
        sys.exit(1)
