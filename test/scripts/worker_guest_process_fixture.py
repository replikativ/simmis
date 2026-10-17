"""Opt-in cold guest bootstrap test. No login, HTTP, or real model calls.

Run under an external memory/deadline cgroup. This source-classpath regression
does NOT verify the future packaged artifact or production worker controller.
"""

import argparse
import base64
import json
import os
from pathlib import Path
import selectors
import subprocess
import tempfile
import time


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--classpath-file", required=True)
    parser.add_argument("--dvergr-root", required=True)
    parser.add_argument("--input-file", required=True)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    dvergr = Path(args.dvergr_root).resolve()
    jars = [p for p in Path(args.classpath_file).read_text().strip().split(":")
            if p.endswith(".jar")]
    # NEVER inherit dev/user.clj: Clojure.main auto-loads it before -main.
    classpath = ":".join([str(root / "src"), str(root / "resources"),
                          str(dvergr / "src"), str(dvergr / "resources")] + jars)
    frozen = json.loads(Path(args.input_file).read_bytes())
    started = time.monotonic()
    deadline = started + 180
    with tempfile.TemporaryDirectory(prefix="simmis-guest-cold-") as tmp:
        config = Path(tmp) / "config.edn"
        config.write_text("{}")
        env = {"PATH": "/usr/bin:/bin", "LANG": "C.UTF-8", "HOME": tmp,
               "DVERGR_HOME": tmp + "/dvergr", "DVERGR_CONFIG": str(config),
               "CODEX_HOME": tmp + "/codex", "CLAUDE_CONFIG_DIR": tmp + "/claude"}
        command = ["java", "-Xmx512m", "-XX:MaxMetaspaceSize=384m",
                   "-XX:ReservedCodeCacheSize=64m", "-XX:ActiveProcessorCount=2",
                   "-Duser.home=" + tmp, "-cp", classpath, "clojure.main",
                   "-m", "is.simm.ops.worker-process", "guest"]
        # Retain diagnostics even if cold compilation exceeds the test budget.
        # The child has an empty environment/home and fixture-only task data.
        with tempfile.NamedTemporaryFile(prefix="simmis-guest-cold-stderr-", delete=False) as stderr:
            print("Cold guest stderr:", stderr.name, flush=True)
            child = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                     stderr=stderr, cwd=tmp, env=env)
            selector = selectors.DefaultSelector()
            selector.register(child.stdout, selectors.EVENT_READ)
            buffered = bytearray()

            def send(frame):
                child.stdin.write(json.dumps(frame, ensure_ascii=False).encode() + b"\n")
                child.stdin.flush()

            def receive():
                while b"\n" not in buffered:
                    remaining = deadline - time.monotonic()
                    if remaining <= 0 or not selector.select(remaining):
                        raise AssertionError("Guest protocol deadline exceeded")
                    chunk = os.read(child.stdout.fileno(), 16384)
                    assert chunk, "Guest exited before its next protocol frame"
                    buffered.extend(chunk)
                    assert len(buffered) <= 4198400, "Unbounded guest frame"
                frame, _, tail = buffered.partition(b"\n")
                buffered[:] = tail
                return json.loads(frame)

            try:
                send({"version": 1, "kind": "execute", "frozen": frozen})
                ready = receive()
                assert ready["kind"] == "guest-ready"
                assert ready["input-id"] == frozen["input-id"]
                print("Guest ready at", round(time.monotonic() - started, 2), "seconds", flush=True)
                for sequence in (1, 2):
                    request = receive()
                    print("Inference", sequence, "at", round(time.monotonic() - started, 2), "seconds", flush=True)
                    assert request["kind"] == "inference"
                    assert request["request"]["sequence"] == sequence
                    response = {"content": "Binary deliverable ready.", "tool-calls": None,
                                "stop-reason": "end-turn", "usage": {"input-tokens": 1, "output-tokens": 1}}
                    if sequence == 1:
                        code = ('(do (babashka.fs/create-dirs "/out") '
                                '(babashka.fs/copy "/docs/input.bin" "/out/🦋.bin") '
                                '(babashka.fs/delete "/docs/remove.txt"))')
                        response.update({"content": "", "stop-reason": "tool-use",
                                         "tool-calls": [{"id": "copy", "name": "clojure_eval", "input": {"code": code}}]})
                    else:
                        assert any(m.get("type") == "function_call_output" and m.get("call_id") == "copy"
                                   for m in request["request"]["messages"])
                    send({"version": 1, "kind": "inference-result",
                          "reply": {"version": 1, "sequence": sequence, "response": response}})
                result = receive()
                assert result["kind"] == "result" and result["input-id"] == frozen["input-id"]
                print("Guest result at", round(time.monotonic() - started, 2), "seconds", flush=True)
                output = result["output"]
                assert output["exit-code"] == 0
                assert output["summary"] == "Binary deliverable ready."
                assert output["files"]["docs/remove.txt"] is None
                descriptor = output["files"]["out/🦋.bin"]
                assert base64.b64decode(descriptor["base64"]) == bytes([0, 255, 128, 10, 42])
                assert descriptor["mode"] == 0x81A4
                assert set(output["files"]) == {"out/🦋.bin", "docs/remove.txt"}
                child.stdin.close()
                assert child.wait(timeout=max(1, deadline - time.monotonic())) == 0
                assert not buffered and not child.stdout.read(), "Non-protocol/trailing stdout"
                assert not (Path(tmp) / "codex").exists(), "Unexpected login access"
                print("Cold production guest bootstrap: two inference turns, real SCI binary copy/deletion, valid result, clean EOF/exit 0")
            finally:
                selector.close()
                if child.poll() is None:
                    child.kill()
                    child.wait(timeout=5)


if __name__ == "__main__":
    main()
