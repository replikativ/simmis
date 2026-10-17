"""Scripted PIPE fixture, not a model provider or deployable worker runtime."""
import json
import sys

mode, count, bad, executor_id, inference_id = sys.argv[1:]


def send(value):
    print(json.dumps(value), flush=True)


def read():
    return json.loads(sys.stdin.readline())


opening = read()
if mode == "broker":
    assert opening["kind"] == "configure"
    send({"version": 1, "kind": "broker-ready", "config-id": "11111111-1111-4111-8111-111111111111",
          "inference-policy-id": inference_id})
    while True:
        frame = read()
        if frame["kind"] == "close":
            assert sys.stdin.read() == ""
            break
        assert frame["kind"] == "inference"
        send({"version": 1, "kind": "inference-result", "reply": {"content": "fixture relay answer"}})
else:
    frozen = opening["frozen"]
    assert opening["kind"] == "execute"
    send({"version": 1, "kind": "guest-ready", "input-id": frozen["input-id"],
          "executor-policy-id": "wrong" if bad == "1" else executor_id, "inference-policy-id": inference_id})
    for sequence in range(int(count)):
        send({"version": 1, "kind": "inference", "request": {"sequence": sequence + 1}})
        answer = read()
        assert answer["kind"] == "inference-result"
    send({"version": 1, "kind": "result", "input-id": frozen["input-id"],
          "output": {"files": {}, "summary": "fixture relay answer", "exit-code": 0}})
    assert sys.stdin.read() == ""
