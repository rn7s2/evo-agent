#!/usr/bin/env python3
"""serve-e2e.py — drive `build/evo serve --no-userspace` over HTTP only.

Backend-free: the model is tests/stub-messages.py, registered at runtime
through POST /eval, so this needs nothing but python3 and a built binary.
Everything evo does is asked of it over HTTP, exactly as a coordinator would;
the stub's own request log (GET /_requests on the stub) is how the test sees
what evo sent to the "model".

Covers: bad tokens rejected; model + provider registered by eval; a prompt
streamed to its settled end; interrupt; steer; a 409 for a journal switch
while busy; /compact; goal create/refine/pause/resume/complete; model and
thinking switch; a tool registered by eval offered on the next turn; fork,
new, resume; state, transcript, journal, lore, sessions, registry (no
secrets); event replay by Last-Event-ID; clean shutdown with :session-end
fired and exit 0 — under the supervisor, as `evo serve` normally runs.

Usage: tests/serve-e2e.py [path/to/evo]      (exit 0 on success)
"""

import http.client
import json
import os
import shutil
import socket
import subprocess
import sys
import tempfile
import time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EVO = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else os.path.join(ROOT, "build", "evo")
SECRET = "e2e-secret-api-key-never-shown"

passed = 0
failed = 0


def check(name, ok, detail=""):
    global passed, failed
    if ok:
        passed += 1
        print(f"ok   {name}")
    else:
        failed += 1
        print(f"FAIL {name} {detail}")


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


class Evo:
    def __init__(self, port, token):
        self.port = port
        self.token = token

    def request(self, method, path, body=None, token=None, headers=None, timeout=60):
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=timeout)
        h = {"Authorization": f"Bearer {self.token if token is None else token}"}
        if token == "":
            h = {}
        h.update(headers or {})
        data = None
        if body is not None:
            data = json.dumps(body).encode()
            h["Content-Type"] = "application/json"
        conn.request(method, path, body=data, headers=h)
        resp = conn.getresponse()
        raw = resp.read().decode()
        conn.close()
        try:
            return resp.status, json.loads(raw)
        except ValueError:
            return resp.status, raw

    def get(self, path, **kw):
        return self.request("GET", path, **kw)

    def post(self, path, body=None, **kw):
        return self.request("POST", path, body if body is not None else {}, **kw)

    def command(self, text):
        return self.post("/command", {"text": text})

    def stream(self, method, path, body=None, headers=None, timeout=60):
        """Open an SSE response and yield (id, event, data) until it closes."""
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=timeout)
        h = {"Authorization": f"Bearer {self.token}"}
        h.update(headers or {})
        data = None
        if body is not None:
            data = json.dumps(body).encode()
            h["Content-Type"] = "application/json"
        conn.request(method, path, body=data, headers=h)
        resp = conn.getresponse()
        assert resp.status == 200, resp.status
        event = {"id": None, "event": None, "data": None}
        while True:
            line = resp.fp.readline()
            if not line:
                break
            line = line.decode().rstrip("\n")
            if line == "":
                if event["event"]:
                    yield (event["id"], event["event"], json.loads(event["data"]))
                event = {"id": None, "event": None, "data": None}
            elif line.startswith(":"):
                continue
            else:
                key, _, value = line.partition(": ")
                event[key] = int(value) if key == "id" else value
        conn.close()

    def stream_command(self, path, body):
        """POST with "stream": true; return the result and the events after it."""
        events = list(self.stream("POST", path, dict(body, stream=True)))
        assert events and events[0][1] == "result", events[:1]
        return events[0][2], events[1:]

    def wait_settled(self, cursor, timeout=60):
        """Events after CURSOR up to and including the next `settled`."""
        seen = []
        for eid, etype, data in self.stream("GET", f"/events?since={cursor}", timeout=timeout):
            seen.append((eid, etype, data))
            if etype == "settled":
                break
        return seen

    def wait_for(self, predicate, timeout=30):
        deadline = time.time() + timeout
        while time.time() < deadline:
            status, state = self.get("/state")
            if status == 200 and predicate(state):
                return state
            time.sleep(0.1)
        return None


def stub_requests(stub_port):
    conn = http.client.HTTPConnection("127.0.0.1", stub_port, timeout=10)
    conn.request("GET", "/_requests")
    data = json.loads(conn.getresponse().read())
    conn.close()
    return data


def types(events):
    return [e[1] for e in events]


def main():
    global failed
    if not os.access(EVO, os.X_OK):
        print(f"serve-e2e: no binary at {EVO} (make build first)")
        return 1
    work = tempfile.mkdtemp(prefix="evo-serve-e2e-")
    home = os.path.join(work, "home")
    proj = os.path.join(work, "proj")
    os.makedirs(home)
    os.makedirs(proj)
    token_file = os.path.join(work, "token")
    ended_file = os.path.join(work, "session-ended")
    stub_port = free_port()
    port = free_port()

    stub = subprocess.Popen([sys.executable, os.path.join(ROOT, "tests", "stub-messages.py"),
                             str(stub_port)], stdout=subprocess.PIPE, text=True)
    assert stub.stdout.readline().startswith("stub listening")

    env = dict(os.environ, EVO_HOME=home)
    for var in ("EVO_SERVE_TOKEN", "EVO_SUPERVISED_CHILD", "EVO_NO_SUPERVISOR",
                "ANTHROPIC_API_KEY"):
        env.pop(var, None)
    log = open(os.path.join(work, "evo.log"), "w")
    # Plain invocation: the supervisor parent spawns the session child.
    evo_proc = subprocess.Popen([EVO, "serve", "--no-userspace", "--port", str(port),
                                 "--token-file", token_file],
                                cwd=proj, env=env, stdout=log, stderr=subprocess.STDOUT)
    try:
        deadline = time.time() + 60
        while not os.path.exists(token_file) and time.time() < deadline:
            time.sleep(0.1)
        check("token file written", os.path.exists(token_file))
        if not os.path.exists(token_file):
            raise SystemExit("serve-e2e: evo never came up")
        mode = os.stat(token_file).st_mode & 0o777
        check("token file is private (0600)", mode == 0o600, oct(mode))
        token = open(token_file).read().strip()
        evo = Evo(port, token)
        # The listener may come up a moment after the file.
        health, status = None, None
        for _ in range(100):
            try:
                status, health = evo.get("/health")
                if status == 200:
                    break
            except OSError:
                pass
            time.sleep(0.1)
        check("health: the program's identity, ok, pid and cursor",
              status == 200 and isinstance(health, dict) and health.get("ok")
              and isinstance(health.get("pid"), int)
              and isinstance(health.get("cursor"), int)
              and health.get("name") == "evo" and health.get("version")
              and health.get("features") == [], health)

        run_all(evo, stub_port, work, ended_file)

        # --- shutdown -----------------------------------------------------
        status, reply = evo.post("/shutdown")
        check("shutdown accepted", status == 200 and reply["ok"], reply)
        try:
            code = evo_proc.wait(timeout=30)
        except subprocess.TimeoutExpired:
            code = None
        check("evo exits 0 after shutdown (supervisor included)", code == 0, code)
        check(":session-end fired on shutdown", os.path.exists(ended_file))
        check("token file removed on clean exit", not os.path.exists(token_file))
    except BaseException as e:
        failed += 1
        print(f"FAIL aborted: {e!r}")
    finally:
        if evo_proc.poll() is None:
            evo_proc.kill()
        stub.kill()
        log.close()
    if failed:
        print(open(os.path.join(work, "evo.log")).read()[-4000:])
    else:
        shutil.rmtree(work, ignore_errors=True)
    print(f"\nserve-e2e: {passed} passed, {failed} failed")
    return 1 if failed else 0


def run_all(evo, stub_port, work, ended_file):
    # --- auth ---------------------------------------------------------------
    check("no token -> 401", evo.get("/state", token="")[0] == 401)
    check("wrong token -> 401", evo.get("/state", token="not-the-token")[0] == 401)
    check("wrong token on a command -> 401",
          evo.post("/prompt", {"text": "x"}, token="nope")[0] == 401)
    check("unknown endpoint -> 404", evo.get("/nope")[0] == 404)
    check("wrong method -> 405", evo.get("/prompt")[0] == 405)
    conn = http.client.HTTPConnection("127.0.0.1", evo.port, timeout=10)
    conn.request("POST", "/command", body=b"{not json",
                 headers={"Authorization": f"Bearer {evo.token}"})
    check("bad JSON -> 400", conn.getresponse().status == 400)
    conn.close()

    status, state = evo.get("/state")
    check("state before any model: idle, no model", status == 200
          and state["status"] == "idle" and state["model_ready"] is None, state)

    # --- register a provider and models via eval ------------------------------
    form = ("(progn"
            f" (evo:register-provider :stub :base-url \"http://127.0.0.1:{stub_port}\""
            f"   :api-key \"{SECRET}\")"
            " (evo:register-model \"stub-a\" :provider :stub :context-window 200000"
            "   :max-output 8000 :effort t)"
            " (evo:register-model \"stub-b\" :provider :stub :context-window 100000"
            "   :max-output 8000 :effort t)"
            " (evo:set-setting :model \"stub-a\")"
            f" (evo:on :session-end (lambda (p) (declare (ignore p))"
            f"   (with-open-file (o \"{ended_file}\" :direction :output"
            "     :if-exists :supersede) (write-line \"ended\" o))) :name :e2e-end)"
            " :registered)")
    status, reply = evo.post("/eval", {"form": form})
    check("eval form registers provider + models", status == 200
          and reply["data"]["values"] == [":registered"], reply)
    status, reply = evo.post("/eval", {"code": "(defun e2e-f () 41) (1+ (e2e-f))"})
    check("eval body (the tool's form) returns the last value",
          status == 200 and reply["data"]["values"] == ["42"], reply)
    status, reply = evo.post("/eval", {"form": "(error \"boom\")"})
    check("a failing eval is 422 with the condition", status == 422
          and "boom" in reply["error"], reply)
    status, reply = evo.post("/eval", {"form": "(+ 1 2) (+ 3 4)"})
    check("eval form refuses two forms (400)", status == 400, reply)
    status, state = evo.get("/state")
    check("model now resolves", state["model"] == "stub-a" and state["model_ready"], state)

    # --- a prompt streamed to the end of its run --------------------------------
    result, events = evo.stream_command("/prompt", {"text": "hello e2e"})
    check("prompt stream opens with its result", result["ok"] and result["task"], result)
    t = types(events)
    check("prompt stream carries the run",
          "task-start" in t and "text-delta" in t and "run-end" in t, t)
    check("prompt stream ends when the session settles", t and t[-1] == "settled", t)
    text = "".join(d["text"] for _, e, d in events if e == "text-delta")
    check("the stub's answer streamed back", text == "ok: hello e2e", text)
    ids = [i for i, _, _ in events]
    check("event ids are consecutive", ids == list(range(ids[0], ids[0] + len(ids))), ids)
    status, tr = evo.get("/transcript")
    msgs = tr["messages"]
    check("transcript has the exchange", len(msgs) == 2 and msgs[0]["role"] == "user"
          and msgs[1]["content"][0]["text"] == "ok: hello e2e", msgs)

    # Last-Event-ID replays what came after it.
    first = ids[0]
    replay = []
    for ev in evo.stream("GET", "/events", headers={"Last-Event-ID": str(first)}, timeout=5):
        replay.append(ev)
        if len(replay) == 2:
            break
    check("Last-Event-ID resumes right after the given id",
          [r[0] for r in replay] == [first + 1, first + 2], replay)

    # --- model and thinking switch -------------------------------------------------
    status, reply = evo.command("/model stub-b")
    check("/model <id> switches", status == 200 and reply["data"]["model"]["id"] == "stub-b", reply)
    status, reply = evo.command("/model")
    check("/model with no id lists the choices", status == 200
          and [i["value"]["id"] for i in reply["choices"]["items"]] == ["stub-a", "stub-b"], reply)
    status, reply = evo.command("/model no-such-model")
    check("/model with an unknown id is 400", status == 400, reply)
    status, reply = evo.command("/thinking high")
    check("/thinking switches", status == 200 and reply["data"]["thinking"] == "high", reply)
    status, reply = evo.command("/thinking sideways")
    check("/thinking with a bad level is 400", status == 400, reply)
    status, state = evo.get("/state")
    check("state shows the new model and thinking",
          state["model"] == "stub-b" and state["thinking"] == "high", state)
    evo.stream_command("/prompt", {"text": "after switch"})
    last = stub_requests(stub_port)[-1]
    check("the next turn runs on the new model", last["model"] == "stub-b", last)
    check("the next turn carries the new effort", last["effort"] == "high", last)

    # --- a tool registered by eval is offered the very next turn ------------------
    code = ("(evo:register-tool \"e2e_probe\" :description \"e2e probe tool\""
            " :schema '(:object) :execute (lambda (args) (declare (ignore args))"
            " \"probe-result-7\"))")
    status, reply = evo.post("/eval", {"code": code})
    check("eval registers a tool", status == 200, reply)
    before = len(stub_requests(stub_port))
    result, events = evo.stream_command("/prompt", {"text": "please TOOL:e2e_probe now"})
    reqs = stub_requests(stub_port)[before:]
    check("the new tool is offered on the next turn",
          reqs and "e2e_probe" in reqs[0]["tools"], reqs[:1])
    results = [d for _, e, d in events if e == "tool-result"]
    check("the model's call ran the new tool",
          results and results[0]["name"] == "e2e_probe"
          and "probe-result-7" in results[0]["content"], results)

    # --- steer ---------------------------------------------------------------------
    status, reply = evo.post("/steer", {"text": "nothing runs"})
    check("steer with no run is 409", status == 409, reply)
    status, reply = evo.post("/prompt", {"text": "SLOW for steering"})
    check("a slow prompt starts a run", status == 200 and reply["task"], reply)
    cursor = reply["cursor"]
    status, reply = evo.post("/steer", {"text": "steered-in mid run"})
    check("steer during a run is accepted", status == 200, reply)
    status, reply = evo.command("/new")
    check("a journal switch while busy is 409, not a race", status == 409, reply)
    status, reply = evo.command("/compact")
    check("/compact while busy is 409", status == 409, reply)
    events = evo.wait_settled(cursor)
    steering = [d["text"] for _, e, d in events if e == "steering"]
    check("the steer landed at a turn boundary of the same run",
          "steered-in mid run" in steering, steering)
    status, tr = evo.get("/transcript")
    answers = [m["content"][0]["text"] for m in tr["messages"]
               if m["role"] == "assistant" and m["content"] and m["content"][0]["type"] == "text"]
    check("the model answered the steer", "ok: steered-in mid run" in answers, answers[-3:])

    # --- interrupt -----------------------------------------------------------------
    status, reply = evo.post("/interrupt")
    check("interrupt with nothing running says so", status == 200
          and reply["data"]["interrupted"] is None, reply)
    status, reply = evo.post("/prompt", {"text": "SLOW to interrupt"})
    cursor = reply["cursor"]
    # Wait until the model is actually talking, then interrupt it.
    for eid, etype, data in evo.stream("GET", f"/events?since={cursor}", timeout=30):
        if etype == "text-delta":
            break
    result, events = evo.stream_command("/interrupt", {})
    check("interrupt answers interrupted", result["data"]["interrupted"] is True, result)
    run_ends = [d for _, e, d in events if e == "run-end"]
    check("the run ends aborted", run_ends and run_ends[-1]["outcome"] == "aborted", events)
    check("interrupt's stream ends settled", types(events)[-1:] == ["settled"], types(events))
    state = evo.wait_for(lambda s: s["status"] == "idle")
    check("idle after the interrupt", state is not None)

    # --- /compact --------------------------------------------------------------------
    evo.post("/eval", {"form": "(setf evo.kernel::*compact-keep-recent-tokens* 1)"})
    result, events = evo.stream_command("/command", {"text": "/compact keep the probe"})
    outputs = [d["text"] for _, e, d in events if e == "output"]
    check("/compact runs as a task and succeeds", "✓ compacted" in outputs, outputs)
    status, journal = evo.get("/journal")
    check("a :compaction entry is on the path",
          any(e["type"] == "compaction" for e in journal["entries"]))
    summarizer = [r for r in stub_requests(stub_port) if r["summarizer"]]
    check("the summarizer request reached the model", len(summarizer) >= 1)

    # --- goal create / refine / pause / resume / complete ------------------------------
    status, reply = evo.command("/goal")
    check("/goal with no goal says so", status == 200 and reply["data"]["goal"] is None, reply)
    status, reply = evo.command("/goal alpha objective")
    check("/goal creates the goal and starts driving it", status == 200
          and reply["data"]["goal"]["status"] == "active" and reply["task"], reply)
    status, reply = evo.command("/goal beta objective")
    check("/goal on an active goal refines it",
          reply["data"]["goal"]["objective"] == "beta objective", reply)
    status, reply = evo.command("/goal pause")
    check("/goal pause pauses it", reply["data"]["goal"]["status"] == "paused", reply)
    state = evo.wait_for(lambda s: s["status"] == "idle", timeout=30)
    check("a paused goal stops driving", state is not None
          and state["goal"]["status"] == "paused", state and state["goal"])
    status, reply = evo.command("/goal pause")
    check("pausing a paused goal is 409", status == 409, reply)
    status, reply = evo.command("/goal resume")
    check("/goal resume resumes it", status == 200
          and reply["data"]["goal"]["status"] == "active", reply)
    cursor = reply["cursor"]
    status, reply = evo.command("/goal beta objective FINISH")
    check("refine again while it runs", status == 200, reply)
    evo.wait_for(lambda s: s["goal"] and s["goal"]["status"] == "complete"
                 and s["status"] == "idle", timeout=60)
    status, state = evo.get("/state")
    check("the model completed the goal through update_goal",
          state["goal"]["status"] == "complete", state["goal"])

    # --- lore, export, tree, rewind ---------------------------------------------------------
    status, reply = evo.command("/lore prefer small commits")
    check("/lore adds project lore", status == 200 and reply["data"]["id"], reply)
    status, lore = evo.get("/lore")
    check("GET /lore lists it", any(e["text"] == "prefer small commits" for e in lore["entries"]))
    export_path = os.path.join(work, "export.md")
    status, reply = evo.command(f"/export {export_path}")
    check("/export writes the transcript", status == 200 and os.path.exists(export_path), reply)
    status, reply = evo.command("/rewind")
    check("/rewind hands the last user text back as a draft",
          status == 200 and reply["data"].get("draft"), reply)
    status, reply = evo.command("/tree")
    check("/tree lists the path's entries",
          status == 200 and reply["choices"] and reply["choices"]["items"], reply)

    # --- fork / new / resume ---------------------------------------------------------------
    status, state = evo.get("/state")
    original = state["session"]
    status, reply = evo.command("/fork")
    forked = reply["data"].get("session")
    check("/fork switches to a new file", status == 200 and forked and forked != original, reply)
    status, reply = evo.command("/new")
    fresh = reply["data"].get("session")
    check("/new starts a fresh session", status == 200 and fresh not in (original, forked), reply)
    status, tr = evo.get("/transcript")
    check("a new session has an empty transcript", tr["messages"] == [], tr)
    status, sessions = evo.get("/sessions")
    # The list resolves each path (truename) while a command reply keeps the
    # journal path as spelled — on macOS /var is a symlink to /private/var, so
    # compare what the filesystem says the two names are.
    paths = [os.path.realpath(s["path"]) for s in sessions["sessions"]]
    check("GET /sessions lists the sessions on disk",
          os.path.realpath(original) in paths and os.path.realpath(forked) in paths, paths)
    status, reply = evo.command(f"/resume {original}")
    check("/resume <path> switches back", status == 200
          and reply["data"].get("session") == original, reply)
    status, tr = evo.get("/transcript")
    check("the resumed session's history is back",
          any(m["role"] == "user" and "hello e2e" in json.dumps(m) for m in tr["messages"])
          or any("SUMMARY" in json.dumps(m) for m in tr["messages"]), len(tr["messages"]))
    status, reply = evo.command("/resume 99")
    check("/resume <n> past the list is 404", status == 404, reply)
    status, reply = evo.command("/nonsense-command")
    check("an unknown command is 404", status == 404, reply)

    # --- state, registry --------------------------------------------------------------------
    status, state = evo.get("/state")
    check("state has the status fields", all(k in state for k in (
        "status", "task", "model", "thinking", "context_tokens", "goal", "todos",
        "jobs", "session", "cursor")), sorted(state))
    status, raw = evo.request("GET", "/registry")
    text = json.dumps(raw)
    check("registry lists models, tools and commands",
          "stub-a" in text and "e2e_probe" in text and "compact" in text)
    check("registry shows no secrets", SECRET not in text)
    stub_provider = [p for p in raw["providers"] if p["key"] == "stub"]
    check("a provider says it has a key without showing it",
          stub_provider and stub_provider[0]["has_api_key"] is True, stub_provider)


if __name__ == "__main__":
    sys.exit(main())
