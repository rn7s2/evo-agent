#!/usr/bin/env python3
"""serve-e2e.py — drive `build/evo-agent serve --no-userspace` over HTTP only.

Backend-free: the model is tests/stub-messages.py, registered at runtime
through the `eval` op, so this needs nothing but python3 and a built binary.
Everything evo does is asked of it over HTTP, exactly as a GUI or a
coordinator would; the stub's own request log (GET /_requests on the stub) is
how the test sees what evo sent to the "model".

Covers the redesign's protocol (CONTRACT §5):
  * the ready file: port, token, epoch, session, mode 0600, deleted on exit
  * auth and transport: 401, 400 (bad JSON envelope), 404, 405
  * GET /health without touching the session thread
  * GET /snapshot: one seq across topics, and /items paging
  * GET /stream: hello first, item ops, coalesced item.append, topic filter
  * snapshot + stream consistency: applying the ops to a snapshot equals the
    next snapshot
  * reconnecting with a cursor: the ops that follow it, exactly
  * input.send / input.cancel, with a queued item a client can cancel
  * ops idempotency by rid (a retry does nothing twice)
  * error codes: unknown_op, invalid_args, not_quiescent, not_found, busy
  * GET /catalog, GET /sessions, GET /debug/context, GET /debug/journal
  * stream.reset after a restart: the epoch changed, so a cursor from the old
    process gets hello then stream.reset{restarted}
  * server.shutdown: exit 0, ready file removed
  * an idle server spends no CPU, and --watch-stdin ends it when its input does
    — before a turn and after one, with a stream open
  * GET /catalog on a live evo-swarm carries `lanes.models[]`, from the hook
    serve defines and swarm/main.lisp registers (skipped if it is not built)
  * the small truths: `queued`, has_more, result {}, truncated, usage,
    the default model's provider
  * the caller's environment cannot reach the child: HOME and EVO_HOME are the
    test's, and no supervisor marker is inherited
  * every documented boolean is a boolean (notice.durable, compaction.manual,
    tool result.truncated, state.model.ready, the catalog's flags, ok/queued)

Usage: tests/serve-e2e.py [path/to/evo-agent]    (exit 0 on success)
"""

import http.client
import json
import os
import shutil
import socket
import stat
import subprocess
import sys
import tempfile
import threading
import time
import uuid

from clean_env import clean  # the environment a test's children start from

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EVO = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else os.path.join(ROOT, "build", "evo-agent")
SWARM = os.path.join(ROOT, "build", "evo-swarm")

passed = 0
failed = 0
REPLIES = []            # every op reply the run saw, for the boolean sweep


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


class Server:
    """One `evo serve` child, driven through its ready file."""

    def __init__(self, work, baby=False, binary=None):
        self.work = work
        self.binary = binary or EVO
        self.ready = os.path.join(work, "ready.json")
        self.log_path = os.path.join(work, "evo.log")
        self.baby = baby

    def start(self, args=(), stdin_pipe=False):
        # The test isolates itself completely: its own home for HOME and
        # EVO_HOME, so nothing about the machine running it — the caller's
        # skills, templates, settings — can reach the child.  (A real HOME
        # left a /command scan of the caller's skill directory in the path,
        # which made one call slow enough to trip a serve bug: see below.)
        home = os.path.join(self.work, "home")
        # A test's child must inherit nothing about who ran the test: the
        # supervisor's markers are the dangerous ones — a child that thinks it
        # is a supervised child of somebody else, watching somebody else's pid,
        # in somebody else's sessions directory, with somebody else's token —
        # and so is a provider key, when every model here is a stub.  Only what
        # this test made, and the flags it passes (tests/clean_env.py).
        env = clean(EVO_HOME=home, HOME=home)
        # Set (not inherited): this suite drives the session process itself.
        env["EVO_NO_SUPERVISOR"] = "1"
        if os.path.exists(self.ready):
            os.remove(self.ready)
        log = open(self.log_path, "a")
        # A pipe the test can close is how --watch-stdin is driven: the server
        # is told to end when its input does.
        self.proc = subprocess.Popen(
            [self.binary, "serve", "--no-userspace", "--port", "0",
             "--ready-file", self.ready, *args],
            cwd=os.path.join(self.work, "proj"), env=env,
            stdin=subprocess.PIPE if stdin_pipe else None,
            stdout=log, stderr=subprocess.STDOUT)
        self.piped_stdin = stdin_pipe
        deadline = time.time() + 60
        while not os.path.exists(self.ready) and time.time() < deadline:
            if self.proc.poll() is not None:
                raise SystemExit("serve-e2e: evo exited before writing its ready file")
            time.sleep(0.05)
        if not os.path.exists(self.ready):
            raise SystemExit("serve-e2e: evo never wrote its ready file")
        self.info = json.load(open(self.ready))
        self.port = self.info["port"]
        self.token = self.info["token"]
        return self.info

    def request(self, method, path, body=None, token=None, headers=None, timeout=30,
                raw_body=None):
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=timeout)
        h = {} if token == "" else {"Authorization": f"Bearer {self.token if token is None else token}"}
        h.update(headers or {})
        data = raw_body
        if body is not None:
            data = json.dumps(body).encode()
        if data is not None:
            h["Content-Type"] = "application/json"
        conn.request(method, path, body=data, headers=h)
        resp = conn.getresponse()
        payload = resp.read().decode()
        conn.close()
        try:
            return resp.status, json.loads(payload)
        except ValueError:
            return resp.status, payload

    def get(self, path, **kw):
        return self.request("GET", path, **kw)

    def op(self, name, args=None, rid=None, **kw):
        rid = rid or uuid.uuid4().hex
        status, reply = self.request("POST", "/ops",
                                     {"rid": rid, "op": name, "args": args or {}}, **kw)
        if isinstance(reply, dict):
            REPLIES.append((name, reply))
        return status, reply, rid

    def snapshot(self, topics="session", items=200):
        status, body = self.get(f"/snapshot?topics={topics}&items={items}")
        assert status == 200, (status, body)
        return body

    def stream(self, topics=None, since=None, timeout=30, limit=None):
        """Open the SSE stream and yield (id, event, data) until it closes."""
        path = "/stream"
        params = []
        if topics:
            params.append(f"topics={topics}")
        if since:
            params.append(f"since={since}")
        if params:
            path += "?" + "&".join(params)
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=timeout)
        conn.request("GET", path, headers={"Authorization": f"Bearer {self.token}"})
        resp = conn.getresponse()
        if resp.status != 200:
            conn.close()
            raise AssertionError(f"stream: {resp.status} {resp.read()!r}")
        frame = {"id": None, "event": None, "data": None}
        seen = 0
        try:
            while True:
                line = resp.fp.readline()
                if not line:
                    break
                line = line.decode().rstrip("\n")
                if line == "":
                    if frame["event"]:
                        seen += 1
                        yield (frame["id"], frame["event"], json.loads(frame["data"]))
                        if limit and seen >= limit:
                            break
                    frame = {"id": None, "event": None, "data": None}
                elif line.startswith(":"):
                    continue
                else:
                    key, _, value = line.partition(": ")
                    frame[key] = value
        finally:
            conn.close()

    def read_stream(self, collect, topics=None, since=None, timeout=30):
        """Read the stream until COLLECT(hello, ops) is satisfied; returns the
        hello frame and the ops seen."""
        hello = None
        ops = []
        for eid, event, data in self.stream(topics=topics, since=since, timeout=timeout):
            if event != "op":
                continue
            if data.get("op") == "hello":
                hello = data
                continue
            ops.append(data)
            if collect and collect(ops):
                break
        return hello, ops

    def probe_stream(self, topics=None, seconds=1.5):
        """Open a stream and read it for SECONDS: the frames it carried, and
        nothing more (an unsatisfiable predicate would block until the socket
        times out, which is exactly what a filtered stream looks like)."""
        frames = []
        try:
            for frame in self.stream(topics=topics, timeout=seconds):
                frames.append(frame)
        except (socket.timeout, TimeoutError, OSError):
            pass
        return frames

    def close_stdin(self):
        """The other end of the pipe goes away, as it does when a terminal
        closes or a GUI that spawned the server exits."""
        if self.proc.stdin:
            self.proc.stdin.close()
            self.proc.stdin = None

    def wait_exit(self, timeout=30):
        try:
            return self.proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            return None

    def stop(self):
        if self.proc.poll() is None:
            self.proc.kill()


def cpu_seconds(pid):
    """CPU time the process has burned, in seconds (`ps -o time=`)."""
    out = subprocess.run(["ps", "-o", "time=", "-p", str(pid)],
                         capture_output=True, text=True).stdout.strip()
    total = 0.0
    for part in out.split(":"):
        try:
            total = total * 60 + float(part)
        except ValueError:
            return None
    return total


def register_stub_model(server, stub_port):
    """tests/stub-messages.py, registered as a provider and a model through the
    eval op — the one op that can do this and the reason it is on by default."""
    form = ("(progn"
            f" (evo:register-provider :stub :base-url \"http://127.0.0.1:{stub_port}\""
            "   :api-key \"e2e-secret\")"
            " (evo:register-model \"stub-a\" :provider :stub :context-window 200000"
            "   :max-output 8000 :effort t)"
            " (evo:set-setting :model \"stub-a\")"
            " :registered)")
    return server.op("eval", {"code": form})


def stub_requests(stub_port):
    conn = http.client.HTTPConnection("127.0.0.1", stub_port, timeout=10)
    conn.request("GET", "/_requests")
    data = json.loads(conn.getresponse().read())
    conn.close()
    return data


def apply_ops(items, state, ops):
    """The client's job: fold the op stream onto a snapshot.

    Items are a list in order; a patch is a JSON merge patch (arrays
    replaced whole), an append concatenates, an add inserts."""
    items = [dict(i) for i in items]
    state = dict(state or {})
    for op in ops:
        kind = op.get("op")
        if kind == "item.add":
            item = dict(op["item"])
            after = op.get("after")
            if after is None:
                items.append(item)
            else:
                index = next((n for n, i in enumerate(items) if i["id"] == after), None)
                items.insert(index + 1 if index is not None else len(items), item)
        elif kind == "item.append":
            for item in items:
                if item["id"] == op["id"]:
                    item[op["field"]] = (item.get(op["field"]) or "") + op["text"]
        elif kind == "item.patch":
            for item in items:
                if item["id"] == op["id"]:
                    item.update(op["patch"])
        elif kind == "item.remove":
            items = [i for i in items if i["id"] != op["id"]]
        elif kind == "state.patch":
            state.update(op["patch"])
    return items, state


def bad_effort_levels(models, ladder, where):
    """The model entries whose effort_levels is not what the catalog promises.

    Every model object states the levels it takes — a list, in ladder order,
    drawn from thinking_levels — and [] for a model with no effort parameter.
    A list, never null: `reasoning` says a model can be asked to think, and
    this is what a client offers it without branching on the field's type."""
    bad = []
    for model in models:
        levels = model.get("effort_levels")
        if not isinstance(levels, list):
            bad.append(f"{where}[{model.get('id')}].effort_levels={levels!r}")
        elif levels != [level for level in ladder if level in levels]:
            bad.append(f"{where}[{model.get('id')}].effort_levels={levels!r}"
                       " is not a subset of thinking_levels in ladder order")
    return bad


def non_bools(items=(), state=None, catalog=None):
    """The documented boolean fields that are not JSON booleans.

    CONTRACT §4.1 (items), §4.2 (topic state) and §5.6 (catalog) call these
    fields bool.  NIL encodes as null, and a client parsing a flag cannot read
    null — so a null here is a bug, not a missing value."""
    bad = []
    for item in items:
        kind = item.get("kind")
        if kind == "notice" and not isinstance(item.get("durable"), bool):
            bad.append(f"notice.durable={item.get('durable')!r}")
        if kind == "compaction" and not isinstance(item.get("manual"), bool):
            bad.append(f"compaction.manual={item.get('manual')!r}")
        if kind == "tool" and not isinstance((item.get("result") or {}).get("truncated"), bool):
            bad.append(f"tool.result.truncated={(item.get('result') or {}).get('truncated')!r}")
    if state is not None and not isinstance((state.get("model") or {}).get("ready"), bool):
        bad.append(f"state.model.ready={(state.get('model') or {}).get('ready')!r}")
    if catalog is not None:
        for model in catalog["models"]:
            for field in ("reasoning", "images", "ready"):
                if not isinstance(model.get(field), bool):
                    bad.append(f"models[{model.get('id')}].{field}={model.get(field)!r}")
        for provider in catalog["providers"]:
            if not isinstance(provider.get("has_key"), bool):
                bad.append(f"providers[{provider.get('name')}].has_key={provider.get('has_key')!r}")
        lanes = catalog.get("lanes")
        if isinstance(lanes, dict):
            for model in lanes["models"]:
                if not isinstance(model.get("ok"), bool):
                    bad.append(f"lanes.models[{model.get('id')}].ok={model.get('ok')!r}")
    return bad


def wait_idle(server, timeout=45):
    """Wait for the session to be idle with no task: a turn's end."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        state = server.snapshot("session")["topics"]["session"]["state"]
        if state["status"] == "idle" and not state["task"]:
            return True
        time.sleep(0.1)
    return False


def item_text(items, kind, i=-1):
    got = [x for x in items if x["kind"] == kind]
    return got[i] if got else None


def in_thread(fn):
    """Run FN on a thread, remembering what it raised — a stream that dies
    quietly would otherwise look like an empty stream."""
    result = {}

    def run():
        try:
            fn()
        except BaseException as e:            # noqa: BLE001 - reported below
            result["error"] = repr(e)
            import traceback
            result["traceback"] = traceback.format_exc()

    thread = threading.Thread(target=run)
    thread.start()
    result["thread"] = thread
    return result


def join(result, timeout):
    result["thread"].join(timeout=timeout)
    if "error" in result:
        print(result.get("traceback"))
    return result.get("error")


def run_all(server, stub_port, work):
    # --- transport and auth -------------------------------------------------
    check("no token -> 401", server.get("/health", token="")[0] == 401)
    check("wrong token -> 401", server.get("/health", token="nope")[0] == 401)
    status, reply, _ = server.op("server.shutdown", token="nope")
    check("wrong token on an op -> 401", status == 401, (status, reply))
    check("unknown endpoint -> 404", server.get("/nope")[0] == 404)
    check("wrong method -> 405", server.get("/ops")[0] == 405)
    status, _ = server.request("POST", "/ops", raw_body=b"{not json")
    check("malformed JSON envelope -> 400", status == 400, status)
    status, _ = server.request("POST", "/ops", body={"op": "input.send"})
    check("an op without a rid -> 400", status == 400, status)

    # --- health -------------------------------------------------------------
    status, health = server.get("/health")
    check("health: identity, epoch, pid and the session clock",
          status == 200 and health["ok"] and health["program"] == "evo-agent"
          and health["version"] and len(health["epoch"]) == 8
          and isinstance(health["pid"], int)
          and isinstance(health["started_at"], int) and health["started_at"] > 10**12
          and isinstance(health["session_loop_age_ms"], int), health)

    # --- the model, registered through the eval op ----------------------------
    status, reply, _ = register_stub_model(server, stub_port)
    check("the eval op registers a provider and a model",
          status == 200 and reply["ok"] and reply["result"]["values"] == [":registered"], reply)
    status, reply, _ = server.op("eval", {"code": "(+ 1 2)"})
    check("eval returns the value", reply["result"]["values"] == ["3"], reply)
    status, reply, _ = server.op("eval", {"code":
        "(if (some (function evo.util:getenv)"
        " '(\"EVO_SUPERVISED_CHILD\" \"EVO_HEARTBEAT_FILE\" \"EVO_SERVE_WATCH_PID\""
        " \"EVO_SESSIONS_DIR\" \"EVO_SERVE_TOKEN\" \"EVO_RECOVERY\" \"EVO_PID\"))"
        " :leaked :clean)"})
    check("the server's environment is this test's, not a supervisor's",
          (reply.get("result") or {}).get("values") == [":clean"], reply)
    status, reply, _ = server.op("eval", {"code":
        "(if (string= (string-right-trim \"/\" (or (evo.util:getenv \"HOME\") \"\"))"
        " (string-right-trim \"/\" (namestring (evo.util:evo-home))))"
        " :this-test :someone-elses)"})
    check("the server's HOME is this test's home, not the caller's",
          (reply.get("result") or {}).get("values") == [":this-test"], reply)
    status, reply, _ = server.op("model.set", {"id": "stub-a"})
    check("model.set journals the choice", reply["ok"] and reply["result"]["model"]["id"] == "stub-a",
          reply)

    # --- catalog --------------------------------------------------------------
    status, catalog = server.get("/catalog")
    text = json.dumps(catalog)
    check("catalog: models, providers, ops, commands, tools",
          status == 200 and [m["id"] for m in catalog["models"]] == ["stub-a"]
          and catalog["default_model"]["id"] == "stub-a"
          and any(p["name"] == "stub" and p["has_key"] for p in catalog["providers"])
          and {"input.send", "input.cancel", "run.interrupt", "server.shutdown"}
          <= {o["name"] for o in catalog["ops"]}
          and any(c["name"] == "compact" for c in catalog["commands"]),
          sorted(catalog))
    default = catalog["default_model"]
    check("catalog: the default model's provider is the model's own",
          default["id"] == "stub-a"
          and default["provider"] == next(m for m in catalog["models"]
                                          if m["id"] == default["id"])["provider"],
          (default, catalog["models"]))
    check("catalog: every documented boolean is a boolean",
          not non_bools(catalog=catalog), non_bools(catalog=catalog))
    check("catalog: an evo-agent's document has no lanes key at all",
          "lanes" not in catalog, sorted(catalog))
    # The lanes half belongs to a program that runs lanes: evo-swarm registers
    # the hook, and this is that registration.
    status, reply, _ = server.op("eval", {"code":
        "(progn (setf evo.serve:*catalog-lanes-hook* #'evo.serve:lane-catalog)"
        " :hooked)"})
    check("catalog: the lanes hook is installable", reply["ok"], reply)
    status, hooked = server.get("/catalog")
    lanes = hooked.get("lanes")
    check("catalog: with the hook set, lanes.models is an object to read",
          isinstance(lanes, dict) and isinstance(lanes.get("models"), list)
          and [m["id"] for m in lanes["models"]] == ["stub-a"], lanes)
    bad = bad_effort_levels(lanes["models"], hooked["thinking_levels"], "lanes.models")
    check("catalog: a lane's model states its levels too, the same way",
          not bad, bad)
    check("catalog: a lane model's ok is a boolean, beside its id and provider",
          all(isinstance(m.get("ok"), bool) and m.get("provider")
              for m in lanes["models"]), lanes)
    check("catalog: the lanes half needs no lane started to answer",
          not non_bools(catalog=hooked), non_bools(catalog=hooked))
    server.op("eval", {"code": "(setf evo.serve:*catalog-lanes-hook* nil)"})
    status, back = server.get("/catalog")
    check("catalog: unregistering the hook takes the key back out",
          "lanes" not in back, sorted(back))
    check("catalog carries no secret", "e2e-secret" not in text)
    # Exactly what the session accepts: the ladder has no off rung, and the
    # CLI, /thinking and evo-swarm all refuse one.
    check("catalog: thinking levels and languages",
          catalog["thinking_levels"] == ["low", "medium", "high", "xhigh", "max"]
          and catalog["languages"], catalog["thinking_levels"])
    # What each model takes, as a list rather than a range: the stub declares
    # every level, and the shape is the one every client reads.
    bad = bad_effort_levels(catalog["models"], catalog["thinking_levels"], "models")
    check("catalog: every model states the effort levels it takes",
          not bad and catalog["models"][0]["effort_levels"] == catalog["thinking_levels"],
          bad or catalog["models"][0])
    # The effort a session started here would run on.  Nothing is journaled in
    # this server and no --thinking was passed, so it is the default the ladder
    # ends at — and it is a rung of that ladder, never a word of its own.
    check("catalog: the default thinking is what a fresh session runs on",
          catalog["default_thinking"] == "medium", catalog.get("default_thinking"))
    check("catalog: the default thinking is a rung of the ladder",
          catalog["default_thinking"] in catalog["thinking_levels"],
          catalog.get("default_thinking"))
    op_schema = next(o for o in catalog["ops"] if o["name"] == "input.send")
    check("catalog: an op carries its argument schema",
          op_schema["args"]["properties"]["text"]["type"] == "string"
          and op_schema["args"]["properties"]["queue"]["enum"] == ["now", "after_run"]
          and op_schema["precondition"] == "none", op_schema)
    cancel_schema = next(o for o in catalog["ops"] if o["name"] == "input.cancel")
    check("catalog: required arguments are named",
          cancel_schema["args"]["required"] == ["item_id"]
          and cancel_schema["precondition"] == "none", cancel_schema)
    complete_schema = next(o for o in catalog["ops"] if o["name"] == "complete")
    check("catalog: the completion op is offered with its schema",
          complete_schema["args"]["properties"]["text"]["type"] == "string"
          and complete_schema["args"]["properties"]["cursor"]["type"] == "integer"
          and complete_schema["args"]["required"] == ["text", "cursor"]
          and complete_schema["precondition"] == "none", complete_schema)

    # --- complete: a client's input box, answered without evaluating code -----
    # It used to take an `eval` op and a hand-built Lisp form calling
    # evo.eval:completions-for — remote code execution to read a name list,
    # and nothing at all under --no-http-eval.
    status, reply, _ = server.op("complete", {"text": "run /comp now", "cursor": 9})
    result = reply.get("result") or {}
    check("complete names the command word at the caret",
          reply["ok"] and result.get("kind") == "command"
          and result.get("start") == 5 and result.get("end") == 9, reply)
    check("complete offers the commands the catalog lists",
          any(i["name"] == "compact" for i in result.get("items", [])), result)
    check("complete's items are {name, description} objects",
          all(set(i) == {"name", "description"} and isinstance(i["description"], str)
              for i in result.get("items", [])), result)
    status, reply, _ = server.op("complete", {"text": "/eval (evo:all-too", "cursor": 18})
    result = reply.get("result") or {}
    check("complete completes /eval symbols, no eval op needed",
          reply["ok"] and result.get("kind") == "symbol"
          and result.get("start") == 7
          and any(i["name"] == "evo:all-tools" for i in result.get("items", [])), reply)
    status, reply, _ = server.op("complete", {"text": "/eval (list 1 / 2)", "cursor": 15})
    result = reply.get("result") or {}
    check("a lone slash inside /eval content answers as a symbol",
          reply["ok"] and result.get("kind") == "symbol"
          and not any(i["name"] == "compact" for i in result.get("items", [])), reply)
    status, reply, _ = server.op("complete",
                                 {"text": '/eval (format nil "see /comp")', "cursor": 28})
    result = reply.get("result") or {}
    check("a string's /word inside /eval content completes as a symbol too",
          reply["ok"] and result.get("kind") == "symbol"
          and result.get("items") == [], reply)
    status, reply, _ = server.op("complete", {"text": "/eval (list 1 / 2)", "cursor": 3})
    check("...while the /eval word itself still completes as a command",
          reply["ok"] and (reply["result"] or {}).get("kind") == "command"
          and any(i["name"] == "eval" for i in (reply["result"] or {}).get("items", [])),
          reply)
    status, reply, _ = server.op("complete", {"text": "hello there", "cursor": 3})
    result = reply.get("result") or {}
    check("nothing at the caret to complete: a null kind and no items",
          result.get("kind") is None and result.get("start") is None
          and result.get("items") == [], reply)
    status, reply, _ = server.op("complete", {"text": "x", "cursor": 9})
    check("a cursor outside the text -> invalid_args",
          reply["ok"] is False and reply["error"]["code"] == "invalid_args", reply)

    # --- the first snapshot: empty session, one seq for every topic -----------
    snap = server.snapshot("session,swarm,lane:*")
    check("snapshot has an epoch, a seq and the session topic",
          snap["epoch"] and isinstance(snap["seq"], int) and "session" in snap["topics"]
          and isinstance(snap["topics"]["session"]["state"], dict), sorted(snap))
    check("snapshot reports the model the catalog does",
          snap["topics"]["session"]["state"]["model"]["id"] == "stub-a",
          snap["topics"]["session"]["state"])
    check("snapshot: has_more is a boolean, not the null NIL would send",
          snap["topics"]["session"]["has_more"] is False,
          snap["topics"]["session"].get("has_more"))
    check("state carries the shape the GUI draws (CONTRACT §4.2)",
          {"status", "task", "model", "thinking", "language", "context", "goal",
           "todos", "queue", "jobs", "segments", "session"}
          <= set(snap["topics"]["session"]["state"]), sorted(snap["topics"]["session"]["state"]))

    # --- stream: hello, then the ops of a turn --------------------------------
    baseline = server.snapshot("session")

    def fold(ops, snapshot=None):
        base = snapshot or baseline
        return apply_ops(base["topics"]["session"]["items"],
                         base["topics"]["session"]["state"], ops)

    def turn_over(ops):
        """The turn is over when the answer is final *and* nothing runs: the
        run's own end is the last op of a turn."""
        items, state = fold(ops)
        return (any(i["kind"] == "assistant" and i.get("status") in ("final", "error")
                    for i in items)
                and state.get("status") not in ("running", "compacting"))

    collected = {}

    def stream_turn():
        cursor = f"{baseline['epoch']}.{baseline['seq']}"
        collected["hello"], collected["ops"] = server.read_stream(turn_over, topics="session",
                                                                  since=cursor)

    reading = in_thread(stream_turn)
    time.sleep(0.3)
    status, reply, _ = server.op("input.send", {"text": "hello e2e"})
    # Idle, and asking for "now": the input starts the run, so it is *sent*,
    # not queued — `queued` is the truth about the input (CONTRACT §5.5).
    check("input.send answers with the item id and its queue state",
          status == 200 and reply["ok"] and reply["result"]["item_id"]
          and reply["result"]["queued"] is False
          and reply["result"]["blocked"] is None, reply)
    sent_id = reply["result"]["item_id"]
    join(reading, 45)
    ops = collected.get("ops") or []
    hello = collected.get("hello")
    check("the stream's first frame is hello, at the snapshot's cursor",
          hello and hello["epoch"] == baseline["epoch"] and hello["seq"] == baseline["seq"], hello)
    check("the input appears as a queued user item",
          any(o["op"] == "item.add" and o["item"].get("id") == sent_id
              and o["item"]["kind"] == "user" and o["item"]["status"] == "queued" for o in ops),
          ops[:3])
    items, state = fold(ops)
    answers = [i for i in items if i["kind"] == "assistant"]
    check("the answer is its own item", len(answers) == 1, answers)
    check("the answer streamed back through item.append",
          answers and answers[0]["text"] == "ok: hello e2e", answers)
    check("the answer's final status reached the client",
          any(o["op"] == "item.patch" and o["patch"].get("status") in ("final", "error")
              for o in ops)
          or any(o["op"] == "item.add" and o["item"]["kind"] == "assistant"
                 and o["item"]["status"] == "final" for o in ops), ops[-3:])
    check("the item's model and provider are on it",
          answers and answers[0]["model"] == "stub-a" and answers[0]["provider"] == "stub",
          answers)
    check("the user's item reads as sent once it is journaled",
          any(i["kind"] == "user" and i["status"] == "sent" for i in items), items)
    check("every op carries seq, ts and topic",
          all(isinstance(o.get("seq"), int) and isinstance(o.get("ts"), int)
              and o.get("topic") == "session" for o in ops
              if o["op"] not in ("hello", "stream.reset")), ops[:2])
    check("an assistant item's usage is absent or an object, never null",
          not [o for o in ops if o["op"] == "item.add"
               and o["item"].get("kind") == "assistant" and "usage" in o["item"]
               and o["item"]["usage"] is None]
          and not [o for o in ops if o["op"] == "item.patch" and "usage" in o["patch"]
                   and o["patch"]["usage"] is None], ops)
    check("the queued item says which queue was asked for",
          [o for o in ops if o["op"] == "item.add" and o["item"].get("id") == sent_id
           and o["item"].get("queue") == "now"], ops[:3])
    check("a turn: every documented boolean is a boolean",
          not non_bools(items=items, state=state), non_bools(items=items, state=state))
    check("appends are coalesced, not one op per delta",
          len([o for o in ops if o["op"] == "item.append" and o["field"] == "text"]) <= 3,
          [o for o in ops if o["op"] == "item.append"])

    # --- snapshot + stream consistency ----------------------------------------
    items, state = fold(ops)
    after = server.snapshot("session")
    check("applying the ops to a snapshot equals the next snapshot",
          [i["id"] for i in items] == [i["id"] for i in after["topics"]["session"]["items"]]
          and [i.get("text") for i in items]
          == [i.get("text") for i in after["topics"]["session"]["items"]],
          (items, after["topics"]["session"]["items"]))
    check("the snapshot is at a seq no older than the stream's last op",
          after["seq"] >= ops[-1]["seq"], (after["seq"], ops[-1]["seq"]))
    check("the answer's status is final in the snapshot",
          item_text(after["topics"]["session"]["items"], "assistant")["status"] == "final")
    check("the folded state matches the snapshot's too",
          state.get("status") == after["topics"]["session"]["state"]["status"]
          and state.get("model") == after["topics"]["session"]["state"]["model"], state)

    # --- reconnect with a cursor ----------------------------------------------
    cursor = f"{after['epoch']}.{after['seq']}"
    seen = {}

    def stream_second():
        hello, ops = server.read_stream(lambda ops: len(ops) >= 2, since=cursor)
        seen["hello"] = hello
        seen["ops"] = ops

    reading = in_thread(stream_second)
    time.sleep(0.3)
    server.op("input.send", {"text": "second turn"})
    join(reading, 45)
    check("reconnecting with a cursor hears only what came after it",
          seen.get("hello", {}).get("seq") == after["seq"]
          and seen["ops"][0]["seq"] == after["seq"] + 1, (seen.get("hello"), seen.get("ops", [])[:1]))
    check("the ops after the cursor are the new turn's",
          len(seen["ops"]) >= 2, seen.get("ops"))

    # --- a cursor from nowhere -------------------------------------------------
    hello, ops = server.read_stream(lambda ops: len(ops) >= 0, since="deadbeef.5")
    check("a cursor from another epoch gets hello then stream.reset{restarted}",
          hello and any(o["op"] == "stream.reset" and o["reason"] == "restarted" for o in ops),
          (hello, ops))
    hello, ops = server.read_stream(lambda ops: len(ops) >= 0, since=f"{snap['epoch']}.999999")
    check("a cursor beyond the log gets stream.reset{cursor_unknown}",
          any(o["op"] == "stream.reset" and o["reason"] == "cursor_unknown" for o in ops), ops)
    hello, ops = server.read_stream(lambda ops: len(ops) >= 0, since="not-a-cursor")
    check("a cursor that is not one gets stream.reset{cursor_unknown}",
          any(o["op"] == "stream.reset" and o["reason"] == "cursor_unknown" for o in ops), ops)

    # --- topic filtering -------------------------------------------------------
    frames = server.probe_stream(topics="lane:*", seconds=1.5)
    check("a stream filtered to lane:* carries hello and nothing else",
          len(frames) == 1 and frames[0][1] == "op"
          and frames[0][2]["op"] == "hello", frames[:3])

    # --- a queued item a client can cancel -------------------------------------
    # Queued input is drainable at the running turn's next boundary, so the
    # window a client can cancel in is a turn: the stub's SLOW answer holds it
    # open for six seconds.
    status, reply, _ = server.op("input.send", {"text": "SLOW while queuing"})
    first_id = reply["result"]["item_id"]
    status, reply, _ = server.op("input.send", {"text": "cancel this one"})
    item_id = reply["result"]["item_id"]
    check("input.send while running queues, and says so",
          reply["ok"] and item_id != first_id and reply["result"]["queued"] is True, reply)
    # complete is read-only and needs nothing idle: a client's input box keeps
    # answering while a turn runs (its precondition is `none`).
    busy = server.snapshot("session")["topics"]["session"]["state"]["status"]
    status, reply, _ = server.op("complete", {"text": "/comp", "cursor": 5})
    check("complete answers while the session is busy",
          busy == "running" and reply["ok"]
          and (reply["result"] or {}).get("kind") == "command", (busy, reply))
    status, reply, _ = server.op("input.cancel", {"item_id": item_id})
    check("a queued input can be cancelled", status == 200 and reply["ok"], reply)
    check("input.cancel answers an object, not null", reply["result"] == {}, reply)
    status, reply, _ = server.op("input.cancel", {"item_id": item_id})
    check("cancelling it again -> already_sent",
          reply["ok"] is False and reply["error"]["code"] == "already_sent", reply)
    status, reply, _ = server.op("input.cancel", {"item_id": first_id})
    check("input already drained -> already_sent",
          reply["ok"] is False and reply["error"]["code"] == "already_sent", reply)
    snap = server.snapshot("session")
    check("the cancelled item is gone from the snapshot",
          all(i["id"] != item_id for i in snap["topics"]["session"]["items"]),
          [i for i in snap["topics"]["session"]["items"] if i["id"] == item_id])

    server.op("run.interrupt", {"scope": "session"})

    # --- input.cancel while the run is going, then interrupt --------------------
    status, reply, _ = server.op("input.send", {"text": "SLOW and long"})
    running_snap = server.snapshot("session")
    check("input.send starts a run when the session is idle",
          running_snap["topics"]["session"]["state"]["status"] == "running",
          running_snap["topics"]["session"]["state"])
    queued_id = reply["result"]["item_id"]
    status, reply, _ = server.op("input.send", {"text": "steered mid run"})
    check("input.send while running queues a second item", reply["ok"], reply)
    status, reply, _ = server.op("run.interrupt", {"scope": "session"})
    check("run.interrupt(scope session) reports what it interrupted",
          reply["ok"] and reply["result"]["interrupted"] == ["session"], reply)
    deadline = time.time() + 30
    idle = False
    while time.time() < deadline:
        if server.snapshot("session")["topics"]["session"]["state"]["status"] == "idle":
            idle = True
            break
        time.sleep(0.1)
    check("the session is idle again after the interrupt", idle)
    status, reply, _ = server.op("run.interrupt", {"scope": "session"})
    check("interrupting an idle session reports nothing interrupted",
          reply["ok"] and reply["result"]["interrupted"] == [], reply)
    # A lane is a topic on the server that has it: one that does not exist is
    # NOT_FOUND, the caller naming something absent — not a failed op.
    status, reply, _ = server.op("run.interrupt", {"scope": "lane", "lane": 9})
    check("interrupting a lane that does not exist -> not_found",
          reply["ok"] is False and reply["error"]["code"] == "not_found", reply)
    status, reply, _ = server.op("run.interrupt", {"scope": "lane", "lane": 1})
    check("interrupting lane 1 on a server with no lanes -> not_found",
          reply["ok"] is False and reply["error"]["code"] == "not_found", reply)
    status, reply, _ = server.op("run.interrupt", {"scope": "lane"})
    check("scope lane without a lane number -> invalid_args",
          reply["ok"] is False and reply["error"]["code"] == "invalid_args", reply)
    status, reply, _ = server.op("run.interrupt", {"scope": "swarm"})
    check("scope swarm on a plain server -> invalid_args (unchanged)",
          reply["ok"] is False and reply["error"]["code"] == "invalid_args", reply)
    # A call that takes seconds is still a call.  The wait must not read a
    # condition variable's wake as an answer: a spurious one used to answer
    # 503 "the session thread did not answer" for a call that was merely slow.
    status, reply, _ = server.op("eval", {"code": "(progn (sleep 3) :slept)"})
    check("a slow call answers rather than 503ing",
          status == 200 and reply["ok"] and reply["result"]["values"] == [":slept"], reply)

    # --- input.send names a topic, and a lane is not one a client writes to ----------
    status, reply, _ = server.op("input.send", {"text": "steer a lane",
                                                "topic": "lane:1"})
    check("input.send to a lane topic -> invalid_args, not not_found",
          reply["ok"] is False and reply["error"]["code"] == "invalid_args", reply)
    check("and the refusal says whose lane it is",
          "coordinator" in reply["error"]["message"], reply)
    status, reply, _ = server.op("input.send", {"text": "steer a lane",
                                                "topic": "lane:9"})
    check("a lane that does not exist is refused the same way",
          reply["ok"] is False and reply["error"]["code"] == "invalid_args", reply)
    status, reply, _ = server.op("input.send", {"text": "nowhere", "topic": "nope"})
    check("a topic that is not one -> not_found (unchanged)",
          reply["ok"] is False and reply["error"]["code"] == "not_found", reply)
    status, reply, _ = server.op("input.send", {"text": "to the session",
                                                "topic": "session"})
    check("input.send to the session topic is the same as without it",
          reply["ok"] is True and reply["result"]["item_id"], reply)
    wait_idle(server)

    # --- after_run with nothing running is simply now -------------------------------
    status, reply, _ = server.op("input.send", {"text": "after_run and idle",
                                                "queue": "after_run"})
    check("input.send(after_run) while idle: sent now, so not queued",
          reply["ok"] and reply["result"]["queued"] is False, reply)
    after_run_id = reply["result"]["item_id"]
    wait_idle(server)
    asked = next((i for i in server.snapshot("session")["topics"]["session"]["items"]
                  if i["id"] == after_run_id), None)
    check("the item still says after_run, which is what was asked",
          asked and asked.get("queue") == "after_run" and asked["status"] != "queued", asked)

    # --- ops idempotency ---------------------------------------------------------
    before = len(stub_requests(stub_port))
    rid = uuid.uuid4().hex
    status, first, _ = server.op("input.send", {"text": "sent once"}, rid=rid)
    status, second, _ = server.op("input.send", {"text": "sent once"}, rid=rid)
    check("a retried rid returns the same reply",
          first["ok"] and second == first, (first, second))
    time.sleep(2)
    check("and does nothing a second time",
          len(stub_requests(stub_port)) - before == 1,
          len(stub_requests(stub_port)) - before)
    snap = server.snapshot("session")
    check("the retried prompt is one item, not two",
          len([i for i in snap["topics"]["session"]["items"]
               if i["kind"] == "user" and i.get("text") == "sent once"]) == 1)

    # --- error codes --------------------------------------------------------------
    status, reply, _ = server.op("nonsense.op", {})
    check("an unknown op -> unknown_op (still HTTP 200)",
          status == 200 and reply["error"]["code"] == "unknown_op", reply)
    status, reply, _ = server.op("input.send", {"text": ""})
    check("an op with bad arguments -> invalid_args",
          reply["error"]["code"] == "invalid_args", reply)
    status, reply, _ = server.op("input.send", {"text": "x", "queue": "whenever"})
    check("an enum argument out of range -> invalid_args",
          reply["error"]["code"] == "invalid_args", reply)
    status, reply, _ = server.op("input.cancel", {"item_id": "e_nope"})
    check("cancelling something never queued -> already_sent",
          reply["error"]["code"] == "already_sent", reply)
    status, reply = server.get("/items/not-an-item?topic=session")
    check("an unknown item -> 404", status == 404, (status, reply))
    check("an unknown topic -> 404", server.get("/items?topic=nope")[0] == 404)
    check("unknown media -> 404", server.get("/media/e_x/0?topic=session")[0] == 404)
    check("an error message never quotes the value it refused",
          "whenever" not in json.dumps(reply), reply)

    # --- command.run ---------------------------------------------------------------
    status, reply, _ = server.op("command.run", {"name": "model"})
    check("command.run runs a builtin and returns its choices",
          reply["ok"] and reply["result"]["choices"] is not None, reply)
    status, reply, _ = server.op("command.run", {"name": "thinking", "args": "high"})
    check("command.run switches the thinking level",
          reply["ok"] and reply["result"]["data"]["thinking"] == "high", reply)
    notices = reply["result"]["notices"]
    check("command.run answers the line the command said, not its style keyword",
          len(notices) == 1 and notices[0]["text"].startswith("thinking"), notices)
    snap = server.snapshot("session")
    check("the state follows the command", snap["topics"]["session"]["state"]["thinking"] == "high",
          snap["topics"]["session"]["state"]["thinking"])
    # ...and so does the catalog, which a client that only reads it has to
    # agree with: the level this session resolves next, not a launch default
    # frozen at boot.
    status, after = server.get("/catalog")
    check("catalog: the default thinking follows the session's own level",
          after["default_thinking"] == "high", after.get("default_thinking"))
    # A command's lines carry their own text and the severity of the style they
    # were said with.  The reply keeps them as (:style … :text …) plists, and
    # reading one as (style text) sent the keywords "style" and "plain" as the
    # line's text, with every severity an info.
    status, reply, _ = server.op("eval", {"code": """
        (progn
          (evo:register-command
           "notice-probe"
           (lambda (ctx)
             (let ((host (getf ctx :host)))
               (evo.command:host-notice host "a plain line")
               (evo.command:host-notice host "a failure" :severity :error)
               nil)))
          :registered)"""})
    check("command.run: the probe command is registered", reply["ok"], reply)
    status, reply, _ = server.op("command.run", {"name": "notice-probe"})
    notices = (reply.get("result") or {}).get("notices")
    check("command.run: each notice is the line, in the order it was said",
          [n["text"] for n in notices or []] == ["a plain line", "a failure"], notices)
    check("command.run: an error line reads as an error, a plain one as info",
          [n["severity"] for n in notices or []] == ["info", "error"], notices)
    status, reply, _ = server.op("command.run", {"name": "nonsense"})
    check("command.run of an unknown command -> not_found",
          reply["error"]["code"] == "not_found", reply)

    # --- goal ops -----------------------------------------------------------------
    status, reply, _ = server.op("goal.set", {"objective": "e2e objective"})
    check("goal.set creates the goal", reply["ok"] and reply["result"]["goal"]["status"] == "active",
          reply)
    status, reply, _ = server.op("goal.pause", {})
    check("goal.pause pauses it", reply["result"]["goal"]["status"] == "paused", reply)
    status, reply, _ = server.op("goal.resume", {})
    check("goal.resume resumes it", reply["result"]["goal"]["status"] == "active", reply)
    status, reply, _ = server.op("goal.clear", {})
    check("goal.clear withdraws the goal (it reads as none)",
          reply["ok"] and reply["result"]["goal"] is None, reply)

    # --- items paging ---------------------------------------------------------------
    status, page = server.get("/items?topic=session&limit=1")
    check("items: paging returns the newest N and says there is more",
          status == 200 and len(page["items"]) == 1 and page["has_more"] is True, page)
    older = page["items"][0]["id"]
    status, page2 = server.get(f"/items?topic=session&before={older}&limit=100")
    status, page_wide = server.get("/items?topic=session&limit=1000")
    check("items: has_more is false (not null) when there is nothing more",
          page_wide["has_more"] is False, page_wide.get("has_more"))
    check("items: `before` pages back from an id",
          status == 200 and all(i["id"] != older for i in page2["items"]), page2)

    # --- a tool call: the result's flags are booleans, not null ------------------------
    status, reply, _ = server.op("input.send",
                                 {"text": 'CALL bash {"command": "echo hi"}'})
    check("a tool-calling turn starts", reply["ok"], reply)
    check("the tool turn finishes", wait_idle(server))
    snap = server.snapshot("session")
    tools = [i for i in snap["topics"]["session"]["items"] if i["kind"] == "tool"]
    check("the tool call is an item carrying its result object",
          tools and isinstance(tools[-1].get("result"), dict), tools[-1:])
    check("tool result.truncated is false, not null",
          tools and tools[-1]["result"]["truncated"] is False, tools[-1:])

    # --- preconditions and the debug reads --------------------------------------------
    status, reply, _ = server.op("input.send", {"text": "SLOW while switching"})
    status, reply, _ = server.op("session.new", {})
    check("session.new while busy -> not_quiescent",
          reply["ok"] is False and reply["error"]["code"] == "not_quiescent", reply)
    status, reply, _ = server.op("context.compact", {})
    check("context.compact while busy -> busy",
          reply["ok"] is False and reply["error"]["code"] == "busy", reply)
    # make sure the session is idle before the compaction below
    deadline = time.time() + 30
    while time.time() < deadline and \
            server.snapshot("session")["topics"]["session"]["state"]["status"] != "idle":
        time.sleep(0.1)
    server.op("run.interrupt", {"scope": "session"})
    deadline = time.time() + 30
    while time.time() < deadline and \
            server.snapshot("session")["topics"]["session"]["state"]["status"] != "idle":
        time.sleep(0.1)
    status, ctx = server.get("/debug/context")
    check("debug/context shows what the model sees",
          status == 200 and isinstance(ctx["messages"], list) and ctx["messages"], list(ctx)[:3])
    status, journal = server.get("/debug/journal")
    check("debug/journal shows the entries on the path",
          status == 200 and journal["entries"] and journal["header"]["id"], list(journal)[:3])

    # --- a compaction, and the durable notice it leaves --------------------------------
    # Make the compaction actually compact: what triggers it is the amount of
    # context it may keep, not the size of the session.
    server.op("eval", {"code": "(setf evo.kernel::*compact-keep-recent-tokens* 1)"})
    status, reply, _ = server.op("context.compact", {"hint": "keep the last turn"})
    check("context.compact on an idle session starts a task",
          status == 200 and reply["ok"] and reply["result"]["task_id"], reply)
    deadline = time.time() + 60
    while time.time() < deadline:
        state = server.snapshot("session")["topics"]["session"]["state"]
        if state["status"] == "idle" and not state["task"]:
            break
        time.sleep(0.1)
    time.sleep(0.3)
    snap = server.snapshot("session")
    notices = [i for i in snap["topics"]["session"]["items"]
               if i["kind"] == "notice" and (i.get("text") or "").startswith("✓ compacted")]
    check("a durable notice is one item, not a live copy beside the journaled one",
          len(notices) == 1, notices)
    check("a compaction: every documented boolean is a boolean",
          not non_bools(items=snap["topics"]["session"]["items"],
                        state=snap["topics"]["session"]["state"]),
          non_bools(items=snap["topics"]["session"]["items"],
                    state=snap["topics"]["session"]["state"]))
    check("the compaction is an item on the path too",
          any(i["kind"] == "compaction" for i in snap["topics"]["session"]["items"]),
          [(i["kind"], (i.get("text") or "")[:40]) for i in snap["topics"]["session"]["items"]][-5:])

    # --- sessions ---------------------------------------------------------------------
    status, sessions = server.get("/sessions")
    check("sessions: the current session is listed with its path",
          status == 200 and any(os.path.realpath(s["path"])
                                == os.path.realpath(server.info["session"]["path"])
                                for s in sessions["sessions"]), sessions)

    # --- eval gate ----------------------------------------------------------------------
    # What the restart check needs from this process, taken while it is alive.
    snap = server.snapshot("session")
    server.last_cursor = f"{snap['epoch']}.{snap['seq']}"
    status, reply, _ = server.op("eval", {"code": "(+ 1 1)"})
    check("eval is offered when it is not disabled", reply["ok"], reply)
    status, reply, _ = server.op("server.shutdown", {})
    check("server.shutdown is answered", status == 200 and reply["ok"], reply)

    # --- the booleans of a reply ------------------------------------------------
    bad = [f"{name}.ok={reply.get('ok')!r}" for name, reply in REPLIES
           if not isinstance(reply.get("ok"), bool)]
    bad += [f"{name}.result.queued={(reply.get('result') or {}).get('queued')!r}"
            for name, reply in REPLIES
            if name == "input.send" and reply.get("ok")
            and not isinstance((reply.get("result") or {}).get("queued"), bool)]
    check("every reply's ok — and input.send's queued — is a boolean", not bad, bad[:4])


def idle_cpu_check(server):
    """An idle server waits for work; it does not burn a core waiting.

    The session loop sleeps on its inbox, so an idle process should spend
    essentially no CPU.  (It used to spend a whole core: the loop polled.)"""
    server.start()
    pid = server.info["pid"]
    time.sleep(1)                              # let boot and the first sweep go
    before = cpu_seconds(pid)
    time.sleep(3)
    after = cpu_seconds(pid)
    used = None if before is None or after is None else after - before
    check("an idle server spends no measurable CPU",
          used is not None and used < 0.5, f"{used}s of CPU in 3s idle")
    check("the idle server is still answering", server.get("/health")[0] == 200)
    server.op("server.shutdown", {})
    check("an idle server shuts down cleanly", server.wait_exit() == 0)


def stdin_eof_check(server, stub_port):
    """--watch-stdin: the server ends when its input does.

    Before a turn, and after one.  The turn matters because a server that has
    answered is still a server whose stdin closed — and because a stream is
    open while it happens, which is what a client looks like (the GUI's
    t08_quit_on_eof drives exactly this)."""
    server.start(args=("--watch-stdin",), stdin_pipe=True)
    server.close_stdin()
    check("watch-stdin: closing stdin ends an idle server",
          server.wait_exit(timeout=15) == 0,
          "still running 15s after its stdin closed")

    server.start(args=("--watch-stdin",), stdin_pipe=True)
    register_stub_model(server, stub_port)
    reading = in_thread(lambda: server.probe_stream(seconds=30))
    time.sleep(0.3)
    status, reply, _ = server.op("input.send", {"text": "one turn before eof"})
    check("watch-stdin: the turn before the eof runs",
          reply["ok"] and wait_idle(server), reply)
    server.close_stdin()
    check("watch-stdin: closing stdin ends a server that has answered",
          server.wait_exit(timeout=15) == 0,
          "still running 15s after its stdin closed")
    join(reading, 5)


def shutdown_with_stream_check(server, stub_port):
    """server.shutdown after a turn, with a client connected.

    The shutdown path wakes the log, closes the listener, joins the flusher and
    then the connection threads.  Each of those waits has to come back with a
    stream open — when it did not, the server kept running for ever."""
    server.start(args=("--watch-stdin",), stdin_pipe=True)
    register_stub_model(server, stub_port)
    reading = in_thread(lambda: server.probe_stream(seconds=30))
    time.sleep(0.3)
    status, reply, _ = server.op("input.send", {"text": "a turn, then shutdown"})
    check("a turn with a stream open finishes", reply["ok"] and wait_idle(server), reply)
    status, reply, _ = server.op("server.shutdown", {})
    check("server.shutdown after a turn is answered",
          status == 200 and reply["ok"], reply)
    check("server.shutdown after a turn exits 0",
          server.wait_exit(timeout=20) == 0, "still running 20s after shutdown")
    check("the ready file is removed on that exit too", not os.path.exists(server.ready))
    join(reading, 5)


def swarm_catalog_check(work, stub_port):
    """GET /catalog on a live evo-swarm: `lanes` is the swarm's half of the
    document (CONTRACT §5.6).

    It comes from the hook serve defines (`evo.serve:*catalog-lanes-hook*`)
    and swarm/main.lisp registers, so this is the end-to-end check that a real
    swarm answers `lanes.models[]` — and not the null a live run found."""
    if not os.access(SWARM, os.X_OK):
        print("skip no build/evo-swarm: the lanes half is not checked here")
        return
    swarm_work = os.path.join(work, "swarm")
    os.makedirs(os.path.join(swarm_work, "home"), exist_ok=True)
    os.makedirs(os.path.join(swarm_work, "proj"), exist_ok=True)
    swarm = Server(swarm_work, binary=SWARM)
    try:
        swarm.start(args=("--workers", "1"))
        register_stub_model(swarm, stub_port)
        status, catalog = swarm.get("/catalog")
        lanes = catalog.get("lanes")
        check("evo-swarm: the live catalog has a lanes object",
              isinstance(lanes, dict), (lanes, sorted(catalog)))
        check("evo-swarm: lanes.models is the models a lane can run",
              isinstance(lanes.get("models"), list)
              and [m["id"] for m in lanes["models"]] == ["stub-a"], lanes)
        check("evo-swarm: a lane model's ok is a boolean",
              all(isinstance(m.get("ok"), bool) and m.get("provider")
                  for m in lanes["models"]), lanes)
        check("evo-swarm: every documented boolean is a boolean there too",
              not non_bools(catalog=catalog), non_bools(catalog=catalog))
        # The swarm defines its own :lane method (which raises a plain error
        # while looking a lane up); serve's :before method is what makes this
        # not_found rather than op_failed.
        status, reply, _ = swarm.op("input.send", {"text": "steer a lane",
                                                   "topic": "lane:1"})
        check("evo-swarm: input.send to a lane topic -> invalid_args",
              reply["ok"] is False and reply["error"]["code"] == "invalid_args", reply)
        status, reply, _ = swarm.op("run.interrupt", {"scope": "lane", "lane": 1})
        check("evo-swarm: a lane that is idle reports an empty array",
              reply["ok"] and reply["result"]["interrupted"] == [], reply)
        status, reply, _ = swarm.op("run.interrupt", {"scope": "lane", "lane": 9})
        check("evo-swarm: a lane that does not exist -> not_found, not op_failed",
              reply["ok"] is False and reply["error"]["code"] == "not_found", reply)
        status, reply, _ = swarm.op("server.shutdown", {})
        check("evo-swarm: server.shutdown is answered", reply["ok"], reply)
        check("evo-swarm: the swarm exits 0 and takes its lane with it",
              swarm.wait_exit(timeout=60) == 0)
    finally:
        swarm.stop()


def eval_gate_check(server):
    """--no-http-eval removes the op and the catalog entry with it: eval over
    HTTP is remote code execution, and the flag is how a caller says no."""
    server.start(args=("--no-http-eval",))
    status, catalog = server.get("/catalog")
    check("--no-http-eval keeps eval out of the catalog",
          status == 200 and all(o["name"] != "eval" for o in catalog["ops"]), catalog["ops"])
    status, reply, _ = server.op("eval", {"code": "(+ 1 1)"})
    check("--no-http-eval answers unknown_op",
          reply["ok"] is False and reply["error"]["code"] == "unknown_op", reply)
    # Completion is a read, not an evaluation: the gate is about running code,
    # and a client's input box does not need to.
    check("--no-http-eval keeps complete in the catalog",
          any(o["name"] == "complete" for o in catalog["ops"]), catalog["ops"])
    status, reply, _ = server.op("complete", {"text": "/comp", "cursor": 5})
    check("complete works with eval gated off",
          reply["ok"] and (reply["result"] or {}).get("kind") == "command"
          and any(i["name"] == "compact" for i in (reply["result"] or {}).get("items", [])),
          reply)
    # This server resumed the session but no model is registered in it (the
    # registration was this process's), so the model gate says so and the
    # notice it leaves is the ephemeral one.
    status, reply, _ = server.op("input.send", {"text": "no model here"})
    check("input stays queued while the model does not resolve",
          reply["ok"] and reply["result"]["blocked"] == "model_not_ready"
          and reply["result"]["queued"] is True, reply)
    snap = server.snapshot("session")
    items = snap["topics"]["session"]["items"]
    check("a live notice is ephemeral, so durable reads false",
          any(i["kind"] == "notice" and i["durable"] is False for i in items),
          [i for i in items if i["kind"] == "notice"][-1:])
    check("a notice without a model: every documented boolean is a boolean",
          not non_bools(items=items, state=snap["topics"]["session"]["state"]),
          non_bools(items=items, state=snap["topics"]["session"]["state"]))
    server.op("server.shutdown", {})
    check("the gated server still shuts down cleanly", server.wait_exit() == 0)


def restart_check(server):
    """A restart is a new epoch: a cursor from the old process is told to
    re-snapshot, in band (CONTRACT §5.3) — pids are never compared."""
    cursor = server.last_cursor
    old_epoch = cursor.split(".")[0]
    info = server.start()
    check("the restarted server has a new epoch", info["epoch"] != old_epoch,
          (old_epoch, info["epoch"]))
    hello, ops = server.read_stream(lambda ops: len(ops) >= 0, since=cursor)
    check("a cursor from the old epoch gets hello then stream.reset{restarted}",
          hello and hello["epoch"] == info["epoch"]
          and any(o["op"] == "stream.reset" and o["reason"] == "restarted" for o in ops),
          (hello, ops))


def main():
    global failed
    if not os.access(EVO, os.X_OK):
        print(f"serve-e2e: no binary at {EVO} (make build first)")
        return 1
    work = tempfile.mkdtemp(prefix="evo-serve-e2e-")
    os.makedirs(os.path.join(work, "home"))
    os.makedirs(os.path.join(work, "proj"))
    stub_port = free_port()
    stub = subprocess.Popen([sys.executable, os.path.join(ROOT, "tests", "stub-messages.py"),
                             str(stub_port)], stdout=subprocess.PIPE, text=True)
    assert stub.stdout.readline().startswith("stub listening")
    server = Server(work)
    try:
        info = server.start()
        mode = stat.S_IMODE(os.stat(server.ready).st_mode)
        check("the ready file is private (0600)", mode == 0o600, oct(mode))
        for key in ("epoch", "pid", "port", "url", "token", "session", "program", "version"):
            if key not in info:
                check(f"ready file carries {key}", False, sorted(info))
                break
        else:
            check("the ready file carries epoch, pid, port, url, token, session, program, version",
                  info["token"] == server.token and len(info["epoch"]) == 8
                  and info["program"] == "evo-agent"
                  and info["session"]["id"] and info["session"]["path"], info)
        run_all(server, stub_port, work)
        # --- clean shutdown ----------------------------------------------------------
        try:
            code = server.proc.wait(timeout=30)
        except subprocess.TimeoutExpired:
            code = None
        check("evo exits 0 after server.shutdown", code == 0, code)
        check("the ready file is removed on a clean exit", not os.path.exists(server.ready))
        restart_check(server)
        server.op("server.shutdown", {})
        server.wait_exit()
        idle_cpu_check(server)
        stdin_eof_check(server, stub_port)
        shutdown_with_stream_check(server, stub_port)
        eval_gate_check(server)
        swarm_catalog_check(work, stub_port)
    except BaseException as e:
        failed += 1
        print(f"FAIL aborted: {e!r}")
        import traceback
        traceback.print_exc()
    finally:
        server.stop()
        stub.kill()
    if failed:
        print(open(server.log_path).read()[-4000:])
    else:
        shutil.rmtree(work, ignore_errors=True)
    print(f"\nserve-e2e: {passed} passed, {failed} failed")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
