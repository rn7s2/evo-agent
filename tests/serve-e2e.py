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

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EVO = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else os.path.join(ROOT, "build", "evo-agent")

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


class Server:
    """One `evo serve` child, driven through its ready file."""

    def __init__(self, work, baby=False):
        self.work = work
        self.ready = os.path.join(work, "ready.json")
        self.log_path = os.path.join(work, "evo.log")
        self.baby = baby

    def start(self, args=()):
        env = dict(os.environ, EVO_HOME=os.path.join(self.work, "home"),
                   EVO_NO_SUPERVISOR="1")
        for var in ("EVO_SERVE_TOKEN", "EVO_SESSIONS_DIR", "ANTHROPIC_API_KEY"):
            env.pop(var, None)
        if os.path.exists(self.ready):
            os.remove(self.ready)
        log = open(self.log_path, "a")
        self.proc = subprocess.Popen(
            [EVO, "serve", "--no-userspace", "--port", "0",
             "--ready-file", self.ready, *args],
            cwd=os.path.join(self.work, "proj"), env=env,
            stdout=log, stderr=subprocess.STDOUT)
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

    def wait_exit(self, timeout=30):
        try:
            return self.proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            return None

    def stop(self):
        if self.proc.poll() is None:
            self.proc.kill()


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
    form = ("(progn"
            f" (evo:register-provider :stub :base-url \"http://127.0.0.1:{stub_port}\""
            "   :api-key \"e2e-secret\")"
            " (evo:register-model \"stub-a\" :provider :stub :context-window 200000"
            "   :max-output 8000 :effort t)"
            " (evo:set-setting :model \"stub-a\")"
            " :registered)")
    status, reply, _ = server.op("eval", {"code": form})
    check("the eval op registers a provider and a model",
          status == 200 and reply["ok"] and reply["result"]["values"] == [":registered"], reply)
    status, reply, _ = server.op("eval", {"code": "(+ 1 2)"})
    check("eval returns the value", reply["result"]["values"] == ["3"], reply)

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
    check("catalog carries no secret", "e2e-secret" not in text)
    check("catalog: thinking levels and languages",
          catalog["thinking_levels"][0] == "off" and catalog["languages"], catalog["languages"])
    op_schema = next(o for o in catalog["ops"] if o["name"] == "input.send")
    check("catalog: an op carries its argument schema",
          op_schema["args"]["properties"]["text"]["type"] == "string"
          and op_schema["args"]["properties"]["queue"]["enum"] == ["now", "after_run"]
          and op_schema["precondition"] == "none", op_schema)
    cancel_schema = next(o for o in catalog["ops"] if o["name"] == "input.cancel")
    check("catalog: required arguments are named",
          cancel_schema["args"]["required"] == ["item_id"]
          and cancel_schema["precondition"] == "none", cancel_schema)

    # --- the first snapshot: empty session, one seq for every topic -----------
    snap = server.snapshot("session,swarm,lane:*")
    check("snapshot has an epoch, a seq and the session topic",
          snap["epoch"] and isinstance(snap["seq"], int) and "session" in snap["topics"]
          and isinstance(snap["topics"]["session"]["state"], dict), sorted(snap))
    check("snapshot reports the model the catalog does",
          snap["topics"]["session"]["state"]["model"] == "stub-a", snap["topics"]["session"]["state"])

    # --- stream: hello, then the ops of a turn --------------------------------
    # The snapshot the stream is about to continue: the consistency check
    # folds the ops onto exactly this state.
    baseline = server.snapshot("session")

    def got_answer(ops):
        """The turn is over when the assistant item it created is final."""
        answer = next((o for o in ops
                       if o["op"] == "item.add" and o["item"]["kind"] == "assistant"), None)
        if not answer:
            return False
        return any(o["op"] == "item.patch" and o.get("id") == answer["item"]["id"]
                   and o["patch"].get("status") in ("final", "error") for o in ops)

    wrote = {}
    collected = {}

    def stream_turn():
        cursor = f"{baseline['epoch']}.{baseline['seq']}"
        hello, ops = server.read_stream(got_answer, topics="session", since=cursor)
        collected["hello"] = hello
        collected["ops"] = ops

    reading = in_thread(stream_turn)
    time.sleep(0.3)
    status, reply, _ = server.op("input.send", {"text": "hello e2e"})
    check("input.send answers with the item id and its queue state",
          status == 200 and reply["ok"] and reply["result"]["item_id"]
          and reply["result"]["queued"] and reply["result"]["blocked"] is None, reply)
    wrote["id"] = reply["result"]["item_id"]
    join(reading, 45)
    ops = collected.get("ops") or []
    hello = collected.get("hello")
    check("the stream's first frame is hello, at the snapshot's cursor",
          hello and hello["epoch"] == baseline["epoch"] and hello["seq"] == baseline["seq"], hello)
    check("input.send's item appears (item.add, queued)",
          any(o["op"] == "item.add" and o["item"]["id"] == wrote["id"]
              and o["item"]["kind"] == "user" and o["item"]["status"] == "queued" for o in ops),
          ops[:3])
    assistant = [o for o in ops if o["op"] == "item.add" and o["item"]["kind"] == "assistant"]
    check("the answer is its own item", len(assistant) == 1, assistant)
    answer_id = assistant[0]["item"]["id"] if assistant else None
    check("the answer's text arrives as item.append",
          any(o["op"] == "item.append" and o.get("id") == answer_id and o["field"] == "text"
              for o in ops), ops[-4:])
    appends = [o for o in ops if o["op"] == "item.append" and o.get("id") == answer_id]
    text = "".join(o["text"] for o in appends)
    check("the stub's answer streams back", text == "ok: hello e2e", text)
    check("every op carries seq, ts and topic",
          all(isinstance(o.get("seq"), int) and isinstance(o.get("ts"), int)
              and o.get("topic") == "session" for o in ops
              if o["op"] not in ("hello", "stream.reset")), ops[:2])
    check("appends are coalesced, not one op per delta",
          len(appends) <= 3, len(appends))

    # --- snapshot + stream consistency ----------------------------------------
    items, state = apply_ops(baseline["topics"]["session"]["items"],
                             baseline["topics"]["session"]["state"], ops)
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
          reply["ok"] and item_id != first_id, reply)
    status, reply, _ = server.op("input.cancel", {"item_id": item_id})
    check("a queued input can be cancelled", status == 200 and reply["ok"], reply)
    status, reply, _ = server.op("input.cancel", {"item_id": item_id})
    check("cancelling it again -> already_sent",
          reply["ok"] is False and reply["error"]["code"] == "already_sent", reply)
    status, reply, _ = server.op("input.cancel", {"item_id": first_id})
    check("input already drained -> already_sent",
          reply["ok"] is False and reply["error"]["code"] == "already_sent", reply)
    snap = server.snapshot("session")
    cancelled = [i for i in snap["topics"]["session"]["items"] if i["id"] == item_id]
    check("the cancelled item reads as cancelled in the snapshot",
          cancelled and cancelled[0]["status"] == "cancelled", cancelled)
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
    snap = server.snapshot("session")
    check("the state follows the command", snap["topics"]["session"]["state"]["thinking"] == "high",
          snap["topics"]["session"]["state"]["thinking"])
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
    check("items: `before` pages back from an id",
          status == 200 and all(i["id"] != older for i in page2["items"]), page2)

    # --- preconditions and the debug reads --------------------------------------------
    status, reply, _ = server.op("input.send", {"text": "SLOW while switching"})
    status, reply, _ = server.op("session.new", {})
    check("session.new while busy -> not_quiescent",
          reply["ok"] is False and reply["error"]["code"] == "not_quiescent", reply)
    status, reply, _ = server.op("context.compact", {})
    check("context.compact while busy -> busy",
          reply["ok"] is False and reply["error"]["code"] == "busy", reply)
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
        eval_gate_check(server)
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
