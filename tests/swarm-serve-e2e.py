#!/usr/bin/env python3
"""swarm-serve-e2e.py — `evo-swarm serve` on the new protocol, end to end.

The headless swarm, driven exactly as evo-gui drives it: one ready file, one
bearer token, and CONTRACT.md's protocol for the coordinator *and* for its
lanes.  Nothing here knows a lane's address: lanes have their own ready files,
owned by the coordinator, and reach a client only as the topics `lane:1`,
`lane:2` … mirrored from each lane's own /snapshot and /stream into the
coordinator's one op log.  No terminal, no relay, no second port.

The "model" is tests/stub-messages.py, which scripts the coordinator and its
lanes from what they are sent (`CALL <tool> {json}` becomes that tool call), so
this needs nothing but python3 and a built evo-agent + evo-swarm.

Covers:
  * the ready file (0600, atomically written) and /health;
  * the coordinator's protocol: /snapshot with session,swarm,lane:*; /ops;
    /stream with one subscription carrying every lane; /items paging;
  * the swarm topic: workers, one row per lane, its state, model, context,
    task, restarts, last item — and busy/waiting_on_lanes while a lane works;
  * the lane:N topics: the lane's own items and state, mirrored and republished
    under the coordinator's cursor;
  * a delegated task streams on lane:N through that one subscription;
  * a lane killed with SIGKILL comes back: a lane_event item (crashed, then
    restarted), topic.reset lane:N, and the exact session it was on;
  * run.interrupt scope swarm stops the coordinator and every lane;
  * /ops is idempotent per rid; a bad token is 401; an unknown op is refused
    with a code, never a crash;
  * server.shutdown stops every lane, leaves nothing behind, and exits 0.

Usage: tests/swarm-serve-e2e.py [build-dir]     (exit 0 on success; Unix only)
"""

import http.client
import json
import os
import signal
import shutil
import socket
import subprocess
import sys
import tempfile
import threading
import time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BUILD = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else os.path.join(ROOT, "build")
SWARM = os.path.join(BUILD, "evo-swarm")
EVO = os.path.join(BUILD, "evo-agent")
SECRET = "swarm-serve-e2e-secret-4c1f"
LANES = 2
# DELAY2 makes the lane wait before it "thinks", then SLOW streams 60 deltas a
# tenth of a second apart: an ~8 s working window to observe and to watch live.
TASK = "DELAY2 SLOW e2e lane work"
RECORD = "the report is the item"

passed = 0
failed = 0


def check(name, ok, detail=""):
    global passed, failed
    if ok:
        passed += 1
        print(f"ok   {name}", flush=True)
    else:
        failed += 1
        print(f"FAIL {name} {detail}", flush=True)


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def pick(obj, *keys):
    """The first of KEYS present (and not None) in OBJ, else None."""
    if not isinstance(obj, dict):
        return None
    for key in keys:
        if key in obj and obj[key] is not None:
            return obj[key]
    return None


def wait_for(predicate, timeout=60, interval=0.1):
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            value = predicate()
        except Exception:
            value = None
        if value:
            return value
        time.sleep(interval)
    return None


# --------------------------------------------------------------------------
# The protocol, as a client speaks it.

class Topic:
    """One topic as a client holds it: state plus items, applied op by op."""

    def __init__(self, name):
        self.name = name
        self.state = {}
        self.items = []                 # oldest first
        self.order = {}

    def item(self, item_id):
        return self.order.get(item_id)

    def add(self, item, after):
        item_id = item.get("id")
        if item_id in self.order:
            self.order[item_id].update(item)
            return
        self.order[item_id] = item
        if after and after in self.order:
            index = self.items.index(self.order[after]) + 1
            self.items.insert(index, item)
        else:
            self.items.append(item)

    def apply(self, op):
        kind = op.get("op")
        if kind == "item.add":
            self.add(op.get("item") or {}, op.get("after"))
        elif kind == "item.append":
            item = self.order.get(op.get("id"))
            if item is not None:
                field = op.get("field") or "text"
                item[field] = (item.get(field) or "") + (op.get("text") or "")
        elif kind == "item.patch":
            item = self.order.get(op.get("id"))
            if item is not None:
                item.update(op.get("patch") or {})
        elif kind == "item.remove":
            item = self.order.pop(op.get("id"), None)
            if item is not None:
                self.items.remove(item)
        elif kind == "state.patch":
            self.state.update(op.get("patch") or {})
        elif kind in ("topic.reset", "stream.reset"):
            return "reset"
        return kind

    def texts(self):
        return " ".join(str(i.get("text") or "") for i in self.items)

    def kinds(self, kind):
        return [i for i in self.items if i.get("kind") == kind]


class View:
    """Every topic one subscription carries."""

    def __init__(self, names):
        self.topics = {name: Topic(name) for name in names}
        self.resets = []                # (topic, reason) in arrival order
        self.ops = []                   # every op seen, in order

    def apply(self, op):
        self.ops.append(op)
        name = op.get("topic")
        topic = self.topics.get(name)
        if topic is None:
            topic = self.topics.setdefault(name, Topic(name))
        if topic.apply(op) == "reset":
            self.resets.append((name, op.get("reason")))
        return topic

    def find(self, kind, topic=None, **fields):
        for name, held in self.topics.items():
            if topic and name != topic:
                continue
            for item in held.items:
                if item.get("kind") != kind:
                    continue
                if all(item.get(k) == v for k, v in fields.items()):
                    return item
        return None


class Client:
    """The coordinator, as a client reaches it: one token, one port, HTTP."""

    def __init__(self, port, token):
        self.port = port
        self.token = token
        self.rid = 0

    def request(self, method, path, body=None, token=None, headers=None, timeout=30):
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=timeout)
        auth = self.token if token is None else token
        h = {} if auth == "" else {"Authorization": f"Bearer {auth}"}
        h.update(headers or {})
        data = None
        if body is not None:
            data = json.dumps(body).encode()
            h["Content-Type"] = "application/json"
        conn.request(method, path, body=data, headers=h)
        resp = conn.getresponse()
        raw = resp.read().decode(errors="replace")
        conn.close()
        try:
            return resp.status, json.loads(raw)
        except ValueError:
            return resp.status, raw

    def get(self, path, **kw):
        return self.request("GET", path, **kw)

    def op(self, name, args=None, op_rid=None):
        self.rid += 1
        body = {"rid": op_rid or f"e2e-{self.rid}", "op": name, "args": args or {}}
        return self.request("POST", "/ops", body=body)

    def snapshot(self, topics="session,swarm,lane:*", items=200):
        status, body = self.get(f"/snapshot?topics={topics}&items={items}")
        return body if status == 200 else None

    def topic_state(self, name):
        snap = self.snapshot(topics=name, items=1) or {}
        return (snap.get("topics") or {}).get(name, {}).get("state") or {}

    def stream(self, path, timeout=60):
        """Yield (id, event, data) from an SSE GET until it ends or times out."""
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=timeout)
        try:
            h = {"Authorization": f"Bearer {self.token}"}
            conn.request("GET", path, headers=h)
            resp = conn.getresponse()
            if resp.status != 200:
                yield (None, "http-error", resp.status)
                return
            frame = {"id": None, "event": None, "data": None}
            while True:
                try:
                    line = resp.fp.readline()
                except (socket.timeout, TimeoutError, OSError):
                    break
                if not line:
                    break
                line = line.decode(errors="replace").rstrip("\n")
                if line == "":
                    if frame["event"]:
                        try:
                            yield (frame["id"], frame["event"], json.loads(frame["data"]))
                        except ValueError:
                            yield (frame["id"], frame["event"], frame["data"])
                    frame = {"id": None, "event": None, "data": None}
                elif line.startswith(":"):
                    continue
                else:
                    key, _, value = line.partition(": ")
                    frame[key] = value
        finally:
            conn.close()


class Collector(threading.Thread):
    """Follow one subscription in the background, applying its ops to a VIEW."""

    def __init__(self, client, topics="session,swarm,lane:*", timeout=120):
        super().__init__(daemon=True)
        self.client = client
        self.path = f"/stream?topics={topics}"
        self.timeout = timeout
        self.view = View(topics.split(","))
        self.hello = None
        self.error = None
        self.stop = threading.Event()

    def run(self):
        try:
            for _, etype, data in self.client.stream(self.path, timeout=self.timeout):
                if self.stop.is_set():
                    return
                if etype == "http-error":
                    self.error = f"HTTP {data}"
                    return
                if etype != "op" or not isinstance(data, dict):
                    continue
                if data.get("op") == "hello":
                    self.hello = data
                    continue
                self.view.apply(data)
        except Exception as e:                          # a stream that dies is the test's to see
            self.error = e


class Swarm:
    """One `evo-swarm serve` process: its ready file, its token, its log."""

    def __init__(self, home, proj, env, ready_file, workers=LANES, resume=False):
        self.ready_file = ready_file
        args = [SWARM, "serve", "--port", "0", "--ready-file", ready_file,
                "--watch-stdin", "--workers", str(workers), "--evo", EVO]
        if resume:
            args.append("--resume")
        self.args = args
        self.log_path = ready_file + ".log"
        self.log = open(self.log_path, "w")
        self.proc = subprocess.Popen(args, cwd=proj, env=env, stdin=subprocess.PIPE,
                                     stdout=self.log, stderr=subprocess.STDOUT,
                                     start_new_session=True)
        self.ready = None
        self.client = None

    def wait_ready(self, timeout=120):
        deadline = time.time() + timeout
        while time.time() < deadline:
            if os.path.exists(self.ready_file) and os.path.getsize(self.ready_file):
                try:
                    self.ready = json.load(open(self.ready_file))
                except ValueError:
                    self.ready = None
                if self.ready:
                    self.client = Client(self.ready["port"], self.ready["token"])
                    return True
            if self.proc.poll() is not None:
                return False
            time.sleep(0.1)
        return False

    def wait(self, timeout=30):
        try:
            return self.proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            return None

    def kill(self):
        if self.proc.poll() is None:
            try:
                os.killpg(self.proc.pid, signal.SIGKILL)
            except OSError:
                self.proc.kill()
            self.proc.wait(timeout=10)

    def log_tail(self, lines=60):
        try:
            with open(self.log_path, errors="replace") as f:
                return "".join(f.readlines()[-lines:])
        except OSError:
            return ""


def lane_dirs(home):
    return sorted(
        (d for d in _lane_dir_candidates(home)),
        key=lambda d: int(d.rsplit("-", 1)[1]))


def _lane_dir_candidates(home):
    import glob
    return glob.glob(os.path.join(home, "swarm", "*", "lane-*"))


def lane_ready(home, n):
    """One lane's ready file — its own address, its own session."""
    import glob
    for path in glob.glob(os.path.join(home, "swarm", "*", f"lane-{n}", "ready.json")):
        try:
            return json.load(open(path))
        except (OSError, ValueError):
            return None
    return None


def lane_session_file(home, n):
    """The lane's journal, as its ready file named it — the file a restart
    must resume, and the proof that the session reached disk."""
    ready = lane_ready(home, n) or {}
    path = (ready.get("session") or {}).get("path")
    return path if path and os.path.exists(path) else None


def lane_pids(home):
    """Every lane process still alive, read from the lanes' own ready files."""
    pids = []
    for directory in lane_dirs(home):
        path = os.path.join(directory, "ready.json")
        try:
            pids.append(json.load(open(path))["pid"])
        except (OSError, ValueError, KeyError):
            pass
    return pids


def alive(pid):
    try:
        os.kill(pid, 0)
        return True
    except OSError:
        return False


def swarm_rows(state):
    lanes = (state or {}).get("lanes") or []
    return {int(pick(row, "n") or 0): row for row in lanes}


def lane_state(client, n):
    return client.topic_state(f"lane:{n}")


# --------------------------------------------------------------------------

def run_checks(swarm, stub, home, work, proj):
    client = swarm.client

    # --- the ready file and /health ----------------------------------------
    mode = os.stat(swarm.ready_file).st_mode & 0o777
    check("the ready file is mode 0600", mode == 0o600, oct(mode))
    check("the ready file names the port, the url, the token and the session",
          all(swarm.ready.get(k) for k in ("port", "url", "token", "epoch", "pid"))
          and (swarm.ready.get("session") or {}).get("path"),
          swarm.ready)
    check("...and the program", swarm.ready.get("program") == "evo-swarm",
          swarm.ready.get("program"))
    status, health = client.get("/health")
    check("GET /health is 200 and says evo-swarm",
          status == 200 and health.get("program") == "evo-swarm", health)
    check("...with the epoch and pid its ready file named",
          health.get("epoch") == swarm.ready["epoch"]
          and health.get("pid") == swarm.ready["pid"], health)

    # --- auth --------------------------------------------------------------
    check("no token -> 401", client.get("/snapshot?topics=session", token="")[0] == 401)
    check("wrong token -> 401", client.get("/snapshot?topics=session", token="x")[0] == 401)
    status, _ = client.request("POST", "/ops", body={"rid": "auth-1", "op": "input.send",
                                                     "args": {"text": "hi"}}, token="")
    check("no token -> 401 on /ops", status == 401, status)
    check("an unknown route is 404", client.get("/lanes")[0] == 404)

    # --- /ops --------------------------------------------------------------
    status, reply = client.op("nonsense.op", {}, op_rid="bad-op")
    check("/ops refuses an unknown op with a code, HTTP 200",
          status == 200 and reply.get("ok") is False
          and (reply.get("error") or {}).get("code"), reply)
    status, reply = client.op("input.send", {"text": "hello"}, op_rid="dupe")
    first = reply
    status, again = client.op("input.send", {"text": "hello"}, op_rid="dupe")
    check("a retried rid is answered from the cache, not acted on twice",
          again == first and first.get("ok"), (first, again))
    check("...and the reply carries an item id and a cursor",
          first.get("ok") and (first.get("result") or {}).get("item_id")
          and isinstance(first.get("seq"), int), first)

    # --- the swarm topic and the lane topics --------------------------------
    lanes = wait_for(lambda: _all_idle(client), 120)
    check(f"the snapshot carries session, swarm and lane:1..{LANES}",
          lanes is not None, client.snapshot())
    state = client.topic_state("swarm")
    rows = swarm_rows(state)
    check("the swarm topic names its id and workers",
          state.get("id") and state.get("workers") == LANES, state)
    check("...one row per lane, numbered 1..N, every one idle",
          sorted(rows) == list(range(1, LANES + 1))
          and all(row.get("state") == "idle" for row in rows.values()), state)
    check("...with a model, a context and a last item per lane",
          all(row.get("model") is not None and row.get("context") is not None
              for row in rows.values()), state)
    check("...and a live pid each (a lane is a process)",
          all(row.get("pid") and alive(row["pid"]) for row in rows.values()), state)
    check("the swarm is not busy and not waiting", not state["status"].get("busy")
          and not state["status"].get("waiting_on_lanes"), state["status"])
    one = lane_state(client, 1)
    check("lane 1's own topic carries its items and its state",
          one.get("status") == "idle" and one.get("session", {}).get("id"), one)

    # --- one subscription carries every lane --------------------------------
    collector = Collector(client)
    collector.start()
    check("the stream opens with hello {epoch, seq}",
          wait_for(lambda: collector.hello, 20) is not None, collector.hello)

    # --- a delegated task ---------------------------------------------------
    status, reply = client.op("input.send",
                              {"text": f'CALL delegate {{"lane":1,"task":"{TASK}"}}'})
    check("the coordinator accepts the prompt", reply.get("ok"), reply)
    delegated = wait_for(lambda: stub.find("lane 1", "e2e lane work"), 60)
    check("the coordinator delegated it to lane 1",
          delegated and delegated.get("system_has_report_note"), delegated)
    working = wait_for(lambda: (_lane_row(client, 1).get("state") == "working"
                                and _lane_row(client, 1)), 60)
    check("the swarm topic shows lane 1 working on the task",
          working is not None and TASK in (working.get("task") or ""), working)
    busy = wait_for(lambda: client.topic_state("swarm")["status"].get("busy") == 1, 30)
    check("...and the swarm is busy while it works", busy is not None)
    check("...and waiting_on_lanes, because the coordinator has nothing to do",
          wait_for(lambda: client.topic_state("swarm")["status"]
                   .get("waiting_on_lanes"), 30) is not None)
    check("...and the lane's own topic says running",
          lane_state(client, 1).get("status") == "running", lane_state(client, 1))
    streamed = wait_for(lambda: "slow1" in collector.view.topics["lane:1"].texts(), 60)
    check("the delegated work streams on topic lane:1 through that one subscription",
          streamed is not None, collector.view.topics["lane:1"].texts()[:200])
    check("...as item.add + item.append ops, not one row per delta",
          any(op.get("op") == "item.append" and op.get("topic") == "lane:1"
              for op in collector.view.ops),
          sorted({op.get("op") for op in collector.view.ops}))

    # --- paging, both ways ---------------------------------------------------
    lane_items = client.snapshot(topics="lane:1", items=2)["topics"]["lane:1"]
    ids = [i.get("id") for i in lane_items.get("items") or []]
    check("a lane's snapshot takes the newest N items", len(ids) <= 2 and ids, ids)
    status, body = client.get(f"/items?topic=lane:1&before={ids[0]}&limit=5")
    check("GET /items pages backwards through a lane",
          status == 200 and (body.get("items") or []), body)
    status, body = client.get(f"/items/{ids[-1]}?topic=lane:1")
    check("GET /items/<id> returns one item whole",
          status == 200 and (body.get("item") or {}).get("id") == ids[-1], body)
    check("paging a topic nobody registered is empty, not an error",
          client.get("/items?topic=nope&limit=5")[0] in (200, 404))

    # --- killing a lane -----------------------------------------------------
    # Wait for the run to settle first: a session reaches disk when its first
    # assistant message lands, which is also when "resumes its exact session"
    # is a claim worth making.
    settled = wait_for(lambda: (_lane_row(client, 1)
                                if _lane_row(client, 1).get("state") == "idle"
                                and lane_session_file(home, 1) else None), 120)
    check("the lane's run settles and its session reaches disk",
          settled is not None, _lane_row(client, 1))
    before = lane_state(client, 1)
    session_id = (before.get("session") or {}).get("id")
    session_file = lane_session_file(home, 1)
    pid = _lane_row(client, 1)["pid"]
    restarts = _lane_row(client, 1)["restarts"]
    os.kill(pid, signal.SIGKILL)
    check("the lane's process is gone", wait_for(lambda: not alive(pid), 30))
    back = wait_for(lambda: (_lane_row(client, 1)
                             if _lane_row(client, 1).get("state") == "idle"
                             and _lane_row(client, 1).get("restarts") > restarts
                             else None), 180)
    check("the coordinator restarts it and it comes back idle", back is not None,
          _lane_row(client, 1))
    check("...with a new process", back and back["pid"] != pid, back)
    check("...on the exact session it was on",
          (lane_state(client, 1).get("session") or {}).get("id") == session_id
          and lane_session_file(home, 1) == session_file,
          (lane_state(client, 1).get("session"), session_id, session_file))
    resumed = wait_for(lambda: (_lane_row(client, 1)
                                if client.snapshot(topics="lane:1", items=200
                                                   )["topics"]["lane:1"]["items"]
                                else None), 30)
    items = (client.snapshot(topics="lane:1", items=200)["topics"]["lane:1"]["items"])
    check("...with the work it had already done still in its items",
          any("e2e lane work" in json.dumps(i) for i in items),
          json.dumps(items)[-300:])
    check("...and a topic.reset lane:1, so every client re-snapshots it",
          wait_for(lambda: any(t == "lane:1" and r == "lane_restarted"
                               for t, r in collector.view.resets), 60) is not None,
          collector.view.resets)
    event = wait_for(lambda: collector.view.find("lane_event", topic="session",
                                                 lane=1, event="crashed"), 30)
    check("...and the coordinator is told, as a lane_event item",
          event is not None, [i for i in collector.view.topics["session"].items
                              if i.get("kind") == "lane_event"][-3:])
    check("...naming the crash and the restart that followed it",
          event and collector.view.find("lane_event", topic="session",
                                        lane=1, event="restarted"), event)
    check("...which it hears as a message with a :lane-event origin",
          _lane_event_origins(client), _lane_event_origins(client)[:1])

    # --- run.interrupt with scope swarm -------------------------------------
    status, reply = client.op("input.send",
                              {"text": f'CALL delegate {{"lane":2,"task":"{TASK}"}}'})
    check("a second task is delegated to lane 2", reply.get("ok"), reply)
    check("...and lane 2 starts working",
          wait_for(lambda: _lane_row(client, 2).get("state") == "working", 60) is not None)
    # The coordinator takes a slow task of its own too: run.interrupt answers
    # what it actually stopped, and an idle session is not something a client
    # can show as stopped (CONTRACT §5.5).
    status, reply = client.op("input.send", {"text": "SLOW coordinator work"})
    check("the coordinator takes work of its own", reply.get("ok"), reply)
    check("...and is running when the swarm is interrupted",
          wait_for(lambda: client.topic_state("session").get("status") == "running",
                   30) is not None, client.topic_state("session").get("status"))
    status, reply = client.op("run.interrupt", {"scope": "swarm"})
    check("run.interrupt scope swarm is answered",
          status == 200 and reply.get("ok"), reply)
    interrupted = (reply.get("result") or {}).get("interrupted") or []
    check("...naming the session and every lane", "session" in interrupted
          and any(str(i).startswith("lane:") for i in interrupted), interrupted)
    check("...and every lane stops",
          wait_for(lambda: all(_lane_row(client, n).get("state") in ("idle", "down")
                               for n in range(1, LANES + 1)), 60) is not None,
          [_lane_row(client, n) for n in range(1, LANES + 1)])
    check("...while the swarm says it is no longer busy",
          wait_for(lambda: not client.topic_state("swarm")["status"].get("busy"), 30)
          is not None)
    # The coordinator is told a human stopped the lanes — in its own
    # transcript, as after-run input, so a client sees why they stopped even
    # though the run that would have read a notice was the one interrupted.
    note = wait_for(lambda: collector.view.find("human_action", topic="session",
                                                action="interrupt"), 30)
    check("...and the human's stop reaches the coordinator's transcript",
          note is not None, [i for i in collector.view.topics["session"].items
                             if i.get("kind") == "user"][-3:])
    check("...as a human_action item naming the lanes that were stopped",
          note is not None and 1 in (note.get("lanes") or [])
          and 2 in (note.get("lanes") or []), note)
    check("...which was queued as after-run input, not steered into a run",
          note is not None and note.get("queue") == "after_run", note)

    # --- a lane's report is an item, and not a notice too -------------------
    status, reply = client.op(
        "input.send",
        {"text": "CALL delegate " + json.dumps(
            {"lane": 1, "task": "CALL report " + json.dumps({"done": RECORD,
                                                             "next": "nothing"})})})
    check("a task whose lane reports is delegated", reply.get("ok"), reply)
    item = wait_for(lambda: collector.view.find("lane_report", topic="session"), 60)
    check("...and the report reaches the coordinator as a lane_report item",
          item is not None and item.get("done") == RECORD, item)
    check("...once, as the item: not also as a notice saying the same thing",
          not any(RECORD in str(i.get("text") or "")
                  for i in collector.view.topics["session"].items
                  if i.get("kind") == "notice"),
          [i for i in collector.view.topics["session"].items
           if i.get("kind") == "notice"][-3:])

    collector.stop.set()


def _all_idle(client):
    rows = swarm_rows(client.topic_state("swarm"))
    if len(rows) != LANES:
        return None
    return rows if all(r.get("state") == "idle" for r in rows.values()) else None


def _lane_row(client, n):
    return swarm_rows(client.topic_state("swarm")).get(n) or {}


def _lane_event_origins(client):
    """The coordinator's own journal: a lane's crash is a message whose origin
    says :lane-event, so nothing has to parse the prose a model reads."""
    status, body = client.get("/debug/journal")
    if status != 200:
        return []
    entries = body.get("entries") or []
    out = []
    for entry in entries:
        origin = entry.get("origin") or (entry.get("message") or {}).get("origin")
        if isinstance(origin, dict) and origin.get("kind") == "lane_event":
            out.append(origin)
    return out


def main():
    global failed
    if not (os.access(SWARM, os.X_OK) and os.access(EVO, os.X_OK)):
        print(f"swarm-serve-e2e: no binaries in {BUILD} (make build first)")
        return 1
    work = tempfile.mkdtemp(prefix="evo-swarm-serve-e2e-")
    home = os.path.join(work, "home")
    proj = os.path.join(work, "proj")
    os.makedirs(home)
    os.makedirs(proj)
    stub = Stub()
    with open(os.path.join(home, "init.lisp"), "w") as f:
        f.write(f'(evo:register-provider :stub :base-url "http://127.0.0.1:{stub.port}" '
                f':api-key "{SECRET}")\n'
                '(evo:register-model "stub-a" :provider :stub :context-window 200000 '
                ':max-output 8000 :effort t)\n'
                '(evo:set-setting :model "stub-a")\n')

    # A lane must not inherit this process's supervision or session: a foreign
    # EVO_SESSIONS_DIR would put the swarm's journals in someone else's home.
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(("EVO_", "ANTHROPIC_"))}
    env.update(HOME=home, EVO_HOME=home, EVO_BINARY=EVO, TERM="xterm-256color")

    swarm = Swarm(home, proj, env, os.path.join(work, "ready.json"))
    try:
        if not swarm.wait_ready():
            print("swarm-serve-e2e: `evo-swarm serve` did not come up "
                  f"(exit {swarm.proc.poll()})")
            failed_early = True
        else:
            failed_early = False
            run_checks(swarm, stub, home, work, proj)

        # --- shutdown ---------------------------------------------------------
        dirs = lane_dirs(home)
        check(f"{LANES} lane directories", len(dirs) == LANES, dirs)
        pids = lane_pids(home)
        check("every lane published a ready file of its own, all alive",
              len(pids) == LANES and all(alive(p) for p in pids), pids)
        check("the coordinator owns them: they are not supervised by it",
              all(p != swarm.ready["pid"] for p in pids), pids)
        status, reply = swarm.client.op("server.shutdown", {})
        check("server.shutdown is accepted", status == 200 and reply.get("ok"), reply)
        code = swarm.wait(timeout=30)
        check("the swarm exits 0 (supervisor included)", code == 0, code)
        check("every lane process is gone with it",
              wait_for(lambda: not any(alive(p) for p in pids), 30),
              [(p, alive(p)) for p in pids])
        check("the ready file is removed on clean exit",
              not os.path.exists(swarm.ready_file))
        check("no lane is orphaned (a lane's stdin is the coordinator's pipe)",
              wait_for(lambda: not any(alive(p) for p in lane_pids(home)), 20))
        leaks = _secret_leaks(home)
        check("no provider secret is written to a journal, log or lane file",
              not leaks, leaks)
    except BaseException as e:
        failed += 1
        print(f"FAIL aborted: {e!r}")
    finally:
        swarm.kill()
        stub.proc.kill()
        swarm.log.close()
    if failed:
        print(swarm.log_tail())
    else:
        shutil.rmtree(work, ignore_errors=True)
    print(f"\nswarm-serve-e2e: {passed} passed, {failed} failed")
    return 1 if failed else 0


def _secret_leaks(home):
    leaks = []
    for root, _dirs, files in os.walk(home):
        for name in files:
            path = os.path.join(root, name)
            if path == os.path.join(home, "init.lisp"):
                continue
            try:
                if SECRET in open(path, errors="replace").read():
                    leaks.append(path)
            except OSError:
                pass
    return leaks


class Stub:
    """tests/stub-messages.py: the scripted model both agents talk to."""

    def __init__(self):
        self.port = free_port()
        self.proc = subprocess.Popen([sys.executable,
                                      os.path.join(ROOT, "tests", "stub-messages.py"),
                                      str(self.port)], stdout=subprocess.PIPE, text=True)
        assert self.proc.stdout.readline().startswith("stub listening")

    def requests(self):
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=10)
        conn.request("GET", "/_requests")
        data = json.loads(conn.getresponse().read())
        conn.close()
        return data

    def find(self, role, needle):
        for r in self.requests():
            if r["role"] == role and needle in r["last_user"]:
                return r
        return None


if __name__ == "__main__":
    sys.exit(main())
