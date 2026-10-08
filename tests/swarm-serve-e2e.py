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
  * a client's own `--prompt-note` file: in the coordinator's system prompt
    and in each lane's, because a lane's transcript is that client's to show
    too (the one check here that talks to a lane's own port, which its ready
    file names — a lane's prompt is the only place its note can be read);
  * the swarm topic: workers, one row per lane, its state, model, context,
    task, restarts, last item — and busy/waiting_on_lanes while a lane works;
  * the lane:N topics: the lane's own items and state, mirrored and republished
    under the coordinator's cursor;
  * a lane topic a client snapshotted *before* that lane existed is announced
    with topic.reset lane:N when the lane comes up, so a client that held it
    empty is not left holding `{}` for the lane's whole life;
  * a delegated task streams on lane:N through that one subscription;
  * a lane killed with SIGKILL comes back: a lane_event item (crashed, then
    restarted), topic.reset lane:N, and the exact session it was on;
  * run.interrupt scope swarm stops the coordinator and every lane;
  * the runtime lane count, `/lanes N` (POST command.run): a count that is not
    one is refused `:invalid`; growing the pool while it runs — the new lane's
    topic, its ready file and its process, all of it inside the one
    subscription a client already holds, and the re-snapshot that client takes
    on the reset; a count the pool already has, or one below it, is a no-op
    that stops nothing; the coordinator's own run guards the resize while a
    lane being busy does not, and a lane that is working is not restarted by a
    growth; and every lane's own prompt note — the lane that joined and the
    lanes already there — carries the new count;
  * a resumed swarm restores the roster its session recorded — the pool
    `/lanes N` grew — whether the launch names no `--workers` at all, a lower
    one (which must not shrink it) or a higher one (which grows it);
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

from clean_env import clean  # the environment a test's children start from

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BUILD = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else os.path.join(ROOT, "build")
SWARM = os.path.join(BUILD, "evo-swarm")
EVO = os.path.join(BUILD, "evo-agent")
SECRET = "swarm-serve-e2e-secret-4c1f"
LANES = 2
# The lane count `/lanes N` grows the pool to: bigger than LANES, so the growth
# is a real change, and bigger than the lower `--workers 1` a resumed swarm is
# launched with, so that resume proves the roster did not shrink back.
GROWN = LANES + 1
# DELAY2 makes the lane wait before it "thinks", then SLOW streams 60 deltas a
# tenth of a second apart: an ~8 s working window to observe and to watch live.
TASK = "DELAY2 SLOW e2e lane work"
RECORD = "the report is the item"
# What the client's own --prompt-note file says, word for word: the marker in a
# system prompt is the proof the flag reached that process.
NOTE_MARKER = "NOTE-MARKER-GUI"

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

    def resnapshot(self, name, state, items):
        """What a client does on `topic.reset NAME` (docs/serve.md §5.3):
        replace what it held for NAME with a fresh snapshot of that topic, state
        and items.  The stream does not carry the snapshot — the client asks for
        it — so this is the only way a topic a client held empty comes to hold a
        lane whole."""
        topic = self.topics.setdefault(name, Topic(name))
        topic.state = dict(state or {})
        topic.items = []
        topic.order = {}
        for item in items or []:
            topic.add(item, None)
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

    def __init__(self, client, topics="session,swarm,lane:*", timeout=120, since=None):
        super().__init__(daemon=True)
        self.client = client
        self.path = f"/stream?topics={topics}"
        if since:
            # From a snapshot's cursor, the way a client that took a snapshot
            # follows it: only what happened since.
            self.path += f"&since={since}"
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

    def __init__(self, home, proj, env, ready_file, workers=LANES, resume=False,
                 notes=()):
        self.ready_file = ready_file
        # `--workers` is left off the command line entirely when WORKERS is
        # None: what a launch with no flag does (the recorded roster, else the
        # config default) is one of the things a resume has to be read on.
        args = [SWARM, "serve", "--port", "0", "--ready-file", ready_file,
                "--watch-stdin"]
        if workers is not None:
            args += ["--workers", str(workers)]
        args += ["--evo", EVO]
        for note in notes:
            args += ["--prompt-note", note]
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


def hold_lanes_from_the_start(swarm):
    """The client evo-gui is: snapshot `lane:*` the moment the swarm is up, then
    stream from that cursor.

    A lane's topic is registered — empty — when the swarm starts; the lane's
    process does not exist until later, and a resumed swarm's lanes are new
    processes too.  So everything a lane turns out to be (its status, model,
    context, its items) has to reach a client that is *already holding* that
    topic: the protocol's way is `topic.reset` `lane:N`, on which the client
    re-snapshots that one topic (docs/serve.md §5.3).  A client that is never
    told keeps `{}` for the lane's whole life — no model, context or cache
    chips, and "Lane N hasn't been given work yet" over work the lane's own
    session is holding.
    """
    names = [f"lane:{n}" for n in range(1, LANES + 1)]
    snapshot = swarm.client.snapshot(topics=",".join(names), items=1) or {}
    held = Collector(swarm.client, ",".join(names), timeout=180,
                     since=f"{snapshot.get('epoch')}.{snapshot.get('seq')}")
    # The client's own view starts as the snapshot left it.
    for name, topic in (snapshot.get("topics") or {}).items():
        mine = held.view.topics.setdefault(name, Topic(name))
        mine.state.update(topic.get("state") or {})
        for item in topic.get("items") or []:
            mine.add(item, None)
    held.names = names
    held.snapshot = snapshot
    held.start()
    return held


def check_held_lanes_are_announced(swarm, held):
    """Every lane a client is already holding empty has to be announced."""
    late = [name for name in held.names
            if ((held.snapshot.get("topics") or {}).get(name) or {}).get("state")]
    for name in late:
        print(f"note {name} was already up when the client snapshotted it", flush=True)
    watched = [name for name in held.names if name not in late]
    if not watched:
        check("a lane topic is still empty when the client snapshots lane:*",
              False, f"every lane was up already: {held.names}")
        return
    documented = {"session_switched", "leaf_moved", "lane_restarted", "swarm_switched"}
    announced = wait_for(lambda: all(any(topic == name for topic, _ in held.view.resets)
                                     for name in watched), 60)
    reasons = {name: sorted({reason for topic, reason in held.view.resets if topic == name})
               for name in watched}
    check("a client holding a lane topic empty is told the lane came up",
          announced is not None, reasons)
    if announced is None:
        return
    check("...with a reason docs/serve.md gives a topic.reset",
          all(reasons[name] and set(reasons[name]) <= documented for name in watched), reasons)
    for name in watched:
        # What a client does on topic.reset: re-snapshot that topic, and hold
        # the lane it was never told about before.
        fresh = (swarm.client.snapshot(topics=name, items=1) or {}).get("topics") or {}
        mine = held.view.topics[name]
        mine.state.update((fresh.get(name) or {}).get("state") or {})
        check(f"{name} is whole in the client that re-snapshotted it",
              bool(mine.state.get("status")) and bool((mine.state.get("session") or {}).get("id")),
              mine.state)


# --------------------------------------------------------------------------

def prompt_note_check(swarm, home):
    """A client's own system-prompt note, in the coordinator and in every lane.

    evo-gui renders the agent's output itself and says so with `--prompt-note`,
    a serve flag that is generic on evo's side: a markdown file, registered in
    the kernel's prompt-note registry.  The coordinator has it because the
    launch named it; each lane has it because the swarm puts the same flag on
    the lane's own command line — the lane's transcript is shown by the same
    client.  This reads a lane's prompt through the lane's own port (the ready
    file the coordinator owns names it): a lane's prompt is the only place its
    note can be read, and the topic it publishes carries items, not prompts."""
    question = {"code": f'(if (search "{NOTE_MARKER}" (evo.kernel:build-system-prompt nil)) :in :absent)'}
    status, reply = swarm.client.op("eval", question, op_rid="note-coordinator")
    check("--prompt-note: the coordinator's system prompt carries it",
          status == 200 and (reply.get("result") or {}).get("values") == [":in"], reply)
    for n in range(1, LANES + 1):
        ready = lane_ready(home, n) or {}
        if not ready.get("port"):
            check(f"--prompt-note: lane {n}'s ready file names its port", False, ready)
            continue
        lane = Client(ready["port"], ready["token"])
        status, reply = lane.op("eval", question, op_rid=f"note-lane-{n}")
        check(f"--prompt-note: lane {n}'s system prompt carries it",
              status == 200 and (reply.get("result") or {}).get("values") == [":in"], reply)


def _notice_texts(reply):
    """The command's own lines from a command.run reply, as text."""
    return [n.get("text") or "" for n in ((reply or {}).get("result") or {}).get("notices") or []]


def _lane_client(home, n):
    """A client on lane N's own port, from the ready file the coordinator owns."""
    ready = lane_ready(home, n) or {}
    if not ready.get("port"):
        return None
    return Client(ready["port"], ready["token"])


def _lane_prompt_has(lane, needle):
    """Whether LANE's own system prompt carries NEEDLE.  A lane's prompt is the
    only place its own note can be read, so this asks the lane's port."""
    question = {"code": f'(if (search "{needle}" (evo.kernel:build-system-prompt nil)) :in :absent)'}
    status, reply = lane.op("eval", question)
    return status == 200 and (reply.get("result") or {}).get("values") == [":in"]


def lane_count_checks(swarm, home, collector):
    """`/lanes N` at runtime (POST command.run): the pool the swarm runs is
    read from `/lanes` and changed with `/lanes N`, over the same op a client
    already uses for any other slash command.

    Covers: a count that is not a lane count refused `:invalid`; growing the
    pool while it runs — the new lane's topic, ready file and process, all of it
    reaching a subscription a client was already holding, and the re-snapshot
    that client takes on the reset; that a count the pool already has, or one
    below it, is a no-op that stops nothing; that the coordinator's own run is
    the guard, while a lane being busy is not; that growing restarts none of the
    lanes already there; and that every lane's own prompt note — the lane that
    joined and the lanes already there — carries the new count.

    Returns the lane count the pool ended at (GROWN when the growth worked,
    LANES otherwise), so the caller knows what to expect of the shutdown and of
    the resume."""
    client = swarm.client
    lanes = LANES

    # --- a count that is not a lane count is refused on the wire ------------
    status, reply = client.op("command.run", {"name": "lanes", "args": "nope"},
                              op_rid="lanes-invalid")
    check("/lanes with something that is not a number is refused",
          status == 200 and reply.get("ok") is False
          and (reply.get("error") or {}).get("code") == "invalid_args", reply)
    status, reply = client.op("command.run", {"name": "lanes", "args": "99"},
                              op_rid="lanes-out-of-range")
    check("/lanes with a count out of range is refused too",
          status == 200 and reply.get("ok") is False
          and (reply.get("error") or {}).get("code") == "invalid_args", reply)
    check("...and neither of them changed the pool",
          sorted(swarm_rows(client.topic_state("swarm"))) == list(range(1, lanes + 1)),
          sorted(swarm_rows(client.topic_state("swarm"))))

    # --- a count the pool already has is a no-op ---------------------------
    before = {n: _lane_row(client, n).get("pid") for n in range(1, lanes + 1)}
    was = len(lane_dirs(home))
    status, reply = client.op("command.run", {"name": "lanes", "args": str(lanes)},
                              op_rid="lanes-equal")
    check("/lanes N is accepted for N the pool already has",
          status == 200 and reply.get("ok"), reply)
    check("...and the command answers, saying nothing changed",
          _notice_texts(reply), reply)
    # Nothing about a no-op is async, but a wrong implementation's effect would
    # be: give it a moment to show before reading the pool back.
    time.sleep(1.0)
    after = {n: _lane_row(client, n).get("pid") for n in range(1, lanes + 1)}
    check("...and it is a no-op: the same lanes, the same processes",
          after == before and len(lane_dirs(home)) == was, (before, after))

    # --- a smaller count is a no-op too: /lanes never shrinks a pool --------
    status, reply = client.op("command.run", {"name": "lanes", "args": str(lanes - 1)},
                              op_rid="lanes-smaller")
    check("/lanes N below the count the pool has is accepted",
          status == 200 and reply.get("ok"), reply)
    check("...and answers rather than refusing: growth-only, so it says so",
          _notice_texts(reply), reply)
    time.sleep(1.0)
    rows = swarm_rows(client.topic_state("swarm"))
    check("...and it stops nothing: every lane is still there, same process",
          sorted(rows) == list(range(1, lanes + 1))
          and {n: row.get("pid") for n, row in rows.items()} == before, rows)
    check("...no lane process was stopped", all(alive(p) for p in lane_pids(home)),
          lane_pids(home))
    check("...and no lane directory was removed", len(lane_dirs(home)) == lanes,
          lane_dirs(home))

    # --- growing, with a lane busy: the lane that is working is not touched --
    # The guard is the *coordinator's* own run, never a lane's: what a resize
    # changes is what the coordinator waits on, and lanes are busy exactly when
    # more of them would help.  So the state to grow in is a lane working while
    # the coordinator has handed the task over and gone quiet — which is what
    # its run ending and `waiting` on its lanes means.
    long_task = "DELAY4 SLOW e2e lane work"
    status, reply = client.op("input.send",
                              {"text": f'CALL delegate {{"lane":1,"task":"{long_task}"}}'},
                              op_rid="lanes-delegate")
    check("a task is delegated to lane 1", reply.get("ok"), reply)
    quiet = wait_for(lambda: (
        _lane_row(client, 1).get("state") == "working"
        and client.topic_state("session").get("status") in ("idle", "waiting")) or None, 60)
    check("...lane 1 is working while the coordinator is not",
          quiet is not None,
          (client.topic_state("session").get("status"), _lane_row(client, 1).get("state")))
    working_pid = _lane_row(client, 1).get("pid")
    working_session = (lane_state(client, 1).get("session") or {}).get("id")
    status, reply = client.op("command.run", {"name": "lanes", "args": str(GROWN)},
                              op_rid="lanes-grow")
    check(f"/lanes {GROWN} grows the pool at runtime while a lane works",
          status == 200 and reply.get("ok"), reply)
    check("...and a notice names the count it is growing to",
          any(str(GROWN) in text for text in _notice_texts(reply)),
          _notice_texts(reply))
    grown = wait_for(lambda: _roster_ready(client, GROWN), 180)
    check(f"...to {GROWN} lanes, each a live process with a ready file",
          grown is not None, client.topic_state("swarm"))
    state = client.topic_state("swarm")
    check("...and the swarm topic's workers and its rows say the same count",
          state.get("workers") == GROWN
          and sorted(swarm_rows(state)) == list(range(1, GROWN + 1)),
          (state.get("workers"), sorted(swarm_rows(state))))
    check("...and the lane that was working kept its process",
          _lane_row(client, 1).get("pid") == working_pid,
          (_lane_row(client, 1).get("pid"), working_pid))
    check("...and its session",
          (lane_state(client, 1).get("session") or {}).get("id") == working_session,
          (lane_state(client, 1).get("session"), working_session))

    # --- the new lane, as a topic and a process ----------------------------
    topics = wait_for(lambda: _snapshot_topics(client, "lane:*")
                      if f"lane:{GROWN}" in _snapshot_topics(client, "lane:*") else None, 60)
    check(f"a snapshot taken after the growth carries the new lane {GROWN}",
          topics is not None, sorted(_snapshot_topics(client, "lane:*")))
    new = wait_for(lambda: (lane_state(client, GROWN)
                            if (lane_state(client, GROWN).get("session") or {}).get("id")
                            else None), 60)
    check(f"lane {GROWN}'s own topic is whole: a state and a session of its own",
          new is not None, lane_state(client, GROWN))
    ready = lane_ready(home, GROWN) or {}
    check(f"lane {GROWN} published a ready file and its pid is alive",
          bool(ready.get("pid")) and alive(ready["pid"]), ready)
    check("...and it is on disk beside the lanes it joined",
          len(lane_dirs(home)) == GROWN, lane_dirs(home))

    # A client already streaming lane:* — the subscription run_checks opened
    # before the growth — is told the new lane came up, the protocol's way
    # (topic.reset lane:N).  The reset carries no state: what makes the held
    # topic whole is the client's own re-snapshot of it (docs/serve.md §5.3),
    # so that is what is exercised here — replace what the subscription was
    # holding for the lane with the snapshot, then read the held topic back.
    announced = wait_for(lambda: any(t == f"lane:{GROWN}" for t, _ in collector.view.resets),
                         60)
    check(f"a client already holding lane:* is told lane {GROWN} came up",
          announced is not None, collector.view.resets)
    snap = _snapshot_topics(client, f"lane:{GROWN}", items=200).get(f"lane:{GROWN}") or {}
    held = collector.view.resnapshot(f"lane:{GROWN}", snap.get("state"), snap.get("items"))
    check(f"...and the re-snapshot that client takes on the reset holds lane {GROWN} whole",
          bool(held.state.get("status"))
          and bool((held.state.get("session") or {}).get("id")),
          (held.state, sorted(snap)))

    # --- every lane's own prompt note carries the new count -----------------
    # A lane's note says how many lanes there are ("You are lane N of M in a
    # swarm.").  The lane that just joined has it from its baseline; the lanes
    # already there are re-noted asynchronously (swarm/init.lisp's
    # WORKER-NOTE-FORM, whose count only ever moves up), so this waits.
    for n in range(1, GROWN + 1):
        lane = _lane_client(home, n)
        needle = f"You are lane {n} of {GROWN} in a swarm."
        told = wait_for(lambda lane=lane, needle=needle:
                        True if lane and _lane_prompt_has(lane, needle) else None, 120)
        check(f"lane {n}'s prompt note carries the new count ({GROWN} lanes)",
              told is not None, needle)

    # --- the coordinator's own run guards the resize ------------------------
    status, reply = client.op("input.send", {"text": "SLOW coordinator work"},
                              op_rid="lanes-coordinator")
    check("the coordinator takes a run of its own", reply.get("ok"), reply)
    check("...and is running",
          wait_for(lambda: client.topic_state("session").get("status") == "running",
                   30) is not None, client.topic_state("session").get("status"))
    status, reply = client.op("command.run", {"name": "lanes", "args": str(GROWN + 1)},
                              op_rid="lanes-busy")
    # The guard is a conflict (`command-refused :conflict`), which the wire
    # calls `busy`: a resize while the coordinator's own run is in flight.
    check("/lanes is refused while the coordinator is busy",
          status == 200 and reply.get("ok") is False
          and (reply.get("error") or {}).get("code") == "busy", reply)
    check("...and the pool is left exactly as it was",
          sorted(swarm_rows(client.topic_state("swarm"))) == list(range(1, GROWN + 1)),
          sorted(swarm_rows(client.topic_state("swarm"))))

    # --- settle: stop the lane's task and the coordinator's -----------------
    status, reply = client.op("run.interrupt", {"scope": "swarm"}, op_rid="lanes-stop")
    check("run.interrupt stops the working lane and the coordinator",
          status == 200 and reply.get("ok"), reply)
    check(f"...and every one of the {GROWN} lanes settles",
          wait_for(lambda: all(_lane_row(client, n).get("state") in ("idle", "down")
                               for n in range(1, GROWN + 1)), 90) is not None,
          [_lane_row(client, n) for n in range(1, GROWN + 1)])
    return GROWN if grown is not None else lanes


def _snapshot_topics(client, topics, items=1):
    """The topics a snapshot of TOPICS carries, by name."""
    return (client.snapshot(topics=topics, items=items) or {}).get("topics") or {}


def _roster_ready(client, count):
    """The swarm topic's rows once the pool has COUNT lanes, every one of them
    a process that has published its ready file (a pid), else NIL."""
    rows = swarm_rows(client.topic_state("swarm"))
    if len(rows) != count or not all(r.get("pid") for r in rows.values()):
        return None
    return rows


def run_checks(swarm, stub, home, work, proj, held):
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

    # --- the coordinator's catalog -------------------------------------------
    # GET /catalog on an evo-swarm server: the swarm's own half (lanes.models)
    # and, beside the default model, the effort a session started here would
    # run on.
    status, cat = client.get("/catalog")
    check("the coordinator's catalog carries the swarm's lanes half",
          status == 200 and isinstance((cat.get("lanes") or {}).get("models"), list),
          sorted(cat))
    check("catalog: the coordinator's default thinking is a rung of its own ladder",
          cat.get("default_thinking") in (cat.get("thinking_levels") or []),
          (cat.get("default_thinking"), cat.get("thinking_levels")))
    check("catalog: ...and it is the level the session's own state reports",
          cat.get("default_thinking") == client.topic_state("session").get("thinking"),
          (cat.get("default_thinking"), client.topic_state("session").get("thinking")))

    # --- a lane the client was already holding ------------------------------
    check_held_lanes_are_announced(swarm, held)

    # --- the client's own note, here and in every lane ----------------------
    prompt_note_check(swarm, home)

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
    # Only lane 2 is working (lane 1 came back idle from its restart): an idle
    # lane's own run.interrupt answers [] and stops nothing, so it is not named.
    check("...naming the session and the lane that was working",
          "session" in interrupted and "lane:2" in interrupted, interrupted)
    check("...and not the idle lane", "lane:1" not in interrupted, interrupted)
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
          note is not None and (note.get("lanes") or []) == [2], note)
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

    # --- the runtime lane count: /lanes N -----------------------------------
    # Last, so the checks above know the swarm they were written for (LANES):
    # after this the pool is GROWN lanes and stays that way into the resume.
    achieved = lane_count_checks(swarm, home, collector)

    collector.stop.set()
    return achieved


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
    # EVO_SESSIONS_DIR would put the swarm's journals in someone else's home,
    # and a foreign EVO_BINARY would run a lane on somebody else's build
    # (tests/clean_env.py — the same rule the Lisp runners apply).
    env = clean(HOME=home, EVO_HOME=home, EVO_BINARY=EVO, TERM="xterm-256color")

    # The client's own note (`--prompt-note`, docs/serve.md): a file this test
    # writes, which the launch reads for the coordinator and passes to every
    # lane.  Written before the swarm starts, because the flag is read at boot.
    note = os.path.join(work, "gui-renderer.md")
    with open(note, "w") as f:
        f.write(f"The client renders your output itself: {NOTE_MARKER}.\n")

    swarm = Swarm(home, proj, env, os.path.join(work, "ready.json"), notes=[note])
    swarms = [swarm]
    achieved = LANES
    coordinator_session = None
    try:
        if not swarm.wait_ready():
            print("swarm-serve-e2e: `evo-swarm serve` did not come up "
                  f"(exit {swarm.proc.poll()})")
            failed += 1
        else:
            # Snapshot lane:* *now*, before any lane's process exists: this is
            # the cursor evo-gui holds when it opens a resumed swarm's tab.
            held = hold_lanes_from_the_start(swarm)
            achieved = run_checks(swarm, stub, home, work, proj, held) or LANES
            coordinator_session = (swarm.client.topic_state("session").get("session") or {}).get("id")
            check("the coordinator's session identity is available for resume checks",
                  bool(coordinator_session), swarm.client.topic_state("session"))

        # --- shutdown ---------------------------------------------------------
        dirs = lane_dirs(home)
        check(f"{achieved} lane directories", len(dirs) == achieved, dirs)
        pids = lane_pids(home)
        check("every lane published a ready file of its own, all alive",
              len(pids) == achieved and all(alive(p) for p in pids), pids)
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

        # --- resume: the session's roster is the roster, and --workers raises it
        # Three launches of the same recorded session, each read on what comes
        # back: no --workers flag at all (the recorded roster, untouched), a
        # lower one (which must not shrink it), and a higher one (which grows
        # it -- the same ensure-lane-count the runtime `/lanes N` uses).
        phases = [("with no --workers flag", None, achieved),
                  ("with a lower --workers", 1, achieved),
                  ("with a higher --workers", achieved + 2, achieved + 2)]
        for index, (how, workers, expect) in enumerate(phases):
            resumed = Swarm(home, proj, env,
                            os.path.join(work, f"ready-resume-{index}.json"),
                            workers=workers, resume=True)
            swarms.append(resumed)
            if not resumed.wait_ready():
                failed += 1
                print(f"swarm-serve-e2e: the resumed swarm {how} did not come up "
                      f"(exit {resumed.proc.poll()})")
                break
            resumed_checks(resumed, home, expect, coordinator_session, how)
            status, reply = resumed.client.op("server.shutdown", {})
            check(f"the resumed swarm {how} shuts down cleanly",
                  status == 200 and reply.get("ok"), reply)
            check("...and exits 0", resumed.wait(timeout=30) == 0)
    except BaseException as e:
        failed += 1
        print(f"FAIL aborted: {e!r}")
    finally:
        for running in swarms:
            running.kill()
        stub.proc.kill()
        for running in swarms:
            running.log.close()
    if failed:
        for running in swarms:
            print(f"--- {os.path.basename(running.ready_file)} ---")
            print(running.log_tail())
    else:
        shutil.rmtree(work, ignore_errors=True)
    print(f"\nswarm-serve-e2e: {passed} passed, {failed} failed")
    return 1 if failed else 0


def resumed_checks(swarm, home, expected, coordinator_session, how):
    """A resumed swarm restores the roster its session recorded — the one
    `/lanes N` grew the pool to — whatever `--workers` it was launched HOW:
    none at all, a lower one, or a higher one.  Every lane is its own process
    and its own topic again, and the coordinator is on the session it was on."""
    client = swarm.client
    rows = wait_for(lambda: _roster_ready(client, expected), 180)
    check(f"resumed {how}: the roster is the session's {expected} lanes",
          rows is not None, sorted(swarm_rows(client.topic_state("swarm"))))
    state = client.topic_state("swarm")
    check(f"resumed {how}: ...and the swarm's own count is {expected}",
          state.get("workers") == expected, (state.get("workers"), expected))
    check(f"resumed {how}: ...every lane a live process of its own again",
          all(r.get("pid") and alive(r["pid"]) for r in (rows or {}).values()), rows)
    check(f"resumed {how}: ...with a ready file of its own each",
          all(lane_ready(home, n) for n in range(1, expected + 1)),
          [n for n in range(1, expected + 1) if not lane_ready(home, n)])
    # A lane's own topic reports its state once the lane's process has come up
    # and its first snapshot is taken — a moment after the pid the roster above
    # was read from — so wait for all of them rather than reading once.
    def _states_ready():
        missing = [n for n in range(1, expected + 1)
                   if not lane_state(client, n).get("status")]
        return True if not missing else None

    states = wait_for(_states_ready, 90)
    check(f"resumed {how}: ...and a topic with a state of its own each",
          states is not None,
          [n for n in range(1, expected + 1)
           if not lane_state(client, n).get("status")])
    check(f"resumed {how}: ...on disk beside each other",
          len(lane_dirs(home)) == expected, lane_dirs(home))
    check(f"resumed {how}: ...the coordinator on the session it was on",
          bool(coordinator_session)
          and (client.topic_state("session").get("session") or {}).get("id") == coordinator_session,
          (client.topic_state("session"), coordinator_session))


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
