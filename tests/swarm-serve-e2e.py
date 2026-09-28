#!/usr/bin/env python3
"""swarm-serve-e2e.py — `evo-swarm serve` end to end, over HTTP only.

The headless swarm, driven exactly as the GUI will drive it: one bearer token,
`evo serve`'s unchanged protocol for the coordinator, plus the swarm feature —
GET /lanes, a lane's transcript and live events, and lane-state events on the
coordinator's own stream.  No terminal: the swarm runs as a `serve` process.
The "model" is tests/stub-messages.py, which scripts both the coordinator and
its lanes from what they are sent (`CALL <tool> {json}` becomes that tool call),
so this needs nothing but python3, git-free, and a built evo + evo-swarm.

Covers:
  * /health names the server a swarm (name, version, features);
  * the coordinator answers the unchanged protocol (state, journal, transcript,
    sessions, registry, commands, events), and its transcript records the
    delegation it made;
  * auth is required on every route, the swarm's included; a lane is read-only
    over HTTP, and an unknown lane is 404;
  * a prompt through /prompt is delegated to a lane;
  * GET /lanes lists every lane numbered 1..N with its state, task, goal,
    worktree, branch and restarts, and nothing private;
  * the lane-state events on the coordinator's /events track that lane working,
    then idle;
  * a lane's transcript and live events can be read, and resumed;
  * lane tokens and URLs are not exposed over HTTP;
  * /shutdown stops every lane, and `--resume` restores the swarm.

Field names the objective leaves open (an identity field's exact key, a lane
object's exact keys) are read through PICK, which takes the first present of a
small set of synonyms — one constant to change, not the test's shape.

Usage: tests/swarm-serve-e2e.py [build-dir]     (exit 0 on success; Unix only)
"""

import glob
import http.client
import json
import os
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import threading
import time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BUILD = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else os.path.join(ROOT, "build")
SWARM = os.path.join(BUILD, "evo-swarm")
EVO = os.path.join(BUILD, "evo")
SECRET = "swarm-serve-e2e-secret-4c1f"
LANES = 2
# DELAY2 makes the lane wait before it "thinks", then SLOW streams 60 deltas a
# tenth of a second apart: an ~8 s working window to observe and to watch live.
TASK = "DELAY2 SLOW e2e lane work"

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
    """The first of KEYS present (and not None) in OBJ, else None.  The one
    place a field-name synonym lives."""
    if not isinstance(obj, dict):
        return None
    for key in keys:
        if key in obj and obj[key] is not None:
            return obj[key]
    return None


def has_any(obj, *keys):
    """Whether OBJ carries any of KEYS — presence, not value."""
    return isinstance(obj, dict) and any(k in obj for k in keys)


def lane_n(lane):
    return pick(lane, "n", "number", "lane")


def lane_state(lane):
    return (pick(lane, "state", "status") or "")


def lane_task(lane):
    return (pick(lane, "task") or "")


def lane_list(body):
    """The lane objects from a GET /lanes body, whatever its exact shape:
    a bare array, {lanes: [...]}, or {lanes: {n: {...}}}."""
    if isinstance(body, list):
        return body
    if not isinstance(body, dict):
        return None
    lanes = body.get("lanes")
    if lanes is None and isinstance(body.get("swarm"), dict):
        lanes = body["swarm"].get("lanes")
    if lanes is None:
        return None
    if isinstance(lanes, dict):
        return list(lanes.values())
    return lanes


def find_lane(lanes, n):
    return next((l for l in lanes if str(lane_n(l)) == str(n)), None)


def messages_of(body):
    if isinstance(body, list):
        return body
    if isinstance(body, dict):
        return pick(body, "messages", "transcript")
    return None


class Client:
    """The swarm, as a client reaches it: one token, one port, HTTP."""

    def __init__(self, port, token):
        self.port = port
        self.token = token

    def request(self, method, path, body=None, token=None, headers=None, timeout=30):
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
        raw = resp.read().decode(errors="replace")
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

    def stream(self, path, headers=None, timeout=60):
        """Yield (id, event, data) from an SSE GET until it ends or times out."""
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=timeout)
        try:
            h = {"Authorization": f"Bearer {self.token}"}
            h.update(headers or {})
            conn.request("GET", path, headers=h)
            resp = conn.getresponse()
            if resp.status != 200:
                yield (None, "http-error", resp.status)
                return
            event = {"id": None, "event": None, "data": None}
            while True:
                try:
                    line = resp.fp.readline()
                except (socket.timeout, TimeoutError, OSError):
                    break
                if not line:
                    break
                line = line.decode(errors="replace").rstrip("\n")
                if line == "":
                    if event["event"]:
                        try:
                            yield (event["id"], event["event"], json.loads(event["data"]))
                        except ValueError:
                            yield (event["id"], event["event"], event["data"])
                    event = {"id": None, "event": None, "data": None}
                elif line.startswith(":"):
                    continue
                else:
                    key, _, value = line.partition(": ")
                    event[key] = int(value) if key == "id" else value
        finally:
            conn.close()


class Collector(threading.Thread):
    """Read an SSE stream in the background until a stop event (or the end)."""

    def __init__(self, client, path, stop_types=(), headers=None, timeout=60):
        super().__init__(daemon=True)
        self.client = client
        self.path = path
        self.stop_types = set(stop_types)
        self.headers = headers
        self.timeout = timeout
        self.events = []
        self.error = None

    def run(self):
        try:
            for eid, etype, data in self.client.stream(self.path, headers=self.headers,
                                                       timeout=self.timeout):
                self.events.append((eid, etype, data))
                if etype in self.stop_types:
                    break
        except Exception as e:                      # a stream that dies is the test's to see
            self.error = e

    def types(self):
        return [e[1] for e in self.events]

    def ids(self):
        return [e[0] for e in self.events if e[0] is not None]


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

    def find(self, role, needle, after=0.0):
        """The first request by ROLE whose last user text contains NEEDLE and
        that arrived at or after AFTER — a wall-clock time taken before the
        prompt, so a stale request from an earlier step is not mistaken for
        this one's."""
        for r in self.requests():
            if r["role"] == role and needle in r["last_user"] and r["time"] >= after:
                return r
        return None


class Swarm:
    """One `evo-swarm serve` process, its token, and what it answers."""

    def __init__(self, home, proj, env, token_file, workers=LANES, resume=False):
        self.port = free_port()
        self.token_file = token_file
        args = [SWARM, "serve", "--port", str(self.port),
                "--token-file", token_file, "--workers", str(workers), "--evo", EVO]
        if resume:
            args.append("--resume")
        self.log = open(token_file + ".log", "w")
        self.proc = subprocess.Popen(args, cwd=proj, env=env, stdin=subprocess.DEVNULL,
                                     stdout=self.log, stderr=subprocess.STDOUT,
                                     start_new_session=True)
        self.token = None
        self.client = None

    def wait_ready(self, timeout=90):
        """The token file, then /health, under one deadline: T once the swarm
        answers, F once it exits or the deadline passes."""
        deadline = time.time() + timeout
        while time.time() < deadline:
            if os.path.exists(self.token_file) and os.path.getsize(self.token_file) > 0:
                self.token = open(self.token_file).read().strip()
                self.client = self.client or Client(self.port, self.token)
                try:
                    if self.client.get("/health", timeout=5)[0] == 200:
                        return True
                except OSError:
                    pass
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

    def log_tail(self, lines=40):
        try:
            with open(self.log.name, errors="replace") as f:
                return "".join(f.readlines()[-lines:])
        except OSError:
            return ""


def lane_dirs(home):
    return sorted(glob.glob(os.path.join(home, "swarm", "*", "lane-*")),
                  key=lambda d: int(d.rsplit("-", 1)[1]))


def lane_health(directory):
    """A lane's /health status, read from its own url+token on disk, or None
    when it does not answer — how the test tells a stopped lane from a live one
    without the swarm's HTTP API ever handing out a lane's address."""
    try:
        port = int(open(os.path.join(directory, "url")).read().strip().rsplit(":", 1)[1])
        token = open(os.path.join(directory, "token")).read().strip()
    except (OSError, ValueError):
        return None
    conn = http.client.HTTPConnection("127.0.0.1", port, timeout=3)
    try:
        conn.request("GET", "/health", headers={"Authorization": f"Bearer {token}"})
        resp = conn.getresponse()
        resp.read()
        return resp.status
    except OSError:
        return None
    finally:
        conn.close()


def lane_secrets(home):
    """Every lane's token, for the no-secrets check."""
    out = []
    for d in lane_dirs(home):
        try:
            out.append(open(os.path.join(d, "token")).read().strip())
        except OSError:
            pass
    return out


def wait_for(predicate, timeout=30, interval=0.1):
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


def get_lanes(client):
    status, body = client.get("/lanes")
    if status != 200:
        return None
    return lane_list(body)


def wait_lane_state(client, n, want, timeout=30):
    def ready():
        lanes = get_lanes(client)
        lane = find_lane(lanes, n) if lanes else None
        return lane if lane is not None and lane_state(lane) == want else None
    return wait_for(ready, timeout)


def events_until(client, since, upto, timeout=20, cap=8000):
    """Replay the coordinator's log from SINCE to UPTO (a cursor), inclusive."""
    out = []
    for eid, etype, data in client.stream(f"/events?since={since}", timeout=timeout):
        out.append((eid, etype, data))
        if (upto is not None and eid is not None and eid >= upto) or len(out) >= cap:
            break
    return out


def lane_state_events(events):
    out = []
    for eid, etype, data in events:
        if etype == "lane-state":
            out.append((eid, pick(data, "lane", "n", "number"),
                        (pick(data, "state", "status") or ""), data))
    return out


def lanes_all_idle(client):
    """The lanes once every one of them is idle, else None — what a caller
    waits on when it needs the swarm settled."""
    lanes = get_lanes(client)
    return lanes if lanes and all(lane_state(l) == "idle" for l in lanes) else None


def lane_numbers(lanes):
    return sorted(int(lane_n(l)) for l in (lanes or []) if lane_n(l) is not None)


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

    env = dict(os.environ, EVO_HOME=home, EVO_BINARY=EVO, TERM="xterm-256color")
    for var in ("EVO_SERVE_TOKEN", "EVO_SUPERVISED_CHILD", "EVO_NO_SUPERVISOR",
                "EVO_SESSIONS_DIR", "EVO_SERVE_WATCH_PID", "ANTHROPIC_API_KEY"):
        env.pop(var, None)

    first = Swarm(home, proj, env, os.path.join(work, "token-1"))
    second = None
    try:
        if not first.wait_ready():
            print(f"swarm-serve-e2e: `evo-swarm serve` did not come up (exit {first.proc.poll()}) — "
                  "the feature is not implemented yet; this test awaits it.")
            failed += 1
        else:
            run_first(first, stub, home, work)
            # --- shutdown stops every lane ---------------------------------------
            session_before = first.client.get("/journal")[1]["path"]
            dirs = lane_dirs(home)
            check(f"{LANES} lane directories", len(dirs) == LANES, dirs)
            check("every lane answers before shutdown",
                  all(lane_health(d) == 200 for d in dirs), [lane_health(d) for d in dirs])
            status, reply = first.client.post("/shutdown")
            check("shutdown accepted", status == 200 and reply.get("ok"), reply)
            code = first.wait(timeout=30)
            check("the swarm exits 0 after shutdown (supervisor included)", code == 0, code)
            check("every lane is gone after shutdown",
                  wait_for(lambda: all(lane_health(d) is None for d in dirs), 30),
                  [(d, lane_health(d)) for d in dirs])
            check("the token file is removed on clean exit", not os.path.exists(first.token_file))

            # --- --resume restores the swarm -------------------------------------
            second = Swarm(home, proj, env, os.path.join(work, "token-2"), resume=True)
            check("`evo-swarm serve --resume` comes up", second.wait_ready())
            if second.client:
                status, health = second.client.get("/health")
                check("the resumed swarm names itself a swarm again",
                      status == 200 and pick(health, "name", "server") == "evo-swarm", health)
                check("resume reuses the same swarm",
                      len(glob.glob(os.path.join(home, "swarm", "*"))) == 1)
                resumed = wait_for(lambda: lanes_all_idle(second.client), 120)
                check("every lane comes back up", resumed is not None, get_lanes(second.client))
                check("the lane numbering is 1..N",
                      lane_numbers(resumed) == list(range(1, LANES + 1)), lane_numbers(resumed))
                body = wait_for(lambda: ("e2e lane work"
                                         in json.dumps(second.client.get("/lanes/1/transcript")[1])
                                         or None), 30)
                check("a lane's session was resumed", bool(body),
                      json.dumps(second.client.get("/lanes/1/transcript")[1])[-400:])
                path = second.client.get("/journal")[1]["path"]
                check("the coordinator resumed its own session", path == session_before,
                      (path, session_before))
                dirs = lane_dirs(home)
                check("the resumed swarm stops its lanes too",
                      second.client.post("/shutdown")[0] == 200
                      and second.wait(timeout=30) == 0
                      and wait_for(lambda: all(lane_health(d) is None for d in dirs), 30))
    except BaseException as e:
        failed += 1
        print(f"FAIL aborted: {e!r}")
    finally:
        for s in (first, second):
            if s is not None:
                s.kill()
        stub.proc.kill()
        for s in (first, second):
            if s is not None:
                s.log.close()
    if failed:
        print(first.log_tail())
    else:
        shutil.rmtree(work, ignore_errors=True)
    print(f"\nswarm-serve-e2e: {passed} passed, {failed} failed")
    return 1 if failed else 0


def run_first(swarm, stub, home, work):
    c = swarm.client

    # --- auth, on the unchanged protocol and on the swarm routes -------------
    check("no token -> 401", c.get("/state", token="")[0] == 401)
    check("wrong token -> 401", c.get("/state", token="not-the-token")[0] == 401)
    check("unknown endpoint -> 404", c.get("/nope")[0] == 404)
    check("no token -> 401 on /lanes", c.get("/lanes", token="")[0] == 401)
    check("wrong token -> 401 on /lanes", c.get("/lanes", token="nope")[0] == 401)
    check("no token -> 401 on a lane's transcript",
          c.get("/lanes/1/transcript", token="")[0] == 401)
    check("no token -> 401 on a lane's events",
          c.get("/lanes/1/events", token="")[0] == 401)

    # --- /health names the server a swarm ------------------------------------
    status, health = c.get("/health")
    check("GET /health is 200", status == 200, health)
    check("health names it evo-swarm", pick(health, "name", "server") == "evo-swarm", health)
    check("health carries a version", isinstance(pick(health, "version"), str)
          and pick(health, "version") != "", health)
    features = pick(health, "features") or []
    check("health lists the swarm feature", "swarm" in features, health)

    # --- the coordinator, through the unchanged protocol ---------------------
    status, state = c.get("/state")
    check("GET /state works and the coordinator is idle",
          status == 200 and state.get("status") == "idle", state)
    check("state resolves the coordinator's model", state.get("model") == "stub-a", state)
    status, journal = c.get("/journal")
    check("GET /journal works", status == 200 and journal.get("entries") is not None, journal)
    status, sessions = c.get("/sessions")
    check("GET /sessions lists the coordinator's session",
          status == 200 and sessions.get("sessions"), sessions)
    status, registry = c.get("/registry")
    check("GET /registry lists the model", status == 200
          and "stub-a" in json.dumps(registry), registry)
    status, reply = c.command("/model")
    check("POST /command runs a slash command", status == 200
          and reply.get("choices", {}).get("items"), reply)

    # --- /lanes: every lane, its panel fields, nothing private ---------------
    lanes = wait_for(lambda: lanes_all_idle(c), 120)
    check(f"GET /lanes lists {LANES} lanes, every one idle", lanes is not None, get_lanes(c))
    check("the lane numbering is 1..N",
          lane_numbers(lanes) == list(range(1, LANES + 1)), lane_numbers(lanes))
    check("every lane carries goal, worktree, branch and restart fields",
          bool(lanes) and all(all(has_any(l, *keys) for keys in (
              ("goal", "goal_status"), ("worktree",), ("branch",), ("restarts",)))
                              for l in lanes), lanes)
    text = json.dumps(get_lanes(c))
    secrets = lane_secrets(home)
    check(f"exactly {LANES} lane tokens on disk, one per lane",
          len(secrets) == LANES and len(set(secrets)) == LANES, secrets)
    check("no lane token reaches the client", all(t not in text for t in secrets), secrets)
    check("no lane URL reaches the client", "http" not in text, text[:200])
    check("a lane is read-only over HTTP (POST /lanes -> 405)", c.post("/lanes")[0] == 405)
    check("a lane's transcript is read-only (POST -> 405)",
          c.post("/lanes/1/transcript")[0] == 405)
    check("an unknown lane's transcript is 404", c.get("/lanes/99/transcript")[0] == 404)
    check("an unknown lane's events is 404", c.get("/lanes/99/events")[0] == 404)

    # --- a prompt is delegated to a lane -------------------------------------
    t0 = time.time()
    collector = Collector(c, "/lanes/1/events", stop_types=("settled",), timeout=60)
    collector.start()
    status, reply = c.post("/prompt", {"text": f'CALL delegate {{"lane":1,"task":"{TASK}"}}'})
    check("POST /prompt starts the coordinator's run",
          status == 200 and reply.get("task"), reply)
    delegated = wait_for(lambda: stub.find("lane 1", "e2e lane work", t0), 30)
    check("the coordinator delegated the task to lane 1 (worker prompt note)",
          delegated and delegated["system_has_report_note"], delegated)
    working = wait_lane_state(c, 1, "working", 30)
    check("GET /lanes shows lane 1 working on the task",
          working is not None and "e2e lane work" in lane_task(working), working)
    check("the coordinator's own transcript recorded the delegation",
          wait_for(lambda: ("e2e lane work" in json.dumps(c.get("/transcript")[1]) or None), 30)
          is not None, json.dumps(c.get("/transcript")[1])[-400:])

    # --- the lane's live events, and its transcript --------------------------
    collector.join(timeout=60)
    check("lane 1's live event stream relays its run",
          "settled" in collector.types() and collector.error is None,
          (collector.types()[:8], collector.error))
    check("...carrying its streamed text",
          "text-delta" in collector.types(), collector.types()[:12])
    live_ids = collector.ids()
    check("...with event ids", bool(live_ids), live_ids)

    idle = wait_lane_state(c, 1, "idle", 30)
    check("GET /lanes shows lane 1 idle again", idle is not None, get_lanes(c))

    status, transcript = c.get("/lanes/1/transcript")
    msgs = messages_of(transcript)
    check("GET /lanes/1/transcript returns the lane's messages",
          status == 200 and isinstance(msgs, list) and msgs, transcript)
    body = json.dumps(transcript)
    check("the lane's transcript has the task and its answer",
          "e2e lane work" in body and "slow" in body, body[-400:])

    # --- the lane's events are resumable -------------------------------------
    if len(live_ids) >= 2:
        pivot, last = live_ids[0], live_ids[-1]
        resumed = []
        for eid, etype, data in c.stream(f"/lanes/1/events?since={pivot}", timeout=15):
            resumed.append((eid, etype, data))
            if (eid is not None and eid >= last) or len(resumed) >= 8000:
                break
        resumed_ids = [e[0] for e in resumed if e[0] is not None]
        check("lane 1's events resume after a given id",
              bool(resumed_ids) and all(i > pivot for i in resumed_ids)
              and last in resumed_ids, (pivot, last, resumed_ids[:8]))
    else:
        check("lane 1's events resume after a given id", False, live_ids)

    # --- lane-state events on the coordinator's stream -----------------------
    upto = c.get("/state")[1].get("cursor")
    events = events_until(c, 0, upto)
    tracked = [e for e in lane_state_events(events) if str(e[1]) == "1"]
    states = [e[2] for e in tracked]
    check("the coordinator's stream carries lane-state events for lane 1",
          "working" in states and "idle" in states, tracked)
    check("...working before idle",
          ("working" in states and "idle" in states
           and states.index("working") < states.index("idle")), states)
    check("...the working event names the task",
          any("e2e lane work" in json.dumps(e[3]) for e in tracked), tracked)


if __name__ == "__main__":
    sys.exit(main())
