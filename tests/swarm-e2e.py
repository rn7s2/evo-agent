#!/usr/bin/env python3
"""swarm-e2e.py — evo-swarm end to end, with no backend.

The coordinator is a TUI, so this drives it the way a person would: through a
pseudo-terminal, typing prompts.  The "model" is tests/stub-messages.py, which
scripts both the coordinator and the lanes from what they are sent — a prompt
starting `CALL <tool> {json}` becomes that tool call — and logs every request
with who made it (the coordinator or lane N, read from the swarm prompt
notes).  Lanes are checked through their own HTTP API (their url and token
files are in the swarm directory), exactly as the coordinator reaches them.

Proves: N lanes start and pass auth; swarm.lisp sets the coordinator's model;
every lane runs the global and the project swarm.lisp's in-lanes forms (one
at a time, *load-truename*, the lane variables, a package an earlier form
loaded, overriding the coordinator's defaults); each got its
baseline and no secret is in any journal; delegation runs on a lane; a report wakes the idle coordinator;
interrupt and re-steer of a busy lane; an eval adds a tool to one lane only; a
worktree lane works in its worktree; a killed lane restarts and the
coordinator is told; quitting stops every lane; `evo-swarm --resume` restores
coordinator and lanes.

Usage: tests/swarm-e2e.py [build-dir]     (exit 0 on success; Unix only)
"""

import fcntl
import glob
import http.client
import json
import os
import pty
import re
import shutil
import signal
import socket
import struct
import subprocess
import sys
import tempfile
import termios
import threading
import time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BUILD = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else os.path.join(ROOT, "build")
SWARM = os.path.join(BUILD, "evo-swarm")
SECRET = "e2e-literal-secret-key-9f3a"
LANES = 4

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


def wait_for(predicate, timeout=60, interval=0.25):
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


class Stub:
    def __init__(self):
        self.port = free_port()
        self.proc = subprocess.Popen([sys.executable, os.path.join(ROOT, "tests", "stub-messages.py"),
                                      str(self.port)], stdout=subprocess.PIPE, text=True)
        assert self.proc.stdout.readline().startswith("stub listening")

    def requests(self):
        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=10)
        conn.request("GET", "/_requests")
        data = json.loads(conn.getresponse().read())
        conn.close()
        return data

    def find(self, role, needle, after=0.0):
        """The first request by ROLE whose last user text contains NEEDLE."""
        for r in self.requests():
            if r["role"] == role and needle in r["last_user"] and r["time"] >= after:
                return r
        return None


def coordinator_quiet(stub, seconds=1.5, timeout=60):
    """Wait until the coordinator has sent the model nothing for SECONDS: its
    last turn is over.  Typed input that lands mid-turn joins lane notices at
    the next boundary, and the stub answers only the newest block — so each
    scripted prompt is typed into a quiet coordinator."""
    def quiet():
        times = [r["time"] for r in stub.requests() if r["role"] == "coordinator"]
        return not times or time.time() - max(times) > seconds
    return wait_for(quiet, timeout)


class Terminal:
    """evo-swarm under a pseudo-terminal: type into it, read what it paints."""

    def __init__(self, args, cwd, env):
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 160, 0, 0))
        self.proc = subprocess.Popen([SWARM] + args, stdin=slave, stdout=slave, stderr=slave,
                                     cwd=cwd, env=env, start_new_session=True)
        os.close(slave)
        self.master = master
        self.buffer = bytearray()
        threading.Thread(target=self._read, daemon=True).start()

    def _read(self):
        while True:
            try:
                data = os.read(self.master, 65536)
            except OSError:
                break
            if not data:
                break
            self.buffer.extend(data)

    def text(self):
        return re.sub(r"\x1b\[[0-9;?<>=]*[a-zA-Z~]", "", self.buffer.decode(errors="replace"))

    def type(self, line):
        os.write(self.master, line.encode())
        time.sleep(0.3)
        os.write(self.master, b"\r")

    def wait_exit(self, timeout=90):
        try:
            return self.proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            return None

    def kill(self):
        if self.proc.poll() is None:
            os.killpg(self.proc.pid, signal.SIGKILL)


class Lane:
    """A lane, reached through its own HTTP API."""

    def __init__(self, directory):
        self.dir = directory
        self.n = int(directory.rstrip("/").rsplit("-", 1)[1])

    def url(self):
        return open(os.path.join(self.dir, "url")).read().strip()

    def token(self):
        return open(os.path.join(self.dir, "token")).read().strip()

    def request(self, method, path, body=None, token=None, timeout=30):
        port = int(self.url().rsplit(":", 1)[1])
        conn = http.client.HTTPConnection("127.0.0.1", port, timeout=timeout)
        headers = {}
        tok = self.token() if token is None else token
        if tok:
            headers["Authorization"] = f"Bearer {tok}"
        data = json.dumps(body).encode() if body is not None else None
        conn.request(method, path, body=data, headers=headers)
        resp = conn.getresponse()
        raw = resp.read()
        conn.close()
        try:
            return resp.status, json.loads(raw)
        except ValueError:
            return resp.status, raw

    def get(self, path, **kw):
        return self.request("GET", path, **kw)

    def pid(self):
        status, health = self.get("/health", timeout=5)
        return health["pid"] if status == 200 else None

    def tools(self):
        status, reg = self.get("/registry")
        return [t["name"] for t in reg["tools"]] if status == 200 else []

    def eval(self, form):
        return self.request("POST", "/eval", {"form": form})

    def transcript_text(self):
        status, tr = self.get("/transcript")
        return json.dumps(tr["messages"]) if status == 200 else ""


def lanes_of(home):
    dirs = sorted(glob.glob(os.path.join(home, "swarm", "*", "lane-*")),
                  key=lambda d: int(d.rsplit("-", 1)[1]))
    return [Lane(d) for d in dirs]


def lane_ready(lane):
    """Up, authenticated, and initialized (the report tool is its baseline)."""
    return os.path.exists(os.path.join(lane.dir, "token")) and "report" in lane.tools()


def pid_alive(pid):
    try:
        os.kill(pid, 0)
        return True
    except OSError:
        return False


def main():
    if not os.access(SWARM, os.X_OK):
        print(f"swarm-e2e: no binary at {SWARM} (make build first)")
        return 1
    work = tempfile.mkdtemp(prefix="evo-swarm-e2e-")
    home = os.path.join(work, "home")
    proj = os.path.join(work, "proj")
    os.makedirs(home)
    os.makedirs(proj)
    # A git repository, for the worktree lane.
    for cmd in (["git", "init", "-q", "-b", "main"],
                ["git", "-c", "user.email=e2e@evo", "-c", "user.name=e2e",
                 "commit", "-q", "--allow-empty", "-m", "root"]):
        subprocess.run(cmd, cwd=proj, check=True)
    stub = Stub()
    # The coordinator's config registers its provider with a LITERAL key: the
    # swarm must hand it to lanes by environment variable name only.
    with open(os.path.join(home, "init.lisp"), "w") as f:
        f.write(f'(evo:register-provider :stub :base-url "http://127.0.0.1:{stub.port}" '
                f':api-key "{SECRET}")\n'
                '(evo:register-model "stub-a" :provider :stub :context-window 200000 '
                ':max-output 8000 :effort t)\n'
                '(evo:set-setting :model "stub-a")\n')
    # swarm.lisp, read by evo-swarm only, after post-init: the global one sets
    # the coordinator's model for the swarm and gives every lane a tool...
    with open(os.path.join(home, "swarm.lisp"), "w") as f:
        f.write('(evo:register-model "stub-swarm" :provider :stub :context-window 200000 '
                ':max-output 8000 :effort t)\n'
                '(evo:set-setting :model "stub-swarm")\n'
                '(evo.swarm:in-lanes ()\n'
                '  (evo:register-tool "e2e_global_lane_tool" :description "from ~/.evo/swarm.lisp"\n'
                "    :schema '(:object) :execute (lambda (a) (declare (ignore a)) \"ok\")))\n")
    # ...and the project's has lanes load a file beside it that defines a
    # package — loaded here too, so the coordinator can read the forms that
    # name it — then use that package, their lane number and the lane count.
    os.makedirs(os.path.join(proj, ".evo"))
    with open(os.path.join(proj, ".evo", "lane-pkg.lisp"), "w") as f:
        f.write('(defpackage :e2e-lane-pkg (:use :cl) (:export #:tool-name))\n'
                '(in-package :e2e-lane-pkg)\n'
                '(defun tool-name (n total) (format nil "e2e_lane_~d_of_~d" n total))\n'
                '(evo:register-tool "e2e_baseline_tool" :description "from in-lanes"\n'
                "  :schema '(:object) :execute (lambda (a) (declare (ignore a)) \"ok\"))\n")
    with open(os.path.join(proj, ".evo", "swarm.lisp"), "w") as f:
        f.write('(load (merge-pathnames "lane-pkg.lisp" *load-truename*))\n'
                '(evo.swarm:in-lanes (lane lanes)\n'
                '  (load (merge-pathnames "lane-pkg.lisp" *load-truename*))\n'
                '  (evo:register-tool (e2e-lane-pkg:tool-name lane lanes)\n'
                '    :description "names its lane"\n'
                "    :schema '(:object) :execute (lambda (a) (declare (ignore a)) \"ok\"))\n"
                "  (when (= lane 1) (evo:set-setting :thinking :low)))\n")
    env = dict(os.environ, EVO_HOME=home, TERM="xterm-256color", EVO_BINARY=os.path.join(BUILD, "evo"))
    for var in ("EVO_SERVE_TOKEN", "EVO_SUPERVISED_CHILD", "EVO_NO_SUPERVISOR",
                "EVO_SESSIONS_DIR", "EVO_SERVE_WATCH_PID", "ANTHROPIC_API_KEY"):
        env.pop(var, None)

    term = Terminal(["--workers", str(LANES)], proj, env)
    try:
        first_run(term, stub, home, proj)
        term = Terminal(["--resume"], proj, env)
        resumed_run(term, stub, home, proj)
        no_secrets(home)
    except BaseException as e:
        global failed
        failed += 1
        print(f"FAIL aborted: {e!r}")
    finally:
        term.kill()
        stub.proc.kill()
    if failed:
        print(term.text()[-6000:])
        for lane in lanes_of(home):
            log = os.path.join(lane.dir, "lane.log")
            if os.path.exists(log):
                print(f"--- lane {lane.n} log ---")
                print(open(log, errors="replace").read()[-1500:])
    else:
        shutil.rmtree(work, ignore_errors=True)
    print(f"\nswarm-e2e: {passed} passed, {failed} failed")
    return 1 if failed else 0


def first_run(term, stub, home, proj):
    # --- N lanes start and pass auth; each got its baseline --------------------
    lanes = wait_for(lambda: len(lanes_of(home)) == LANES and lanes_of(home), timeout=30)
    check(f"{LANES} lane directories", lanes is not None)
    ready = wait_for(lambda: all(lane_ready(l) for l in lanes), timeout=120)
    check(f"all {LANES} lanes are up and initialized", ready)
    for lane in lanes:
        check(f"lane {lane.n} refuses a missing token", lane.get("/health", token="")[0] == 401)
        check(f"lane {lane.n} refuses a wrong token", lane.get("/health", token="nope")[0] == 401)
        status, reg = lane.get("/registry")
        check(f"lane {lane.n} accepts its token", status == 200)
        stub_provider = [p for p in reg["providers"] if p["key"] == "stub"]
        check(f"lane {lane.n} baseline: the coordinator's provider, key by env var",
              stub_provider and stub_provider[0]["has_api_key"] is True
              and stub_provider[0]["api_key_env"] == "EVO_SWARM_STUB_API_KEY", stub_provider)
        check(f"lane {lane.n} baseline: the coordinator's models, and its default from swarm.lisp",
              [m["id"] for m in reg["models"]] == ["stub-a", "stub-swarm"]
              and reg["settings"].get("model") == "stub-swarm", (reg["models"], reg["settings"]))
        check(f"lane {lane.n} baseline: core tools and the report tool",
              {"read", "write", "edit", "bash", "report"} <= set(t["name"] for t in reg["tools"]))
        names = [t["name"] for t in reg["tools"]]
        check(f"lane {lane.n} in-lanes: ~/.evo/swarm.lisp's ran", "e2e_global_lane_tool" in names)
        check(f"lane {lane.n} in-lanes: the project's loaded a file beside its swarm.lisp",
              "e2e_baseline_tool" in names)
        check(f"lane {lane.n} in-lanes: a later form used that file's package, with lane and lanes",
              f"e2e_lane_{lane.n}_of_{LANES}" in names, names)
        check(f"lane {lane.n} in-lanes: overrides the coordinator's defaults",
              (reg["settings"].get("thinking") == "low") == (lane.n == 1), reg["settings"])
        status, state = lane.get("/state")
        check(f"lane {lane.n} starts idle", state["status"] == "idle", state["status"])
    pids = [l.pid() for l in lanes]
    check("lanes are separate processes", len(set(pids)) == LANES, pids)
    wait_for(lambda: "lanes ○○○○" in term.text(), timeout=20)
    check("the TUI shows every lane idle", "lanes ○○○○" in term.text())

    # --- delegation runs on a lane; its report wakes the idle coordinator -------
    t0 = time.time()
    coordinator_quiet(stub)
    term.type('CALL delegate {"lane":1,"task":"DELAY3 CALL report {\\"done\\":\\"lane one finished\\",'
              '\\"evidence\\":\\"e2e evidence\\",\\"next\\":\\"nothing\\"}"}')
    delegated = wait_for(lambda: stub.find("lane 1", "DELAY3 CALL report", t0), timeout=30)
    check("delegation reaches lane 1 as its prompt", delegated)
    check("the lane is told it is lane 1 (worker prompt note)",
          delegated and delegated["system_has_report_note"])
    woke = wait_for(lambda: stub.find("coordinator", "[lane 1 report] done: lane one finished", t0),
                    timeout=40)
    check("a report reaches the coordinator as input", woke)
    coordinator_before = [r for r in stub.requests()
                          if r["role"] == "coordinator" and t0 <= r["time"] < (woke or {}).get("time", 0)]
    check("the coordinator had gone idle before the report woke it",
          woke and coordinator_before
          and woke["time"] - coordinator_before[-1]["time"] > 1.0,
          [(round(r["time"] - t0, 1), r["last_user"][:40]) for r in coordinator_before])
    check("the report carries its evidence", woke and "evidence: e2e evidence" in woke["last_user"])
    wait_for(lambda: stub.find("coordinator", "[lane 1] run ended", t0), timeout=30)

    # --- interrupt and re-steer a busy lane --------------------------------------
    t1 = time.time()
    term.type("/lane 2")
    wait_for(lambda: "following lane 2" in term.text(), 10)
    coordinator_quiet(stub)
    term.type('CALL delegate {"lane":2,"task":"SLOW long work"}')
    check("lane 2 starts its slow task", wait_for(lambda: stub.find("lane 2", "SLOW long work", t1), 30))
    time.sleep(1)
    status, state = lanes[1].get("/state")
    check("lane 2 is busy", state["status"] == "running", state["status"])
    coordinator_quiet(stub)
    term.type('CALL interrupt_and_steer {"lane":2,"text":"resteered now"}')
    check("lane 2 gets the new instructions",
          wait_for(lambda: stub.find("lane 2", "resteered now", t1), 30))
    check("lane 2 answers them",
          wait_for(lambda: "ok: resteered now" in lanes[1].transcript_text(), 30),
          lanes[1].transcript_text()[-1500:])
    check("/lane 2 shows the lane's transcript live, read-only",
          wait_for(lambda: "[lane 2] ok: resteered now" in term.text(), 15))
    term.type("/lane off")
    slow = stub.find("lane 2", "SLOW long work", t1)
    steer = stub.find("lane 2", "resteered now", t1)
    check("the slow run was cut short (the 6s stream did not finish first)",
          slow and steer and steer["time"] - slow["time"] < 5.5,
          slow and steer and steer["time"] - slow["time"])

    # --- an eval adds a tool to one lane only ------------------------------------
    coordinator_quiet(stub)
    term.type('CALL lane_eval {"lane":3,"code":"(evo:register-tool \\"probe_three\\" :description '
              '\\"e2e probe\\" :schema (quote (:object)) :execute (lambda (a) (declare (ignore a)) '
              '\\"three\\"))"}')
    check("lane 3 has the new tool", wait_for(lambda: "probe_three" in lanes[2].tools(), 30))
    check("lane 4 does not", "probe_three" not in lanes[3].tools())
    check("lane 1 does not", "probe_three" not in lanes[0].tools())

    # --- delegation as a goal, with a done-when the lane's kernel checks -----------
    t_goal = time.time()
    coordinator_quiet(stub)
    term.type('CALL delegate {"lane":3,"task":"start on the goal","objective":'
              '"reach the e2e goal FINISH","done_when":"(= 1 1)"}')
    done = wait_for(lambda: (lambda st: st[1]["goal"] and st[1]["goal"]["status"] == "complete"
                             and st[1]["status"] == "idle")(lanes[2].get("/state")), 60)
    check("a delegated goal runs on the lane until its done-when passes", done)
    status, state = lanes[2].get("/state")
    check("the lane's goal carries the objective and verifier",
          state["goal"] and state["goal"]["objective"] == "reach the e2e goal FINISH"
          and state["goal"]["done_when"] == "(= 1 1)", state["goal"])
    wait_for(lambda: stub.find("coordinator", "[lane 3] run ended", t_goal), 30)

    # --- a worktree lane works in its worktree -------------------------------------
    old_pid = lanes[3].pid()
    coordinator_quiet(stub)
    term.type('CALL lane_worktree {"lane":4,"action":"create"}')
    moved = wait_for(lambda: lanes[3].pid() not in (None, old_pid) and lane_ready(lanes[3]), 60)
    check("lane 4 restarts in its worktree", moved)
    status, reply = lanes[3].eval("(namestring (uiop:getcwd))")
    cwd = json.loads(reply["data"]["values"][0]) if status == 200 else ""
    check("lane 4's working directory is the worktree", "/worktrees/lane-4" in cwd, cwd)
    branches = subprocess.run(["git", "branch", "--list", "swarm/*"], cwd=proj,
                              capture_output=True, text=True).stdout
    check("the worktree has its own branch", "lane-4" in branches, branches)
    t2 = time.time()
    coordinator_quiet(stub)
    term.type('CALL delegate {"lane":4,"task":"CALL write {\\"path\\":\\"from-lane4.txt\\",'
              '\\"content\\":\\"written by lane 4\\"}"}')
    written = wait_for(lambda: os.path.exists(os.path.join(cwd, "from-lane4.txt")), 30)
    check("lane 4 wrote into its worktree", written)
    check("not into the shared directory", not os.path.exists(os.path.join(proj, "from-lane4.txt")))
    wait_for(lambda: stub.find("coordinator", "[lane 4] run ended", t2), 30)

    # --- a killed lane restarts and the coordinator is told ------------------------
    victim = lanes[0]
    old = victim.pid()
    t3 = time.time()
    os.kill(old, signal.SIGKILL)
    told = wait_for(lambda: stub.find("coordinator", "[lane 1] crashed and was restarted", t3), 90)
    check("the coordinator is told lane 1 crashed and restarted", told)
    check("lane 1 runs again under a new pid", victim.pid() not in (None, old))
    check("and was re-initialized", "report" in victim.tools())
    check("its session survived the crash", "lane one finished" in victim.transcript_text())

    # --- the human can watch a lane, read-only --------------------------------------
    term.type("/lanes")
    check("/lanes lists every lane with its state",
          wait_for(lambda: all(f"lane {n} " in term.text() for n in range(1, LANES + 1))
                   and "worktree" in term.text(), 10))

    # --- quitting stops every lane --------------------------------------------------
    pids = [l.pid() for l in lanes]
    term.type("/quit")
    code = term.wait_exit()
    check("quitting exits 0 (supervisor included)", code == 0, code)
    check("every lane process is gone after quit",
          wait_for(lambda: not any(pid_alive(p) for p in pids if p), 30), pids)
    check("no lane answers after quit", all(safe_pid(l) is None for l in lanes))
    coordinator_sessions = glob.glob(os.path.join(home, "sessions", "*", "*.sexp"))
    check("lane journals stay out of the coordinator's /resume list",
          len(coordinator_sessions) == 1, coordinator_sessions)


def safe_pid(lane):
    try:
        return lane.pid()
    except (OSError, ValueError):
        return None


def resumed_run(term, stub, home, proj):
    lanes = lanes_of(home)
    check("resume reuses the same swarm", len(lanes) == LANES and len(glob.glob(
        os.path.join(home, "swarm", "*"))) == 1)
    ready = wait_for(lambda: all(lane_ready(l) for l in lanes), timeout=120)
    check("every lane comes back", ready)
    check("lane 2's session was resumed", "resteered now" in lanes[1].transcript_text())
    check("lane 3 got its eval back (replayed)", "probe_three" in lanes[2].tools())
    check("resumed lanes run their in-lanes forms again",
          all("e2e_baseline_tool" in l.tools() and f"e2e_lane_{l.n}_of_{LANES}" in l.tools()
              for l in lanes))
    check("lane 4 still lacks it", "probe_three" not in lanes[3].tools())
    status, reply = lanes[3].eval("(namestring (uiop:getcwd))")
    cwd = json.loads(reply["data"]["values"][0]) if status == 200 else ""
    check("lane 4 is back in its worktree", "/worktrees/lane-4" in cwd, cwd)
    t = time.time()
    coordinator_quiet(stub)
    term.type('CALL lanes {}')
    listed = wait_for(lambda: [r for r in stub.requests()
                               if r["role"] == "coordinator" and r["time"] >= t
                               and r["last_user"].startswith("lane 1 ")], 30)
    check("the resumed coordinator's lanes tool sees the lanes", listed)
    history = listed and listed[0]["user_texts"]
    check("the coordinator resumed its own conversation",
          history and any("CALL delegate" in u for u in history))
    t = time.time()
    coordinator_quiet(stub)
    term.type('CALL lane_reports {"lane":1}')
    reports = wait_for(lambda: [r for r in stub.requests()
                                if r["role"] == "coordinator" and r["time"] >= t
                                and "[lane 1 report] done: lane one finished" in r["last_user"]], 30)
    check("a lane's reports outlive a restart (read from its journal)", reports)
    pids = [l.pid() for l in lanes]
    term.type("/quit")
    code = term.wait_exit()
    check("the resumed swarm quits cleanly too", code == 0, code)
    check("resumed lanes stop on quit", wait_for(lambda: not any(pid_alive(p) for p in pids if p), 30))


def no_secrets(home):
    leaks = []
    for path in glob.glob(os.path.join(home, "**", "*"), recursive=True):
        if os.path.isfile(path) and not path.endswith("init.lisp"):
            try:
                if SECRET in open(path, errors="replace").read():
                    leaks.append(path)
            except OSError:
                pass
    check("no secret in any journal, log or lane file", not leaks, leaks)
    journals = glob.glob(os.path.join(home, "swarm", "*", "lane-*", "sessions", "*.sexp"))
    check("lanes journaled their sessions", len(journals) >= 3, journals)


if __name__ == "__main__":
    sys.exit(main())
