#!/usr/bin/env python3
"""swarm-e2e.py — evo-swarm end to end, with no backend.

The coordinator is a TUI, so this drives it the way a person would: through a
pseudo-terminal, typing prompts.  The "model" is tests/stub-messages.py, which
scripts both the coordinator and the lanes from what they are sent — a prompt
starting `CALL <tool> {json}` becomes that tool call — and logs every request
with who made it (the coordinator or lane N, read from the swarm prompt
notes).  Lanes are checked through their own HTTP API (their url and token
files are in the swarm directory), exactly as the coordinator reaches them.

Proves: the build's `evo` is a soft link to the swarm binary and runs it;
N lanes start and pass auth, skipping a coordinator model whose API
they lack; swarm.lisp sets the coordinator's model;
every lane runs the global and the project swarm.lisp's in-lanes forms (one
at a time, *load-truename*, the lane variables, a package an earlier form
loaded, overriding the coordinator's defaults); each got its
baseline and no secret is in any journal; delegation runs on a lane; a report wakes the idle coordinator;
interrupt and re-steer of a busy lane, and a bare interrupt; a report with goal
complete closes the lane's goal in the same run, with no continuation; an eval adds a tool to one lane only; a
worktree lane works in its worktree; a killed lane restarts and the
coordinator is told; quitting stops every lane; `evo-swarm --resume` restores
coordinator and lanes, and so does `/resume` of that session in a fresh swarm.

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

from clean_env import clean  # the environment a test's children start from

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


def was_told(stub, needle, after=0.0):
    """Every request the coordinator made after AFTER whose newest user text
carries NEEDLE — what it was told, rather than what it was sent."""
    return [r for r in stub.requests()
            if r["role"] == "coordinator" and r["time"] >= after and needle in r["last_user"]]


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
    """A lane, reached the way the coordinator reaches it: through the ready
    file the lane wrote, speaking CONTRACT.md's protocol (§4.3, §5, §6)."""

    def __init__(self, directory):
        self.dir = directory
        self.n = int(directory.rstrip("/").rsplit("-", 1)[1])
        self.rid = 0

    def ready(self):
        return json.load(open(os.path.join(self.dir, "ready.json")))

    def url(self):
        return self.ready()["url"]

    def token(self):
        return self.ready()["token"]

    def request(self, method, path, body=None, token=None, timeout=30):
        port = self.ready()["port"]
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

    def op(self, name, args=None):
        self.rid += 1
        return self.request("POST", "/ops",
                            {"rid": f"lane-{self.n}-{self.rid}", "op": name,
                             "args": args or {}})

    def snapshot(self, topics="session", items=200):
        status, snap = self.get(f"/snapshot?topics={topics}&items={items}")
        return snap if status == 200 else None

    def state(self):
        snap = self.snapshot() or {}
        return (snap.get("topics") or {}).get("session", {}).get("state") or {}

    def items(self):
        snap = self.snapshot(items=200) or {}
        return (snap.get("topics") or {}).get("session", {}).get("items") or []

    def transcript_text(self):
        return json.dumps(self.items())

    def pid(self):
        try:
            return self.ready()["pid"]
        except (OSError, ValueError, KeyError):
            return None

    def catalog(self):
        status, cat = self.get("/catalog")
        return cat if status == 200 else {}

    def tools(self):
        return [t["name"] for t in self.catalog().get("tools") or []]

    def eval(self, form):
        return self.op("eval", {"code": form})

    def eval_value(self, form):
        """The first value of FORM, as the eval op prints it."""
        status, reply = self.eval(form)
        if status != 200 or not reply.get("ok"):
            return None
        values = (reply.get("result") or {}).get("values") or []
        return values[0] if values else None


def lanes_of(home, swarm="*"):
    dirs = sorted(glob.glob(os.path.join(home, "swarm", swarm, "lane-*")),
                  key=lambda d: int(d.rsplit("-", 1)[1]))
    return [Lane(d) for d in dirs]


def lane_ready(lane):
    """Up, authenticated, and initialized (the report tool is its baseline)."""
    return os.path.exists(os.path.join(lane.dir, "ready.json")) and "report" in lane.tools()


def pid_alive(pid):
    try:
        os.kill(pid, 0)
        return True
    except OSError:
        return False


def check_alias():
    """`evo` is this program: the build makes it a soft link to evo-swarm (a
    copy on Windows), so running it runs the swarm."""
    evo = os.path.join(BUILD, "evo")
    check("the evo alias is a soft link to evo-swarm",
          os.path.islink(evo) and os.readlink(evo) == "evo-swarm", evo)
    out = subprocess.run([evo, "--version"], capture_output=True, text=True, timeout=120)
    check("the evo alias runs the swarm binary",
          out.returncode == 0 and out.stdout.strip().startswith("evo-swarm"),
          f"exit {out.returncode}: {out.stdout.strip()!r} {out.stderr.strip()!r}")


def main():
    if not os.access(SWARM, os.X_OK):
        print(f"swarm-e2e: no binary at {SWARM} (make build first)")
        return 1
    check_alias()
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
                # An API only the coordinator has — as an extension the lanes
                # never load would define — with a model on it.  The lanes
                # must come up without that model rather than fail.
                "(evo:register-api :e2e-ext-api (make-instance 'evo:provider-api))\n"
                '(evo:register-model "stub-ext" :provider :stub :api :e2e-ext-api '
                ':context-window 200000 :max-output 8000)\n'
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
    # Neither --evo nor EVO_BINARY: the swarm must find evo-agent beside its
    # own binary, where the build puts it — the path a real install takes.
    # (swarm-serve-e2e.py covers the explicit override.)
    # The swarm, its lanes and their servers start from a regular environment:
    # nothing of the session this test was started from — its supervision, its
    # sessions directory, its token — and no provider key, since every model
    # here is a stub (tests/clean_env.py).
    env = clean(EVO_HOME=home, TERM="xterm-256color")

    term = Terminal(["--workers", str(LANES)], proj, env)
    try:
        first_run(term, stub, home, proj)
        term = Terminal(["--resume"], proj, env)
        resumed_run(term, stub, home, proj)
        term = Terminal(["--workers", "2"], proj, env)
        resume_in_session(term, home)
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
        cat = lane.catalog()
        check(f"lane {lane.n} accepts its token", bool(cat))
        check(f"lane {lane.n} wrote its own ready file, mode 0600",
              os.stat(os.path.join(lane.dir, "ready.json")).st_mode & 0o777 == 0o600)
        check(f"lane {lane.n} is a session of its own: the journal says lane",
              lane.state()["session"]["program"] == "lane",
              (lane.state()["session"], lane.ready().get("program")))
        stub_provider = [p for p in cat["providers"] if p["name"] == "stub"]
        check(f"lane {lane.n} baseline: the coordinator's provider, key by env var",
              stub_provider and stub_provider[0]["has_key"] is True
              and stub_provider[0]["key_env"] == "EVO_SWARM_STUB_API_KEY", stub_provider)
        check(f"lane {lane.n} baseline: a coordinator model on an API the lane lacks is skipped",
              "stub-ext" not in [m["id"] for m in cat["models"]], cat["models"])
        check(f"lane {lane.n} baseline: the coordinator's models, and its default from swarm.lisp",
              [m["id"] for m in cat["models"]] == ["stub-a", "stub-swarm"]
              and cat["default_model"]["id"] == "stub-swarm",
              (cat["models"], cat["default_model"]))
        # The effort a session started here would run on, and for a lane that
        # is what its own launch left it at — the same meaning the coordinator's
        # document has, stated the same way.
        check(f"lane {lane.n} catalog: the default thinking is the level its own state reports",
              cat.get("default_thinking") == lane.state().get("thinking"),
              (cat.get("default_thinking"), lane.state().get("thinking")))
        check(f"lane {lane.n} baseline: core tools and the report tool",
              {"read", "write", "edit", "bash", "report"} <= set(lane.tools()))
        names = lane.tools()
        check(f"lane {lane.n} in-lanes: ~/.evo/swarm.lisp's ran", "e2e_global_lane_tool" in names)
        check(f"lane {lane.n} in-lanes: the project's loaded a file beside its swarm.lisp",
              "e2e_baseline_tool" in names)
        check(f"lane {lane.n} in-lanes: a later form used that file's package, with lane and lanes",
              f"e2e_lane_{lane.n}_of_{LANES}" in names, names)
        check(f"lane {lane.n} in-lanes: overrides the coordinator's defaults",
              (lane.state().get("thinking") == "low") == (lane.n == 1), lane.state())
        check(f"lane {lane.n} starts idle", lane.state()["status"] == "idle", lane.state()["status"])
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
    check("lane 2 is busy", lanes[1].state()["status"] == "running", lanes[1].state()["status"])
    coordinator_quiet(stub)
    term.type('CALL interrupt_lane {"lane":2,"text":"resteered now"}')
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

    # --- interrupt a busy lane without new instructions -----------------------------
    t_int = time.time()
    coordinator_quiet(stub)
    term.type('CALL delegate {"lane":2,"task":"SLOW more long work"}')
    check("lane 2 starts another slow task",
          wait_for(lambda: stub.find("lane 2", "SLOW more long work", t_int), 30))
    time.sleep(1)
    coordinator_quiet(stub)
    term.type('CALL interrupt_lane {"lane":2}')
    stopped = wait_for(lambda: stub.find("coordinator", "[lane 2] run ended (aborted)", t_int), 30)
    if not stopped:
        print("DEBUG-STUB", [(r["role"], round(r["time"] - t_int, 2), r["last_user"][:70])
                             for r in stub.requests() if r["time"] > t_int])
        print("DEBUG-TUI", [l for l in term.text().splitlines() if "lane 2" in l][-8:])
    check("interrupt_lane alone stops the lane", stopped)
    check("...and leaves it idle, given nothing new",
          lanes[1].state()["status"] == "idle", lanes[1].state()["status"])
    check("...and sends it no new prompt",
          not any(r["role"] == "lane 2" and r["time"] > (stopped or {}).get("time", t_int)
                  for r in stub.requests()))

    # --- an eval adds a tool to one lane only ------------------------------------
    coordinator_quiet(stub)
    term.type('CALL lane_eval {"lane":3,"code":"(evo:register-tool \\"probe_three\\" :description '
              '\\"e2e probe\\" :schema (quote (:object)) :execute (lambda (a) (declare (ignore a)) '
              '\\"three\\"))"}')
    check("lane 3 has the new tool", wait_for(lambda: "probe_three" in lanes[2].tools(), 30))
    check("lane 4 does not", "probe_three" not in lanes[3].tools())
    check("lane 1 does not", "probe_three" not in lanes[0].tools())

    # --- delegation as a goal ------------------------------------------------------
    t_goal = time.time()
    coordinator_quiet(stub)
    term.type('CALL delegate {"lane":3,"task":"start on the goal","objective":'
              '"reach the e2e goal FINISH"}')
    done = wait_for(lambda: (lambda st: st.get("goal") and st["goal"]["status"] == "complete"
                             and st["status"] == "idle")(lanes[2].state()), 60)
    check("a delegated goal runs on the lane until it completes", done)
    state = lanes[2].state()
    check("the lane's goal carries the objective",
          state["goal"] and state["goal"]["objective"] == "reach the e2e goal FINISH",
          state["goal"])
    ended = wait_for(lambda: was_told(stub,
                                   "[lane 3] run ended (stop) — goal: complete", t_goal), 30)
    check("the run end tells the coordinator the goal is complete",
          ended, [r["last_user"][-200:] for r in was_told(stub, "run ended", t_goal)][-2:])

    # --- a report that delivers the objective closes the goal ----------------------
    t_rep = time.time()
    coordinator_quiet(stub)
    term.type('CALL delegate {"lane":3,"task":"CALL report {\\"done\\":\\"objective delivered\\",'
              '\\"goal\\":\\"complete\\"}","objective":"deliver it by report"}')
    done = wait_for(lambda: (lambda st: st.get("goal")
                             and st["goal"]["objective"] == "deliver it by report"
                             and st["goal"]["status"] == "complete"
                             and st["status"] == "idle")(lanes[2].state()), 60)
    check("a report with goal complete closes the lane's goal", done)
    rep = wait_for(lambda: stub.find("coordinator", "[lane 3 report] done: objective delivered", t_rep), 30)
    check("the report tells the coordinator the goal is complete",
          rep and "goal: complete" in rep["last_user"], rep and rep["last_user"][-300:])
    settled = wait_for(lambda: (lambda ends: ends and "goal: complete" in ends[-1]["last_user"])
                       (was_told(stub, "[lane 3] run ended", t_rep)), 30)
    check("the lane settles once, its goal complete", settled,
          [r["last_user"][-200:] for r in was_told(stub, "[lane 3] run ended", t_rep)])
    check("no continuation sent the lane back to work it had delivered",
          not any(r["role"] == "lane 3" and r["time"] >= t_rep
                  and "You are idle but your goal is still active" in r["last_user"]
                  for r in stub.requests()))

    # --- a worktree lane works in its worktree -------------------------------------
    old_pid = lanes[3].pid()
    coordinator_quiet(stub)
    term.type('CALL lane_worktree {"lane":4,"action":"create"}')
    moved = wait_for(lambda: lanes[3].pid() not in (None, old_pid) and lane_ready(lanes[3]), 60)
    check("lane 4 restarts in its worktree", moved)
    cwd = lane_cwd(lanes[3])
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
    told = wait_for(lambda: stub.find("coordinator", "[lane 1] crashed", t3), 90)
    check("the coordinator is told lane 1 crashed", told)
    check("...and that it came back on its session",
          wait_for(lambda: stub.find("coordinator", "[lane 1] restarted", t3), 60))
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


def lane_cwd(lane):
    """The lane's working directory, asked of the lane itself (its eval op)."""
    printed = lane.eval_value("(namestring (uiop:getcwd))")
    try:
        return json.loads(printed) if printed else ""
    except ValueError:
        return printed or ""


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
    cwd = lane_cwd(lanes[3])
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


def resume_in_session(term, home):
    """A fresh evo-swarm, then /resume of the first swarm's coordinator
    session: its own lanes stop, and the recorded swarm's come back, each
    resuming its session."""
    old_swarm = glob.glob(os.path.join(home, "swarm", "*"))[0]
    session = glob.glob(os.path.join(home, "sessions", "*", "*.sexp"))[0]
    fresh = wait_for(lambda: [d for d in glob.glob(os.path.join(home, "swarm", "*"))
                              if d != old_swarm], timeout=30)
    check("a fresh evo-swarm starts its own swarm", fresh)
    new_lanes = lanes_of(home, os.path.basename(fresh[0]))
    check("...with its own lanes", len(new_lanes) == 2)
    check("...up and initialized",
          wait_for(lambda: all(lane_ready(l) for l in new_lanes), timeout=120))
    new_pids = [l.pid() for l in new_lanes]
    old_lanes = lanes_of(home, os.path.basename(old_swarm))
    term.type(f"/resume {session}")
    check("/resume says it switches swarms",
          wait_for(lambda: "switching to this session's swarm" in term.text(), 30))
    check("/resume stops the fresh swarm's lanes",
          wait_for(lambda: not any(pid_alive(p) for p in new_pids if p), 60), new_pids)
    check("/resume brings the session's own lanes back",
          wait_for(lambda: all(lane_ready(l) for l in old_lanes), timeout=120))
    check("/resume: lane 2's session was resumed, not started empty",
          "resteered now" in old_lanes[1].transcript_text())
    check("/resume: lane 1's too", "lane one finished" in old_lanes[0].transcript_text())
    check("/resume: lane 3 got its eval back", "probe_three" in old_lanes[2].tools())
    cwd = lane_cwd(old_lanes[3])
    check("/resume: lane 4 is back in its worktree", "/worktrees/lane-4" in cwd, cwd)
    pids = [l.pid() for l in old_lanes]
    term.type("/quit")
    check("the switched swarm quits cleanly", term.wait_exit() == 0)
    check("its lanes stop on quit",
          wait_for(lambda: not any(pid_alive(p) for p in pids if p), 30))


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
