#!/usr/bin/env python3
"""The supervisor, live (CONTRACT §1).

What a client is promised when a supervised server dies:

S3  it comes back on the exact session it was on (--resume <that path>, never
    a bare --resume) and the exact port it bound (--port 0 is chosen once);
S4  the ready file names the parent that will bring it back, the child is told
    who its supervisor is and how many times it has been restarted;
F4  an idle server — one that has answered nothing, whose journal was never
    written — comes back on that same session too, and the journal lands at
    the path it promised when it finally has something to say;
T1  the bearer token is minted once, in the parent: a client that held the URL
    and token keeps working across a restart.  evo-agent's frontend and
    evo-swarm's coordinator both;
N1  a client's `--prompt-note` file — a serve flag that is not journalled, so
    the command line is the only place a restarted child can learn it from —
    is on the restarted child's command line and in its system prompt.

Both binaries, over HTTP only, with no backend.  Needs build/evo-agent and
build/evo-swarm beside each other.
"""
import http.client
import json
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import time

EVO = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else "build/evo-agent")
SWARM = os.path.join(os.path.dirname(EVO), "evo-swarm")
failed = []


def check(name, ok, detail=""):
    print(("ok   " if ok else "FAIL ") + name + ("" if ok else (f"  {detail}" if detail else "")))
    if not ok:
        failed.append(name)


def base_env(home):
    """A clean environment: this script may itself run inside an evo session,
    and an inherited EVO_SUPERVISED_CHILD or EVO_NO_SUPERVISOR would make the
    binary skip its own supervisor — the very thing under test."""
    env = {k: v for k, v in os.environ.items()
           if k in ("PATH", "TERM", "LANG", "TMPDIR", "SHELL", "USER")}
    env["HOME"] = home
    return env


def alive(pid):
    try:
        os.kill(pid, 0)
        return True
    except OSError:
        return False


def cmdline(pid):
    return subprocess.run(["ps", "-ww", "-o", "command=", "-p", str(pid)],
                          capture_output=True, text=True).stdout.strip()


def envline(pid):
    return subprocess.run(["ps", "-Eww", "-o", "command=", "-p", str(pid)],
                          capture_output=True, text=True).stdout.strip()


def read_ready(path):
    try:
        with open(path) as fh:
            return json.load(fh)
    except Exception:
        return None


def wait_ready(path, timeout=60):
    end = time.time() + timeout
    while time.time() < end:
        d = read_ready(path)
        if d and d.get("port") and d.get("pid") and alive(d["pid"]):
            return d
        time.sleep(0.05)
    return None


def wait_restart(path, old_pid, timeout=90):
    end = time.time() + timeout
    while time.time() < end:
        d = read_ready(path)
        if d and d.get("pid") and d["pid"] != old_pid and alive(d["pid"]):
            return d
        time.sleep(0.1)
    return None


def get(port, path, token):
    conn = http.client.HTTPConnection("127.0.0.1", port, timeout=15)
    conn.request("GET", path, headers={"Authorization": "Bearer " + token})
    status = conn.getresponse().status
    conn.close()
    return status


def op(port, token, name, args, timeout=30, rid="r1"):
    # RID is the op log's key: two calls with the same one are one op, and the
    # second is answered from the cache (CONTRACT §5.5).  A test that asks two
    # different questions has to say which is which.
    conn = http.client.HTTPConnection("127.0.0.1", port, timeout=timeout)
    conn.request("POST", "/ops", body=json.dumps({"rid": rid, "op": name, "args": args}).encode(),
                 headers={"Authorization": "Bearer " + token, "Content-Type": "application/json"})
    raw = conn.getresponse().read()
    conn.close()
    try:
        return json.loads(raw)
    except ValueError:
        return raw


JOURNAL_ONE = ("(progn (evo.journal:append-entry (evo.kernel:agent-journal evo:*agent*) "
               "'(:type :message :message (:role :assistant :stop-reason :stop "
               ":model \"stub\" :content ((:type :text :text \"hi\"))))) :journaled)")


def kill_tree(parent):
    """The parent and everything it started: the process group this script
    created for it (start_new_session=True), and nothing else — never a kill
    by name or command line."""
    try:
        os.killpg(os.getpgid(parent.pid), signal.SIGKILL)
    except Exception:
        try:
            parent.kill()
        except Exception:
            pass
    try:
        parent.wait(timeout=10)
    except Exception:
        pass


def case_agent(work):
    """evo-agent serve: S3, S4, F4, T1 — and again after a journalled turn."""
    home = os.path.join(work, "home")
    proj = os.path.join(work, "proj")
    os.makedirs(home)
    os.makedirs(proj)
    ready = os.path.join(work, "ready.json")
    log = open(os.path.join(work, "agent.log"), "w")
    # A client's own prompt note (`--prompt-note`, docs/serve.md): written
    # before the launch, and never journalled — the restarted child is given
    # the flag again, which is the only way it can still know the note.
    note = os.path.join(work, "gui-renderer.md")
    with open(note, "w") as f:
        f.write("The client renders your output itself: NOTE-MARKER-RESTART.\n")
    parent = subprocess.Popen([EVO, "serve", "--port", "0", "--ready-file", ready,
                               "--prompt-note", note],
                              cwd=proj, env=base_env(home), stdout=log,
                              stderr=subprocess.STDOUT, text=True,
                              start_new_session=True)
    try:
        first = wait_ready(ready)
        check("S3: the supervisor publishes a ready file", first is not None)
        if not first:
            return
        port1, pid1 = first["port"], first["pid"]
        path1 = (first.get("session") or {}).get("path")
        id1 = (first.get("session") or {}).get("id")
        check("S4: the ready file names the supervisor's pid",
              first.get("supervisor_pid") == parent.pid, (first.get("supervisor_pid"), parent.pid))
        check("the ready file starts at restarts 0", first.get("restarts") == 0, first)
        check("S5': --port 0 was chosen once, not left at 0", port1 not in (0, None), port1)
        check("the child's environment names its supervisor",
              "EVO_SUPERVISOR_PID=" in envline(pid1))
        check("T1: the token reaches a client at all", get(port1, "/health", first["token"]) == 200)

        # F4: nothing has been journalled yet — an idle server.
        check("F4: an idle session has no journal on disk yet", not os.path.exists(path1), path1)

        os.kill(pid1, signal.SIGKILL)
        second = wait_restart(ready, pid1)
        check("S3: the child comes back", second is not None)
        if not second:
            return
        check("S3: the ready file counts the restart", second.get("restarts") == 1, second)
        check("S3: the restarted child has the same port", second["port"] == port1,
              (port1, second["port"]))
        check("F4: the idle session comes back on its own path",
              (second.get("session") or {}).get("path") == path1,
              (path1, (second.get("session") or {}).get("path")))
        check("F4: and keeps its session id, so the client stays where it was",
              (second.get("session") or {}).get("id") == id1,
              (id1, (second.get("session") or {}).get("id")))
        argv2 = cmdline(second["pid"])
        check("F4: the idle restart resumes that exact path",
              f"--resume {path1}" in argv2 or f"--resume {os.path.realpath(path1)}" in argv2, argv2)
        check("S3: the restart pins the port it bound", f"--port {port1}" in argv2, argv2)
        check("S3: the restart passes no bare --resume",
              not argv2.rstrip().endswith("--resume") and "--resume --" not in argv2, argv2)
        check("the restarted child has a new epoch", second["epoch"] != first["epoch"],
              (first["epoch"], second["epoch"]))
        check("N1: the restarted child is given the --prompt-note flag again",
              f"--prompt-note {note}" in argv2 or f"--prompt-note {os.path.realpath(note)}" in argv2,
              argv2)
        noted = op(second["port"], second["token"], "eval", {"code":
            '(if (search "NOTE-MARKER-RESTART" (evo.kernel:build-system-prompt nil)) :in :absent)'},
            rid="n1")
        check("N1: ...and the note is in the restarted child's system prompt",
              (noted.get("result") or {}).get("values") == [":in"], noted)
        check("T1: the token is the one the client already holds",
              second["token"] == first["token"], (first["token"][:16], second["token"][:16]))
        check("T1: the client's old URL and token still answer",
              second["url"] == first["url"] and get(second["port"], "/health", first["token"]) == 200)

        # The session the restart promised is the one it writes to.
        op(second["port"], second["token"], "eval", {"code": JOURNAL_ONE})
        check("F4: the journal lands at the path the restart promised",
              os.path.exists(path1), path1)

        os.kill(second["pid"], signal.SIGKILL)
        third = wait_restart(ready, second["pid"])
        check("S3: a second restart comes back too", third is not None)
        if third:
            check("S3: still the same session and port",
                  (third.get("session") or {}).get("path") == path1 and third["port"] == port1,
                  third)
            check("T1: and still the same token across both restarts",
                  third["token"] == first["token"], third["token"][:16])
    finally:
        kill_tree(parent)
        log.close()


def case_swarm(work):
    """evo-swarm serve: T1 for the coordinator's own frontend."""
    home = os.path.join(work, "home")
    proj = os.path.join(work, "proj")
    os.makedirs(home, exist_ok=True)
    os.makedirs(proj, exist_ok=True)
    ready = os.path.join(work, "swarm-ready.json")
    log = open(os.path.join(work, "swarm.log"), "w")
    parent = subprocess.Popen([SWARM, "serve", "--port", "0", "--ready-file", ready,
                               "--watch-stdin", "--workers", "1", "--evo", EVO],
                              cwd=proj, env=base_env(home), stdin=subprocess.PIPE,
                              stdout=log, stderr=subprocess.STDOUT, text=True,
                              start_new_session=True)
    try:
        first = wait_ready(ready, timeout=90)
        check("swarm: the coordinator publishes a ready file", first is not None)
        if not first:
            return
        os.kill(first["pid"], signal.SIGKILL)
        second = wait_restart(ready, first["pid"], timeout=90)
        check("swarm: the coordinator comes back", second is not None)
        if second:
            check("swarm T1: the token is the one the client already holds",
                  second["token"] == first["token"],
                  (first["token"][:16], second["token"][:16]))
            check("swarm T1: and so are the URL and port",
                  second["url"] == first["url"], (first["url"], second["url"]))
            check("swarm T1: the client's old URL and token still answer",
                  get(second["port"], "/health", first["token"]) == 200)
    finally:
        kill_tree(parent)
        log.close()


def main():
    work = tempfile.mkdtemp(prefix="evo-supervisor-")
    try:
        case_agent(work)
        case_swarm(work)
    finally:
        shutil.rmtree(work, ignore_errors=True)
    print(f"\nsupervisor-e2e: {len(failed)} failed")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
