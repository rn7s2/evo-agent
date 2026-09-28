# evo-swarm — one coordinator, a pool of worker lanes

`evo-swarm` runs one **coordinator** agent in your terminal and a pool of
**lanes**: worker agents, each a separate `evo serve` process with its own
context, working in parallel. You talk only to the coordinator. It explores
enough to split the work into lane-sized pieces with clear done criteria,
delegates them, integrates and verifies what comes back, and reports to you.

```sh
evo-swarm                 # 6 lanes, coordinator TUI here
evo-swarm --workers 3
evo-swarm --resume        # the last swarm here: coordinator and lanes
```

It is a separate program on top of evo: its own system (`evo-swarm.asd`,
`swarm/`), its own binary (`build/evo-swarm`, installed beside `evo`). The
evo binary contains none of it — `make test` checks that the `evo` system
loads without the swarm — and lanes are the evo binary itself, driven only
through its public HTTP API ([docs/serve.md](serve.md)).

## Lanes are lanes, not roles

A lane is like a CPU core: interchangeable capacity, not a specialist. Each is
`evo serve --no-userspace` on loopback, started with the swarm and idle until
given work. The coordinator decides what each one does, task by task.

- **Own process, own token, own journal.** Each lane has a random bearer
  token, its own port, and its own session journal under
  `~/.evo/swarm/<swarm-id>/lane-N/sessions/` — never in the coordinator's
  `/resume` list (the lane runs with `EVO_SESSIONS_DIR` pointing there).
- **Shared directory by default.** Lanes work in the coordinator's working
  directory. When a task needs isolation, the coordinator gives that lane its
  own **git worktree** and branch (`lane_worktree`), restarting it there with
  its session intact, and merges the branch itself when the lane reports done.
- **Lanes never talk to each other.** They report to the coordinator, which
  relays whatever one lane needs from another.
- **Supervised.** A lane runs under evo's own supervisor: a crash restarts it
  with `--resume`. The coordinator notices the new process, re-initializes it
  (below) and is told: `[lane N] crashed and was restarted …`. A lane whose
  coordinator dies shuts itself down (`EVO_SERVE_WATCH_PID`), so lanes never
  outlive the swarm that drives them.
- **Stopped with the swarm.** Quitting the coordinator shuts every lane down
  cleanly (`POST /shutdown`), killing any that do not stop in time.

## The coordinator

The coordinator is an ordinary evo TUI session with every tool, plus:

| Tool | What it does |
|---|---|
| `lanes` | Every lane: idle/working, step clock, current task, worktree, reports. |
| `delegate` | Give a lane a task — the complete instructions, since the lane cannot see this conversation — and optionally an `objective` with `done_when` (a Lisp form), which becomes the lane's goal. Picks the first idle lane unless one is named. Returns at once. |
| `steer_lane` | Guidance for a working lane, seen at its next turn boundary. |
| `interrupt_lane` | Stop what a lane is doing. |
| `interrupt_and_steer` | Stop it and give it new instructions in one step. |
| `lane_command` | Any slash command in a lane (`/goal`, `/model`, `/compact`, `/lore`, …), exactly as typed in its TUI. |
| `lane_eval` | Evaluate Lisp in a lane — to install a tool, an MCP server, a capability it asked for. Kept and replayed if the lane restarts (unless `keep` is false). |
| `lane_transcript` | The last messages of a lane's context. |
| `lane_reports` | What a lane has reported, read from its journal. |
| `restart_lane` | `reinit` (re-run its init in place), `restart` (new process, same session), `fresh` (new session). |
| `lane_worktree` | `create` a git worktree + branch for an idle lane, or `remove` it (the branch is kept to merge). |

Every tool reaches a lane through serve's HTTP API and nothing else.

### What reaches the coordinator

The coordinator subscribes to every lane's event stream (`GET /events`, with
`Last-Event-ID` resume). What it must hear becomes its **input**, exactly like
a message you type: queued to its next turn boundary when it is working, and
starting a run — waking it — when it is idle. You see each one in the
scrollback too.

- `[lane N report] done: … evidence: … next: … blocked: … requests: …` — a
  lane called its `report` tool;
- `[lane N] run ended (stop|error|aborted) — task: …` — a lane went idle;
- `[lane N] error: …` — a lane's task failed;
- `[lane N] crashed and was restarted …` / `[lane N] is down …`.

So the coordinator never polls: it delegates, ends its turn, and is woken.

## Watching the lanes

Input always goes to the coordinator; lanes are only watched.

- The status line shows one glyph per lane — `●` working, `◐` compacting,
  `○` idle, `◌` starting, `✗` down — and how many are busy.
- `/lanes` lists every lane: state, step clock (time in its current step),
  current task, worktree, reports, pid.
- `/lane N` follows lane N's transcript live in the scrollback (its text and
  tool calls, prefixed `[lane N]`); `/lane off` stops.

## Worker init is a program

A lane boots with the kernel and core extensions only. What it then becomes is
decided by **generators**: functions of the lane and the swarm that return
Lisp forms, which the swarm evaluates in the lane (`POST /eval`) before it gets
any work — and again whenever it restarts.

`~/.evo/swarm.lisp`, then `<project>/.evo/swarm.lisp`, are evaluated in the
coordinator after its init files, and define them. A commented sample is
[docs/examples/swarm.lisp](examples/swarm.lisp).

```lisp
(evo.swarm:add-worker-init :lint-tools
  (lambda (lane swarm)
    (declare (ignore swarm))
    (when (member (evo.swarm:lane-n lane) '(1 2))
      '((evo:load-extension "/home/me/tools/lint.lisp")))))
```

| API | |
|---|---|
| `(evo.swarm:add-worker-init name fn)` | Add, or replace in place, a generator. `fn` takes `(lane swarm)` and returns forms. Generators run in order. |
| `(evo.swarm:remove-worker-init name)` | Remove one — `:baseline` included. |
| `(evo.swarm:default-worker-init lane swarm)` | The baseline's forms, for a generator that extends it. |
| `(evo.swarm:set-lane-tools names &key lanes)` | Limit lanes (all, or those numbered) to `names`; `report` is always kept. `nil` names = every tool. |
| `(evo.swarm:set-coordinator-tools names)` | Limit the coordinator. |
| `(evo.swarm:set-worker-note text)`, `(evo.swarm:set-coordinator-note text)` | Replace the prompt notes (below). |
| `(evo:set-setting :swarm-workers n)` | Default lane count. |
| `evo.swarm:lane-n`, `lane-cwd`, `lane-worktree`, `lane-task`, `lane-state` | What a generator knows about its lane. |

**The baseline** (`:baseline`) gives each lane what the coordinator runs on:

- every provider the coordinator has registered, and every model;
- the coordinator's model, provider and thinking level as the lane's defaults;
- the core tools (every lane has them) and the **`report` tool**;
- the lane's **prompt note**, and its tool limit if one is set;
- a run for any goal continuation a resumed lane had queued.

**No secret travels as data.** A provider whose key comes from an environment
variable is registered in the lane with that variable's *name*. One registered
with a literal `:api-key` is registered in the lane with a swarm-private name
(`EVO_SWARM_<PROVIDER>_API_KEY`), and the key itself is set only in the lane
process's environment. No form, prompt, journal or log — the coordinator's or
a lane's — ever holds a key; the end-to-end test greps all of them.

At runtime the coordinator can add more to a lane with `lane_eval`, when a
lane asks for a capability in a report and the coordinator agrees. What it
evaluates is recorded and replayed if that lane restarts.

## Behavior lives in prompt notes

The coordinator's role (explore, split into lane-sized pieces with done
criteria, delegate, integrate, verify, report) and a lane's (do the task in
scope, report after each meaningful piece of work, never go quiet, ask for
what you lack) are prompt notes, `swarm-coordinator` and `swarm-worker`.
`set-coordinator-note` and `set-worker-note` in swarm.lisp replace them: each
is a FORMAT control — the coordinator's takes the lane count; a lane's takes
its number, its number again, the lane count, and a sentence about where it
works (the shared directory, or its worktree and branch).

## Resuming

The coordinator's journal records the swarm as `:custom` state: its id and
directory, and for each lane its worktree, branch, task and the code evaluated
into it. `evo-swarm --resume` reopens the coordinator's session and brings
every lane back from that record — each resuming its own session, in its own
worktree, re-initialized and with its evaluations replayed. A coordinator that
crashes is restarted by evo-swarm's supervisor the same way.

## Command line

```text
evo-swarm [--workers N] [--resume [path]] [--model id] [--thinking level]
          [--evo path] [--no-userspace] [--no-supervisor]
```

`--evo` names the evo binary lanes run; by default the one beside
`evo-swarm`, then `EVO_BINARY`, then `evo` on `PATH`. evo-swarm needs a
terminal — for a headless single agent, use `evo serve`.

Files: `~/.evo/swarm/<swarm-id>/` holds each lane's `lane-N/` directory
(`sessions/`, `token` (0600), `url`, `lane.log`) and `worktrees/lane-N/`.

## Design notes

- **Why processes, not threads.** A lane is a whole evo — its own journal,
  context, extensions, crash domain — so a lane that wedges or crashes cannot
  take the coordinator or another lane with it, and everything evo already
  guarantees (supervision, resume, the journal as truth) holds per lane for
  free. The price is a process per lane, which is cheap next to the model.
- **Why only HTTP.** The coordinator uses the same API a person or another
  program would. Nothing about a lane is private to the swarm, so a lane can
  be inspected with curl (its `url` and `token` files), and serve's API is
  proven by a second real client.
- **Why generators.** A lane's setup differs with the project, the lane and
  the task; data can express "these models", but only a program can express
  "lanes 1-2 get the lint tools, the rest a cheaper model". And forms, not
  shared state, cross into the lane — nothing leaks from the coordinator's
  image by accident.
- **Tested end to end.** `make swarm-test` (`tests/swarm-e2e.py`, also in CI)
  drives the coordinator's TUI through a pseudo-terminal, against the stub
  Messages endpoint scripting coordinator and lanes: lanes start and
  authenticate, each gets its baseline with no secret in any journal,
  delegation, a report waking the idle coordinator, interrupt and re-steer, an
  eval reaching one lane only, a worktree lane writing in its worktree, a
  killed lane restarting with the coordinator told, quit stopping every lane,
  and `--resume` restoring it all.
