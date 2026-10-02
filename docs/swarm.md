# evo-swarm — one coordinator, a pool of worker lanes

`evo-swarm` runs one **coordinator** agent in your terminal — or headless,
with [`evo-swarm serve`](#serving-the-swarm) — and a pool of
**lanes**: worker agents, each a separate `evo-agent serve` process with its own
context, working in parallel. You talk only to the coordinator and give it
goals; you do not need to tell it to use lanes. It decides for itself how to
split the work into lane-sized pieces with clear done criteria, delegates them,
integrates and verifies what comes back, and reports to you.

```sh
evo-swarm                 # 6 lanes, coordinator TUI here
evo-swarm --workers 3
evo-swarm --resume        # the last swarm here: coordinator and lanes
evo-swarm serve --token-file ~/.evo/serve.token   # headless, over HTTP
```

It is a separate program on top of evo: its own system (`evo-swarm.asd`,
`swarm/`), its own binary (`build/evo-swarm`, installed beside `evo-agent` —
the installed `evo` is a soft link to this binary). The agent binary contains
none of it — `make test` checks that the `evo` system loads without the swarm
— and lanes are the `evo-agent` binary itself, driven only through its public
HTTP API ([docs/serve.md](serve.md)).

## Lanes are lanes, not roles

A lane is like a CPU core: interchangeable capacity, not a specialist. Each is
an `evo-agent serve` on loopback, started with the swarm and idle until given
work; the coordinator decides what each one does, task by task. It launches one
with no supervisor of its own, a port it picks and announces, and stdin a pipe
the coordinator holds:

```text
evo-agent serve --no-userspace --no-supervisor --as-lane --port 0
                --ready-file <lane>/ready.json --watch-stdin
                [--prompt-note <path> …]
```

A lane the coordinator brings back adds `--resume <its exact session>` — never a
bare `--resume` (CONTRACT §8), so a lane continues the session it was on and not
whatever is newest in its directory.

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
- **Restarted by the coordinator.** A lane runs `--no-supervisor --as-lane`:
  the coordinator is what supervises it, because only the coordinator knows what
  the lane was doing. When a lane's process dies, the coordinator starts a new
  one `--resume`ing that lane's exact session, re-initializes it (below) and is
  told: `[lane N] crashed and was restarted …`. A lane whose coordinator dies
  shuts itself down (stdin is a pipe the coordinator holds), so lanes never
  outlive the swarm that drives them. `--as-lane` is also what a lane *is*: its
  journal says `program "lane"`.
- **Stopped with the swarm.** Quitting the coordinator shuts every lane down
  cleanly (`server.shutdown`), killing any that do not stop in time.

## The coordinator

The coordinator is an ordinary evo TUI session with every tool, plus:

| Tool | What it does |
|---|---|
| `lanes` | Every lane: idle/working, step clock, current task, worktree, reports. |
| `delegate` | Give a lane a task — the complete instructions, since the lane cannot see this conversation — and optionally an `objective`, which becomes the lane's goal. Picks the first idle lane unless one is named. Returns at once. |
| `steer_lane` | Guidance for a working lane, seen at its next turn boundary. |
| `interrupt_lane` | Stop what a lane is doing. With a `text`, then give it that text as new instructions, in the same step. |
| `lane_command` | Any slash command in a lane (`/goal`, `/model`, `/compact`, `/lore`, …), exactly as typed in its TUI. |
| `lane_eval` | Evaluate Lisp in a lane — to install a tool, an MCP server, a capability it asked for. Kept and replayed if the lane restarts (unless `keep` is false). |
| `lane_transcript` | The last messages of a lane's context. |
| `lane_reports` | What a lane has reported, read from its journal. |
| `restart_lane` | `reinit` (evaluate its baseline and replayed evals again, in place), `restart` (new process, same session), `fresh` (new session). |
| `lane_worktree` | `create` a git worktree + branch for an idle lane, or `remove` it (the branch is kept to merge). |

Every tool reaches a lane through serve's HTTP API and nothing else.

### What reaches the coordinator

The coordinator holds one connection to each lane — that lane's op stream — and
each lane is mirrored back as a topic of the coordinator's own server
([below](#serving-the-swarm)). What the coordinator must *hear* becomes its
**input**, exactly like a message you type: queued to its next turn boundary
when it is working, and starting a run — waking it — when it is idle. You see
each one in the scrollback too.

The message carries an `:origin` (§3) naming what it is: that is what a client
renders instead of prose, and what the model is not shown.

- `[lane N report] done: … evidence: … next: … blocked: … requests: … goal: …` — a
  lane called its `report` tool (`goal:` is the lane's goal status, when it has
  one). Origin `:lane-report`, the same fields as data — the `lane_report` item
  of the `session` topic under serve;
- `[lane N] run ended (stop|error|aborted) — goal: …` — a lane went idle.
  `goal: complete` means it is done; `goal: active, but the lane is idle until
  steered` means it errored or was stopped short of its goal — a lane's active
  goal keeps it working, so it never settles with one otherwise. Origin
  `:lane-event`, which carries the task the run was on: the text stays short on
  purpose, so a lane's whole prompt does not land in the coordinator's context
  every time a run ends;
- `[lane N] error: …` — a lane's task failed;
- `[lane N] crashed and was restarted …` / `[lane N] is down …` — origin
  `:lane-event` too.

So the coordinator never polls: it delegates, ends its turn, and is woken.
A message from you still comes first: the coordinator's note tells it to
answer you before it turns to lane messages that arrived meanwhile.

Under serve those messages are items of the coordinator's `session` topic
(`lane_report`, `lane_event`) — which is where a client reads them, so a report
is one item and not also a notice saying the same thing twice.

### A lane closes its own goal

A lane given an `objective` works on it as a goal: whenever it goes idle with
the goal still active, it is sent back to work. The `report` tool takes an
optional `goal` — `"active"` (the default) or `"complete"`. The report that
delivers the objective passes `"complete"`, and the goal is closed in that
same run, through the path `update_goal status="complete"` takes — so the lane
settles once, instead of being sent back to re-verify and close a goal whose
work it has already delivered. It is a claim, as `update_goal` is: the
coordinator checks the report's evidence, and delegates a fresh goal if the
claim is wrong. Completing an already complete goal is a no-op; a goal that
cannot be completed (the user paused it) stays as it is, and the report is
delivered all the same.

## Watching the lanes

Input always goes to the coordinator; lanes are only watched.

- The status line shows one glyph per lane — `●` working, `◐` compacting,
  `○` idle, `◌` starting, `✗` down — and how many are busy.
- `/lanes` lists every lane: state, step clock (time in its current step),
  current task, worktree, reports, pid.
- `/lane N` follows lane N's transcript live in the scrollback (its text and
  tool calls, prefixed `[lane N]`); `/lane off` stops.

## Serving the swarm

`evo-swarm serve` runs the same swarm headless: the coordinator with no
terminal, driven over HTTP through [serve's protocol](serve.md) — the same
`/health`, `/snapshot`, `/stream`, `/ops` and `/items`, and a `/catalog` whose
`lanes` half is this program's. It takes the agent's serve flags
(`--host`, `--port`, `--ready-file`, `--watch-stdin`, `--allow-remote`,
`--prompt-note`) and evo-swarm's (`--workers`, `--evo`, `--resume`, …). The
swarm code is the same code either way; only the frontend differs.

`--prompt-note` is the one that crosses into the lanes: the coordinator
registers the files as its own prompt notes, and every lane is launched with
the same flags on its command line — a client that renders the agent's output
itself is reading the lanes' transcripts too. A lane that crashes and is
restarted by the coordinator gets them again, since every launch is built from
the swarm's own arguments ([serve.md](serve.md#starting-it) is the flag's own
documentation).

`GET /health` says which server it is — its `program` and its `version`,
beside the epoch, pid and session-loop age every server reports:

```json
{"ok": true, "program": "evo-swarm", "version": "0.1.0", "epoch": "8bdc7c0b",
 "pid": 51234, "restarts": 0, "session-loop-age-ms": 3}
```

so a client can tell it from the agent's serve (`program` `evo-agent`) before
it asks for anything. Those fields are the [identity
seam](serve.md#the-seams-a-program-adds); `GET /catalog` carries the
[lanes half](serve.md#the-catalog) of that seam too, so a client learns which
models a lane can run with:

```json
{"lanes": {"models": [{"id", "provider", "ok", "reason"}]}}
```

### The topics a swarm adds

The swarm adds no routes: everything a client needs is a topic, read the same
way as the session (CONTRACT §4.3, §7). `GET
/snapshot?topics=swarm,lane:*` is the whole swarm at one `seq`, and
`GET /stream?topics=swarm,lane:*` keeps it current — the lanes as items and
states, exactly as the agent's `session` topic is.

- **`swarm`** — the swarm's own state, and no items: `id`, `workers`,
  `status` (`busy` — how many lanes are working — and `waiting_on_lanes`, true
  while the coordinator has settled with lanes still working, which is the
  `waiting` hold of §4.2), `config` (the lane model and thinking level), and
  `lanes[]`, one row per lane: `n`, `state` (`starting`, `idle`, `working`,
  `compacting`, `down`, `stopped`), task and clocks, `restarts`, `pid`,
  `worktree` and `branch`, the lane's `model`, `context`, `goal`, `todos`, how
  many `reports` it has made, and its `last_item`. Enough to draw the panels.
- **`lane:N`** — one per lane: that lane's own topic, mirrored. Its items are
  the lane's transcript (the same items its own server publishes, oldest to
  newest) and its state is the lane's own (`status`, `model`, `context`,
  `goal`, `todos`, `session`), so a client can follow a lane's work, not just
  its summary. A lane that restarts is a new process: its topic is reset
  (`topic.reset`, reason `lane_restarted`) and re-snapshotted.

Nothing in the swarm's topics is a control channel: lane control stays the
coordinator's, and a lane's token and URL are never handed out. A client reads
lanes; it does not talk to them.

### The one human action on lanes

`run.interrupt` is serve's op, and a swarm answers its two extra scopes
(CONTRACT §5.5, §6):

```json
POST /ops {"op": "run.interrupt", "args": {"scope": "swarm"}}
→ {"ok": true, "result": {"interrupted": ["session", "lane:1", "lane:2"]}}
```

- `scope: "lane"` with a `lane` number stops that lane's run and nothing else;
- `scope: "swarm"` stops the coordinator's run and every lane's.

Only what was actually running is named in `interrupted` — an idle lane is not
something a client can show as stopped. A lane that was running is stopped
through its own `run.interrupt`, so the lane's own server is the one that ends
the run. The coordinator is told a person did it: an after-run message with a
`:human-action` origin, shown in its queue at once and delivered when a run
next drains the queue — the human's stop is not a reason to spend a
coordinator turn, but it is not invisible either.

`server.shutdown` stops every lane, and `--resume` restores the swarm —
coordinator and lanes — from the coordinator's journal, as it does for the TUI.

## swarm.lisp

evo-swarm boots the coordinator as evo boots any session — `init.lisp`,
extensions, `post-init.lisp` — then reads `~/.evo/swarm.lisp` and
`<project>/.evo/swarm.lisp`, as the last step of the same pass (so `/reload`
re-reads them too). Plain `evo-agent` never reads swarm.lisp. It is where the swarm
is configured:

```lisp
;; ~/.evo/swarm.lisp
;;; The coordinator: any init.lisp call, for the swarm only
(evo:set-setting :model "claude-opus-5")

;;; The swarm
(evo:set-setting :swarm-workers 4)

;;; Code every lane evaluates
(evo.swarm:in-lanes (lane lanes)
  (load "~/.evo/extensions/020-claude-oauth-provider.lisp")
  (when (<= lane 2)
    (evo:set-setting :model "claude-sonnet-5")))
```

A commented sample is [docs/examples/swarm.lisp](examples/swarm.lisp).

| Call | |
|---|---|
| any init.lisp call (`register-model`, `set-setting :model`, …) | The coordinator's, running after your init files, so it overrides them for the swarm. The lanes inherit it (below). |
| `(evo:set-setting :swarm-workers n)` | The default lane count (`--workers` wins). |
| `(evo.swarm:in-lanes (lane lanes) forms…)` | Code every lane evaluates (below). |
| `(evo.swarm:set-lane-tools names)` | Limit every lane to `names`; with `:lanes '(5 6)`, only those. `report` is always kept. |
| `(evo.swarm:set-worker-note text)` | The lanes' prompt note (below). |
| `(evo.swarm:set-coordinator-tools names)` | Limit the coordinator's tools. |
| `(evo.swarm:set-coordinator-note text)` | The coordinator's prompt note (below). |

`evo-swarm --no-userspace` reads none of these files.

### in-lanes: code for the lanes

A lane starts with `--no-userspace`: it reads none of your init files and
loads none of your extensions. What it has, the coordinator gives it — the
coordinator's providers, models and defaults, automatically — plus whatever
`in-lanes` says. Its body is init.lisp-style code, **evaluated in each lane,
not in the coordinator**: before the lane gets any work, and again whenever it
restarts.

- `(in-lanes (lane lanes) …)` binds `lane` to the lane's number
  (1..`lanes`) and `lanes` to the lane count around each form. Name only what
  you use: `(in-lanes (n) …)`, or `(in-lanes () …)` for neither.
- The forms run in the lane's `EVO.USER` one at a time, in order, each read
  after the one before it ran — as `load` would — with `*load-truename*` the
  swarm.lisp they came from, so `(load (merge-pathnames "x.lisp"
  *load-truename*))` loads a file beside it.
- Every call adds to what lanes run: the global swarm.lisp's first, then the
  project's.
- The body is **read** by the coordinator when it loads swarm.lisp, so a symbol
  like `pkg:foo` needs a package the coordinator has too. It does for your
  extensions, which the coordinator loads; for a package only lanes load,
  write `(uiop:symbol-call :pkg :foo)`.

### What a lane is given, in order

The coordinator evaluates this in the lane (`POST /eval`, a form at a time):

1. the coordinator's **providers** (keys by variable name only, below), and
   its **model** and **thinking level** as the lane's defaults — the swarm's
   `--lane-model` / `--lane-thinking` when the launch named them, else the
   coordinator's;
2. the `in-lanes` forms, which override anything from step 1;
3. `--lane-model` and `--lane-thinking` again, when the launch named them: a
   flag beats config everywhere else in evo, so it beats `in-lanes` too. Only
   what the flags named is repeated, so a lane that inherits the coordinator's
   model or level is still `in-lanes`'s to override; a lane configuration
   restored from a resumed swarm counts as named — a flag wrote it there in
   the first place;
4. the coordinator's **models** the `in-lanes` forms did not register — those
   whose API the lane has. They come after the forms because a model's API
   may be defined by an extension that `in-lanes` loads: Claude OAuth's
   `:anthropic-oauth-messages`, for one. A model whose API the lane lacks is
   skipped, so an extension you have installed but do not use in the swarm
   costs the lanes nothing. Then the lane checks that its **default model**
   is registered; if the default is one it skipped, initialization fails
   with a message naming the missing API and saying to load its extension
   with `in-lanes`;
5. the swarm's own, last, so `in-lanes` cannot lose them: the **`report`
   tool**, the lane's **prompt note**, its **tool limit**;
6. a run for any goal continuation a resumed lane had queued.

**No secret travels as data.** A provider whose key comes from an environment
variable is registered in the lane with that variable's *name*. One registered
with a literal `:api-key` is registered in the lane with a swarm-private name
(`EVO_SWARM_<PROVIDER>_API_KEY`), and the key itself is set only in the lane
process's environment. Nothing the swarm writes into a form, prompt, journal
or log — the coordinator's or a lane's — holds a key; the end-to-end test
greps all of them. `in-lanes` forms are your own code and run in the lane as
written, so give them keys with `:api-key-env`, never a literal `:api-key`.

A changed swarm.lisp reaches the coordinator on `/reload`, and a lane the next
time it is initialized: when it restarts, or on `restart_lane` with `reinit`.

At runtime the coordinator can add more to a lane with `lane_eval`, when a
lane asks for a capability in a report and the coordinator agrees. What it
evaluates is recorded and replayed if that lane restarts.

## Behavior lives in prompt notes

The coordinator's role and a lane's are prompt notes. The coordinator decides
for itself to split and delegate: you give it goals, not lane assignments, and
it puts lanes on anything bigger than a quick answer or small edit, keeps them
busy, handles stuck lanes, then integrates, verifies and reports. A lane does
its task in scope, reports after each meaningful piece of work, never goes
quiet, and asks for what it lacks. The notes are `swarm-coordinator` and
`swarm-worker`; `set-coordinator-note` and `set-worker-note` replace them:
each is a FORMAT control — the coordinator's takes the lane count; a lane's takes its
number, its number again, the lane count, and a sentence about where it works
(the shared directory, or its worktree and branch).

## Resuming

The coordinator's journal records the swarm as `:custom` state: its id and
directory, and for each lane its worktree, branch, task and the code evaluated
into it. `evo-swarm --resume` reopens the coordinator's session and brings
every lane back from that record — each resuming its own session, in its own
worktree, re-initialized and with its evaluations replayed. A coordinator that
crashes is restarted by evo-swarm's supervisor the same way. `/resume` inside a
running swarm does the same: when the chosen session records another swarm,
the running lanes stop (their sessions stay on disk, recorded by the session
just left) and the recorded lanes come back. `/new`, `/fork` and a session with
no swarm keep the running lanes.

## Command line

```text
evo-swarm [--workers N] [--resume [path]] [--model id] [--thinking level]
          [--lane-model id[@provider]] [--lane-thinking level]
          [--evo path] [--no-userspace] [--no-supervisor]

evo-swarm serve [--host addr] [--port n] [--ready-file path] [--allow-remote]
                [--watch-stdin] [--prompt-note path …]
                [--workers N] [--resume [path]] [--model id] [--thinking level]
                [--lane-model id[@provider]] [--lane-thinking level]
                [--evo path] [--no-userspace] [--no-supervisor]

evo-swarm catalog --json       what a swarm launched from here could use
evo-swarm check --json         whether that launch would work; exit 1 if not
```

`--evo` names the evo-agent binary lanes run; without it, `EVO_BINARY`, then
the one beside `evo-swarm`, then `evo-agent` on `PATH`. The TUI form needs a
terminal; `evo-swarm serve` does not — see
[Serving the swarm](#serving-the-swarm). For a headless single agent,
`evo-agent serve` is still the smaller answer.

### The two offline questions

`catalog` and `check` answer a client — a GUI's New Swarm page — before it
starts anything, and neither starts a lane, a server or a model call:

- **catalog** is the coordinator's catalog (models, providers, thinking
  levels, ops, tools, …) plus a `lanes` half: every model as a *lane* would
  see it — `ok` or with the reason it could not — and, on every model in
  either half, the levels that model accepts (`effort_levels`). Its
  `default_thinking`, like `default_model` beside it, is the effort a launch
  from here would start on — `check`'s first answer, in the document a client
  reads whether or not it goes on to run `check`.
- **check** says whether a launch from here would work — do the models
  resolve, is each one's API where it runs, are the credentials there — and
  reports what the launch resolves on its own: the coordinator's effort, the
  lanes', and the lane count, so a chooser opens on the truth instead of on
  medium and six. It exits 1 when it found a problem, and each problem
  carries a machine code (`no_api_key`, `lane_api_missing`, …) beside its
  sentence.

Both work out the lanes' half by *evaluating* `swarm.lisp`'s `in-lanes` forms
the way a lane does — in a sandbox of the coordinator's process, with a fresh
lane's registries (the kernel's APIs and the providers they seed, no models,
no settings) and the variables a launch puts in a lane's environment. So a
model an `in-lanes` form set, and an API an extension it loaded defines, are
reported as that lane would see them, and `--lane-model`/`--lane-thinking` are
reported over `in-lanes`. Nothing of the evaluation is left behind — the
coordinator's registries and settings go back exactly as they were — and a
form that fails becomes a problem in the answer rather than a crash.

Files: `~/.evo/swarm/<swarm-id>/` holds each lane's `lane-N/` directory —
`sessions/`, `ready.json` (0600: the lane's `port`, `url`, `token`, `epoch`,
`pid` and `session`, rewritten by every life of the process), `lane.log` — and
`worktrees/lane-N/`.

## Design notes

- **Why processes, not threads.** A lane is a whole evo — its own journal,
  context, extensions, crash domain — so a lane that wedges or crashes cannot
  take the coordinator or another lane with it, and everything evo already
  guarantees (resume, the journal as truth) holds per lane for free. The price
  is a process per lane, which is cheap next to the model — and the supervisor
  is the coordinator, which is the one thing that knows what the lane was doing.
- **Why only HTTP.** The coordinator uses the same API a person or another
  program would. Nothing about a lane is private to the swarm, so a lane can
  be inspected with curl (its ready file names its `url` and `token`), and
  serve's API is proven by a second real client.
- **Why in-lanes binds its variables.** Code for a lane runs in the lane, so
  `in-lanes` is honest about it: a macro that records its body for the lanes,
  not a function that pretends to run here. The lane's number and the lane
  count are bound in its syntax, as `dolist` binds its variable, so the
  interface shows what the body can read.
- **Tested end to end.** `make swarm-test` (`tests/swarm-e2e.py`, also in CI)
  drives the coordinator's TUI through a pseudo-terminal, against the stub
  Messages endpoint scripting coordinator and lanes: lanes start and
  authenticate, each runs both swarm.lisp files' `in-lanes` forms and gets its
  baseline with no secret in any journal, delegation, a report waking the idle
  coordinator, interrupt and re-steer, a bare interrupt, a goal closed by the
  report that delivers it, an eval reaching one lane only, a worktree lane
  writing in its worktree, a killed lane restarting with the coordinator told,
  quit stopping every lane, and `--resume` restoring it all.
  `make swarm-serve-test` (`tests/swarm-serve-e2e.py`) drives `evo-swarm serve`
  over HTTP only, through one `/stream` subscription: the coordinator answering
  serve's protocol, the `swarm` and `lane:N` topics with one row and one item
  stream per lane, a delegated task streaming on `lane:1` while the swarm topic
  says `busy` and then `waiting_on_lanes`, a lane whose process is killed
  restarting with its topic reset and the coordinator told as a `lane_event`
  item, `run.interrupt` with scope `swarm` stopping the coordinator and every
  lane and leaving a `human_action` item, a lane's report arriving once as a
  `lane_report` item, no lane token or URL over HTTP, `server.shutdown`
  stopping every lane, and `--resume` restoring the swarm.
