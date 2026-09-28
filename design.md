# evo-agent — architecture

`evo-agent` is an agent that self-evolves: a goal-oriented software agent
which, when it lacks a capability, writes the capability into its own running
image and continues.

This document describes the system as built — its invariants, its layers, and
the rules that govern how it changes. It is the reference for anyone, human or
agent, modifying evo. The decision record behind the architecture is Appendix
A; the systems evo learned from are Appendix B.

---

## 1. What evo is

Five properties define the system. Everything downstream is in service of
them.

- **Goal-oriented.** The user states what *done* means; the agent decides how.
  Long-running, unattended pursuit is the normal case, not an edge case.
- **Self-extending.** A missing tool is not a blocker. The agent writes one,
  loads it into its own runtime, and keeps going. Common Lisp makes the
  load-and-redefine half nearly free; the engineering is in the safety rails
  and the seed corpus (§13).
- **Permissive.** There are no permission prompts. The trust boundary is the
  OS or container evo runs in, plus a kernel/userspace split that lets the
  agent break itself deliberately but not accidentally.
- **Self-healing.** Crash, restart, resume the session, continue the goal. The
  supervisor and the journal together make process death a recoverable event
  rather than a lost run.
- **Minimal.** Core functionality of a real agent and nothing ceremonial. The
  omit-list — no permission popups, no sub-agents *inside* an agent (parallel
  agents are separate processes: evo-swarm, §18), no MCP *in the kernel* (it
  is a userspace extension) — is deliberate and each omission has a stated
  re-entry condition (§17).

## 2. Invariants

These hold across every subsystem. A change that breaks one is a change to the
architecture, not to a component, and belongs in Appendix A before it belongs
in code.

1. **The journal is the only source of truth.** Session state lives in an
   append-only file. No Lisp image carries session state; images are build
   artifacts (D2).
2. **State is a fold, never a mutable field.** Context, model, thinking level,
   active tools, goal, todo list — all are derived by folding the
   root→leaf path. Nothing is edited in place, so branching, rewind, resume,
   and pause fall out of the data structure rather than being features (D1).
3. **One extension API, no private doors.** The kernel owns the turn loop and
   nothing else. The TUI, todo lists, and memory are extensions built on
   the same public API an agent-written tool uses. Anything a bundled
   extension needs and cannot get through that API is an API gap to close,
   never a private hook to add (D13).
4. **Errors are data at the provider boundary, conditions at the tool
   boundary.** The provider layer never signals into the loop; failures become
   assistant messages carrying `:stop-reason :error`. Tools signal freely and
   the loop converts the condition into an error tool-result. Either way the
   transcript stays well-formed.
5. **The context is rebuilt, never patched.** Between turns the whole snapshot
   — system prompt, tools, model, messages — is replaced wholesale from the
   journal. Nothing mutates mid-request.
6. **Recovery is editing a source file.** Runtime evolution replays from
   `:load` entries against source on disk. A runtime the agent broke is
   repaired by fixing or removing a file, never by surgery on opaque state.
7. **Journals are data, not code.** They are sexprs, read with `*read-eval*`
   nil into a dedicated package, over a restricted value vocabulary.

## 3. Architecture

```
evo (one binary; evo-swarm is a second program on top of it — §18)
│
├─ SUPERVISOR — the same binary, invoked plainly (D17)
│    re-spawns itself as the session child (EVO_SUPERVISED_CHILD=1,
│    inherited stdio so the TTY passes straight through), monitors
│    process exit and a heartbeat file, restarts with --resume,
│    quarantines a userspace that fails to boot
│
└─ SESSION CHILD (SBCL or ECL image)
   ├─ KERNEL  (locked packages: EVO.KERNEL, EVO.PROVIDER, EVO.JOURNAL, …)
   │    turn loop            errors-as-data, steering queues, save points
   │    journal              append-only sexpr entry tree + leaf pointer
   │    provider APIs        CLOS protocol; anthropic-messages bundled;
   │                         model and provider registries fed from
   │                         init.lisp
   │    tool registry        register / activate / refresh, prompt rebuild
   │    extension API        register-tool, register-command, event hooks —
   │                         both extension tiers build on this
   │    goal driver          idle-continuation loop, budgets, audits
   │    compactor            usage-anchored, self-contained checkpoints
   │    budget guard         per-goal token budget, a hard stop
   │    extension loader     compile, load, journal the load
   │    media                images in: clipboard readers, sniffing,
   │                          size cap + downscale, :image blocks
   ├─ CORE EXTENSIONS  (bundled, hand-written, compiled into the image;
   │                    same API and privileges as user extensions)
   │    todo       checklist tool + :custom state, rendered by the tui
   │    command    the command layer: what each slash command does, for
   │               every frontend (host protocol)
   ├─ FRONTENDS  (system "evo", on top of "evo/core"; one per session)
   │    tui        adaptive renderer, multi-line editor (image attachments
   │               as editable tokens)
   │    serve      headless: the session controlled over HTTP (§16.2);
   │               a program runs its own session on it (§18)
   │    print/events  -p and --events, in the CLI
   ├─ USERSPACE  (unlocked: EVO.USER)
   │    agent-written tools and code — source files plus journal :load
   │    entries; rebuilt from source on every boot
   └─ RESOURCES
        skills/ (progressive disclosure)   prompts/ (templates)
        lore store (out-of-band, injected every turn)
        docs corpus (paths named in the system prompt)
```

Directory conventions:

- Global `~/.evo/` — `init.lisp`, `post-init.lisp`, `sessions/`,
  `extensions/`, `skills/`, `prompts/`, `lore.sexp`, `docs/`
- Project `<cwd>/.evo/` — `init.lisp`, `post-init.lisp`, `extensions/`,
  `skills/`, `prompts/`, `lore.sexp`

Project scope shadows global scope wherever both exist.

## 4. The journal

### 4.1 Model

- One file per session: `~/.evo/sessions/<encoded-cwd>/<timestamp>_<uuid>.sexp`.
- Line 1 is a header form; every later line is one entry with `:id` (short
  random), `:parent-id` (nil at a root), and `:timestamp`.
- The file is a **tree**. Branching moves the leaf pointer; the next append
  becomes a sibling. Entries are never modified or deleted.
- Everything is a fold over root→leaf. There are no mutable fields.
- Writes are ahead of actions: the entry is appended before it is acted on.
  Nothing is written until the first assistant message exists, so abandoned
  sessions leave no litter.

### 4.2 Entry types

```
:message              payload is a message plist (in LLM context)
:model-change         provider + model id           (state fold)
:thinking-change      thinking level                (state fold)
:tools-change         active tool names             (state fold)
:compaction           summary + retained tail — self-contained checkpoint
:branch-summary       summary of an abandoned branch
:custom               extension/tool state, INVISIBLE to the LLM
:custom-message       extension-injected content, visible to the LLM
:label                bookmark on an entry (target id + label)
:session-info         session name etc.
:goal                 goal created/updated: objective, status, budget, usage
:load                 userspace source file loaded (path + reason)
:provider-retry       an attempt was re-sent: attempt/max, delay, reason
```

Three of these carry architectural weight:

- The `:custom` / `:custom-message` split separates *state* from *context*. It
  is how a tool persists state without polluting the prompt.
- `:compaction` materializes the retained tail on the entry, so rebuilding
  context is `[summary, ...retained-tail, ...entries-after]` — O(1), with no
  walk past the compaction.
- `:load` is what makes runtime evolution replayable. Boot is: load kernel
  and core extensions, build userspace (init files, extension directories,
  post-init files), then replay the path's `:load` entries against the source
  files on disk.

### 4.3 Format rules

- One form per line, printed `*print-readably*`-compatible.
- Read with `*read-eval*` **nil**, standard readtable, into a dedicated
  package.
- The value vocabulary is a deliberate "sexpr-JSON" subset: plists, keywords,
  strings, integers, ratios and floats, `t`/`nil`, vectors. No non-keyword
  symbols, no arbitrary objects, no cycles. Round-tripping stays trivial and
  journals stay hand-editable.

```lisp
(:type :session :version 1 :id "0197f2..." :cwd "/home/u/proj" :timestamp "2026-07-25T09:00:00Z")
(:type :message :id "a1b2c3d4" :parent-id nil :timestamp "..."
 :message (:role :user :content ((:type :text :text "fix the build"))))
(:type :goal :id "e5f6a7b8" :parent-id "a1b2c3d4" :timestamp "..."
 :goal-id "g-01" :objective "make ./test.sh pass" :status :active
 :token-budget 500000 :tokens-used 0)
(:type :message :id "c9d0e1f2" :parent-id "e5f6a7b8" :timestamp "..."
 :message (:role :assistant :api :anthropic-messages :provider :anthropic
           :model "claude-fable-5" :stop-reason :tool-use :usage (...)
           :content ((:type :tool-call :id "tc_1" :name "bash"
                      :arguments (:command "./test.sh")))))
```

### 4.4 Session operations

- `/resume` — list sessions via a bounded header scan, reopen, rebuild context
  from the leaf. Pausing is free: stop the process, because the journal *is*
  the state.
- `/tree` — navigate entries and move the leaf. Selecting a user message moves
  the leaf to its parent and puts the text back in the editor, so
  edit-and-resubmit creates a branch. Abandoned branches can carry a summary.
- `/fork` — copy the root→entry path into a new file.
- Double-escape is one-keystroke rewind.
- File-state undo is deliberately **not** built in. The canonical answer is a
  small extension keying `git stash create` by entry id — a worked example in
  the seed corpus rather than a kernel feature.

## 5. Provider layer

One unified message model, one bundled adapter, one extension point.

**Provider APIs are a protocol; models are configuration.** A wire protocol is
a CLOS class implementing `endpoint-path`, `auth-headers`, `build-request`,
`parse-stream`, and `perform-request`, dispatched from
`call-provider` on the model's `:api` tag. The bundled protocol lives in
`src/provider/`; an extension registers its own through the same public `EVO`
surface, specializing the same generics. The three self-seeding generics
(`default-provider-key`, `default-base-url`, `default-api-key-env`) default to
`nil`, so an API that implements only the wire protocol is complete: it seeds
no provider and takes its endpoint from init.lisp like any other.

Models and endpoints come from init.lisp through ordered registries:
`register-model` (re-registration replaces in place) and `register-provider`
(field-wise merge, with stock endpoints pre-seeded from each API's defaults,
so an environment API key alone suffices). There is no built-in model table. A
missing or unknown model is a loud configuration error at CLI preflight — a
usage error, exit 64, which never enters the supervisor's restart loop.

**The adapter contract** is written down in `provider/api.lisp`'s header.
`parse-stream` returns `(:content :model :stopped-p :error-message
:stop-reason :usage [:aborted-p])`. Stop reasons normalize to `:stop`,
`:length`, `:tool-use`, `:error`, `:aborted`. Usage buckets are `:input`,
`:output`, `:cache-read`, `:cache-write`, with `:input` excluding cached and
cache-written tokens. Stream events are `:message-start`, `:text-delta`,
`:thinking-delta`; `:tool-call-start` belongs to the kernel instead, fired
from `run-tool-call` with fully-parsed `:arguments`, which do not exist yet
when a stream block opens. Runtime errors are data; configuration-resolution
errors signal, and preflight catches them.

- **Message model**: four content blocks (`:text`, `:thinking`, `:image`,
  `:tool-call`) and three roles (`:user`, `:assistant`, `:tool-result` as a
  top-level role, so history stays a flat list). Assistant messages
  self-identify with an `:api`/`:provider`/`:model` triple. Usage is tracked
  per message.
- **Images travel by value**: an `:image` block carries `:media-type` (sniffed
  from magic bytes, never from the file name) and base64 `:data`, so it is
  journal data like everything else and a session replays with no side files
  to lose. The adapter encodes it natively (Anthropic `image`/base64 source);
  the handoff pass degrades it to a named text placeholder for a model
  registered `:vision nil`, so a model switch cannot
  poison a transcript that contains one. `evo.media` owns the read path —
  clipboard readers per platform, size cap, downscaling — and nothing above it
  knows where the bytes came from.
- **Only the last few images are re-sent** (`*max-request-images*`, with
  `*max-request-image-data-chars*` as the byte backstop). By value means *by
  value every turn*: a session that keeps screenshotting otherwise re-uploads
  its whole album on every request, forever — one measured session sent 8.9 MB
  of base64 in each of 305 requests, 1.2 GB in under an hour, and a link
  hiccup during any one of those uploads is indistinguishable from a hang.
  Older images become named placeholders that say how to get the picture back:
  read the file again. The walk is newest-first, so the evicted image is always
  the oldest and the placeholder swap moves *forward* through the transcript —
  a new screenshot invalidates the cached prefix from the image it evicts, not
  from further back. Request copy only; the journal keeps the pixels.
- **The agent looks at images too, not just the user**: an `:image` block is a
  legal tool result, so `read` on a png/jpeg/gif/webp returns the picture
  instead of line noise, and the agent can open a screenshot on its own
  initiative. The image stays *inside the result of the call that produced
  it* — Anthropic `tool_result` content has a slot for it.
  Injecting it as a separate user message instead would put words in the
  user's mouth, break call/result adjacency, and move the cached prefix; a
  chat-completions extension has to do exactly that, and only because a
  `tool` message there has nowhere else to carry one. A model registered
  `:vision nil` has the call refused outright — a result that merely explains
  still reads like success, and a megabyte of base64 it can never see costs
  the turn — and the system prompt states per session which case the agent is
  in (`Can see images: yes|no`), because a model that assumes it is blind
  never tries.
- **Provider artifacts are a typed variant, not stringly-typed**: an Anthropic
  thinking signature is a base64 scalar accumulated from chunked
  `signature_delta`, replayed verbatim with its block.
- **Stateless replay** everywhere: full history per request, no server-side
  conversation state.
- **Handoff pass** at request build: same-model thinking replays verbatim;
  cross-model thinking degrades to plain text or is dropped; orphaned tool
  calls receive synthetic error results; errored and aborted assistant turns
  are elided.
- **Streaming** is hand-rolled SSE over one shared framing loop
  (`map-sse-events`: event and data accumulation, CR trim, abort flag); APIs
  supply only per-event dispatchers. Terminal-event guards make a stream that
  ends without `message_stop` or a terminal response event an error, and a
  retryable one. Tool arguments are parsed tolerantly from partial JSON on
  every delta. `perform-request` has a default SSE-over-dexador method that a
  non-SSE framing can override.
- **Retry** has three layers: in-request HTTP retry honoring `retry-after`
  with exponential backoff and jitter, refusing silently-long server delays;
  error normalization; and turn-level retry on finished error messages.
  Classification is on HTTP status plus typed error codes, not regexes over
  message strings. **A retry announces itself**: it emits `:provider-retry`
  (attempt, max, delay, reason) and journals an entry the fold ignores.
  Silently re-sending megabytes behind a spinner that looks exactly like
  progress is how a twelve-attempt worst case becomes "evo hung", and the
  session file kept no trace that it ever happened.
- **Deadlines are measured in progress, not wall clock.** The socket
  `:read-timeout` is `SO_RCVTIMEO` — per read — so it cannot see a request
  that is never answered at all (no send timeout exists either), and a stream
  that only carries keepalives resets it forever. Two watchdog clocks fix
  both, fed by the shared SSE framing loop, which grades every line as
  liveness and every non-`ping` event as content: `*request-stall-timeout*`
  (no bytes at all — covers connect, body upload, prefill, first frame) and
  `*request-idle-timeout*` (bytes but no content — the ping-only zombie). The
  watchdog runs in the caller, because the owner is the thread that is stuck;
  it cancels through the same one-interrupt path as an abort, and the caller
  rewrites its own return value into a retryable error, never the `:aborted`
  that only the user means. No wall-clock deadline: a model that legitimately
  streams for twenty minutes must not be killed for taking twenty minutes.
- **Caching**: `cache_control` breakpoints (system prompt, last tool
  definition, last user message).
  Protecting the cache prefix is a constraint the whole prompt design honors
  (§11).
- **Model registry**: user-registered plists (id, context window, max output,
  effort ladder, thinking mode, vision), registration-ordered for the
  `/model` picker. Token accounting only — no cost table.
- CL stack: `dexador` with `:want-stream t` plus `cl+ssl` and explicit read
  timeouts; `com.inuoe.jzon` on the wire. **A request is owned by its own
  thread**: cancellation interrupts *that* thread with a private condition, so
  the socket is closed by the `unwind-protect` that opened it, and the caller
  joins before returning. Cancelling therefore *stops* the request — the earlier
  design closed the socket from the caller's thread and skipped the join, which
  raced the reader and let a cancelled request keep streaming.

## 6. Concurrency and ownership

Few threads, and every mutable object has exactly one owner. Threads exchange
*messages*; they never reach into each other's state or free each other's
resources. Locks appear only at those handoff points, and there are no atomics
or lock-free tricks anywhere — the model is meant to be checkable by reading.

**Owners.** The TUI loop owns TUI state; under `evo serve` the session
thread owns the task and runs every command, and HTTP connection threads own
only their sockets. One run worker owns agent execution
state. A provider request owns its socket. A tool call owns any child process it
launched. An extension generation owns its hooks, tasks and patches.

**The seams**, each a queue guarded by one lock:

- *TUI → worker*: steering, follow-ups, and the abort control message.
  `request-abort` only posts; the worker latches it at `agent-abort-flag` and
  runs cleanups there, on the thread that owns what is being torn down.
- *worker/poller → TUI*: the event queue. `request-repaint` is the only way
  another thread asks for a frame; `tui-dirty` is set by the TUI thread alone.
- *connection → serve session thread*: the inbox. A request's work is a
  closure the session thread runs in arrival order and answers through a
  promise; the worker's completion arrives the same way. The event log — a
  lock-guarded ring, encoded once at publish — is the one thing connection
  threads read directly.

**One task, not a set of flags.** A run or a manual compaction is a single
task (id, kind, thread) — `tui-task` in the TUI, serve's `task` under
`evo serve`. "Running" and "compacting" are questions asked of
it, not booleans kept in sync, and a completion event names the task it
finishes, so a late `:worker-done` cannot clear a newer task. A task is only
forgotten after its thread is joined — including at shutdown.

**Quiescence before structural change.** Switching journals (`/resume`,
`/new`, `/fork`, `/tree`) requires a session with no task *and* an empty
mailbox. Queued-but-unrun input counts: the model gate deliberately leaves a
submit queued, and carrying it into a different journal would answer it in the
wrong session. Rebuilding userspace (`/reload`) requires only that no task is
running — a queued submit stays welcome there, because a submit gated on broken
model config is *why* the user reloads, and the reload releases it. These
rules live once, in the command layer (`require-session-quiescent`,
`require-idle`), and a command that finds them unmet is *refused*
(`command-refused`): the TUI shows the reason dimmed, serve answers `409`.
Nothing is ever raced — every command runs on the one thread that owns the
task.

**Runtime generations.** `boot-userspace` builds a generation and installs its
registries all-or-nothing: a build that fails anywhere restores the captured
catalog (models, providers, APIs, tools, commands, settings, prompt notes),
because a half-built runtime is worse than a stale one. Disposing the previous
generation's hooks and tracked tasks — not re-running the files — is what makes
a reload idempotent.

**Generations must not overlap**, and that fixes the order: the outgoing
generation is disposed *before* the incoming one loads. A reloaded file reuses
the same package and the same globals, so an old `stop` closure writes the very
variable the new task reads; disposing afterwards let the old poller silently
kill the new one (found by driving four real reloads, and now asserted in
`test-reload-generation-ordering`). The price is that a failed build leaves the
old extensions withdrawn — consistent and repairable, unlike two generations of
one extension running at once.

## 7. Agent loop

A **run** is many **turns**; a turn is one assistant message and its tool
batch. The loop polls steering, calls the provider, executes tools, fires
`turn-end`, takes a save point, and repeats while tool calls or queued
messages remain — then polls follow-ups.

- **The save point** (`prepare-next-turn`) is where the context snapshot is
  rebuilt wholesale from the journal (invariant 5). Compaction, tool-set
  changes, and model switches take effect here and nowhere else.
- **Steering queues** (steer, follow-up, next-turn) are polled at turn
  boundaries only and never preempt a tool batch. Mid-run `/lore` and `/goal`
  ride this.
- **Truncation guard**: on `:stop-reason :length` the tool calls are *not*
  executed — salvaged JSON can validate while still being incomplete. Each
  gets an error result asking the model to re-issue.
- Every event carries a run id and a monotonic turn index. Abort is checked in
  the loop predicate, so no provider call is wasted after an abort. There is
  one transcript, owned by the journal; the loop's context is always a derived
  snapshot.
- **Run-until-settled is kernel code**, not application code: an outer driver
  runs, then asks whether the error is retryable, whether compaction is
  needed, whether messages are queued, whether a goal is active — and
  continues. The goal driver (§9) plugs in here. The drive is announced as
  `:busy` when it starts and `:idle` (with its outcome, errors included) when
  it returns — the one honest "the agent is waiting for the human".
- **What the user said is announced** as `:user-message` when it is drained
  into the journal, on the run's thread at a turn boundary, so context an
  extension journals for it (the IDE bridge's focused file) lands right before
  it. Steering queues mark such a turn; goal continuations and extension
  steering are not user input and are not announced.
- Tool interface: name, description, sexpr schema (emitted as JSON Schema),
  `execute` function, and a result split into `:content` (model-visible) and
  `:details` (host-visible). `:content` is a string in the common case, or
  content blocks when the tool hands back something the model must *see*
  rather than read — `read` on an image is the one such tool today, and the
  text budget (50k chars) applies to the text blocks only. Execution is
  sequential (D9).
- **A foreign contract stays foreign.** A tool proxying a remote one did not
  write its own schema, so the schema may instead be a ready-made JSON Schema
  (passed through untouched), and the tool may ask for `:arguments :json` — the
  model's arguments exactly as written, rather than the keywordized plist. The
  plist spelling (`by_line` ⇄ `:by-line`) is readable and lossy: it upcases
  keys and swaps `_` for `-`, which is invisible for a fixed contract and
  wrong when the keys are *data* (`{"files": {"src/App.jsx": …}}`). The wire
  adapter therefore keeps the model's raw argument JSON on the tool-call block
  and replays it verbatim, so the model is never shown a corrupted copy of its
  own call. `extensions/500-mcp.lisp` is the client this exists for (§17.2).

## 8. Context management

- **Projection pipeline**: journal entries → agent messages →
  `transform-context` → `convert-to-llm` → provider messages. It runs once per
  turn and its output is never written back. In CL this is a generic function
  `entry->llm-messages` with a method per entry type, returning `nil` to
  elide.
- **Compaction**:
  - Triggered by `context-tokens > context-window - reserve` (defaults:
    16k reserve, 20k keep-recent), by overflow-error recovery (compact and
    retry once), or manually via `/compact`.
  - Token accounting is anchored on the last valid provider-reported usage;
    only the tail is estimated (chars/4, images flat ~4800).
  - Cut points are never at a tool result. A single turn exceeding the keep
    budget gets split-turn handling.
  - The summary prompt is structured — Goal, Constraints, Progress, Key
    Decisions, Next Steps, Critical Context, with an instruction to preserve
    exact paths, names, and errors — and a separate iterative UPDATE prompt is
    fed the previous summary.
  - Deterministic facts travel alongside the prose: read and modified file
    sets accumulate across compactions.
  - Summarization calls use a fresh session id and write no cache.
  - The result is a `:compaction` entry carrying its retained tail: a
    self-contained checkpoint.
- Summaries reach the model as ordinary user messages in `<summary>` tags. No
  provider features are involved.

## 9. Goal system (`/goal`)

### 9.1 Model

A goal is journal state; the current goal is a fold over `:goal` entries.

```lisp
(:goal-id "g-01" :objective "..." :status :active
 :token-budget 500000 :tokens-used 123456)
```

Statuses are `:active`, `:paused`, `:budget-limited`, `:complete`. Through
`update_goal` the model may transition to `:complete` (under the audit rules
below), resume a paused goal to `:active`, and **refine** the live goal —
rewrite its `:objective`. There is no `:blocked`: a goal is never given up
on. Pausing is the user's alone (`/goal pause`, `/goal resume`); a model that
needs the user says so in its reply and keeps doing what it can. Budget transitions belong to the system.

### 9.2 Driver

- `/goal <objective>` creates or refines the goal. Refinement appends a new
  `:goal` entry and, if a run is active, injects an "objective updated"
  steering message.
- **Idle continuation**: whenever the agent goes idle with an `:active` goal,
  the driver opens a new turn seeded with a continuation steering prompt
  carrying the objective (as untrusted data), budget numbers,
  anti-scope-shrinking fidelity rules, a **completion audit** (completion must
  be proven from current evidence — files, test output, runtime behavior —
  requirement by requirement, never from memory or intent).
- Doing nothing is not completion. An idle active goal is always re-steered.
  Termination is explicit: the model calls `update_goal` (complete), a budget
  trips, or the user pauses the goal.  A swarm lane may close its goal in the
  report that delivers it (`report` with `goal: "complete"`); that goes
  through the same commit point, `complete-goal`, so it settles once.
- **Pause/resume**: `/goal pause` stops the idle-continuation loop — the
  settled hook only re-steers an `:active` goal, so a paused goal settles and
  waits. It does not auto-resume on session restart either (resumption keys
  on `:active`). `/goal resume` (or the model's `update_goal :active`) picks it
  back up. Headless, a paused goal exits with code 2 (human needed, no
  auto-restart).
- **Budget accounting** runs every turn over tokens. Exhaustion moves the goal
  to `:budget-limited`, and the next steering is a wrap-up template —
  summarize progress, remaining work, next step, start nothing new. This
  doubles as the runaway-cost brake.
- A turn error leaves the goal `:active`. Headless, evo exits 1 and the
  supervisor's `--resume` restart picks the goal back up (§15);
  interactively the error is shown and the next message re-steers.

### 9.3 Model-facing tools

`get_goal`; `create_goal`, which is for explicit user requests only and
refuses while an unfinished goal exists; and `update_goal`, which changes
status (`complete`, or `active` to resume a paused goal) **and** refines the live goal
(`objective` text) — at least one field required, the audit language carried
in the tool description itself.

## 10. Lore system (`/lore`)

Human knowledge, guidance, and constraints, durable across a whole session and
immune to summarization.

- `/lore <text>` appends to an out-of-band store: a sexpr file per scope,
  each entry `(:id ... :text ... :timestamp ...)` on its own line. `/lore`
  writes project scope (`.evo/lore.sexp`), `/global-lore` writes user scope
  (`~/.evo/lore.sexp`); session-scoped entries ride the journal as `:custom`
  state (compaction-immune but disposable — they die with the session).
- Lore is injected into the system-prompt region **every turn**, each entry
  tagged with its `[id]`. It is never entrusted to the compactor's summarizer.
- Mid-run `/lore` rides the steering queue: acknowledged at the next turn
  boundary, durable thereafter.
- The `lore` tool lets the agent **edit or remove** entries by id (and add,
  choosing project/global/session scope). It is stricter than the `memory`
  tools: the agent must not curate lore on its own initiative — only when the
  user has explicitly asked to change their lore.
- Context files (`AGENTS.md`, `CLAUDE.md`) are loaded by walking root→cwd,
  nearest last. Lore complements repository conventions rather than replacing
  them.

## 11. The system prompt

The prompt is assembled fresh on every save point, from source, in a fixed
order: base → tool one-liners → guidelines → own-docs paths → lore → project
context files → skills → environment → language → gitStatus. It is cheap and
pure enough to rebuild that often, which is what keeps invariant 5 honest.

Its content is the agent's operating manual, and it states the things the
architecture cannot enforce: that evo is permissive and pre-authorized to
extend itself, how to reach for `eval` — for a number that has to be right as
much as for `(evo:load-extension ...)` — rather than declaring a capability
missing, how to weigh reversibility when nothing prompts for
permission, how to handle git, and how to treat injected system material.

**Language packs.** The kernel owns the assembly order; the *words* belong to
a registered language pack, and English is one — `src/core-ext/lang-en.lisp`,
a core extension like todo or memory. `evo:register-prompt-language` adds
another; a pack may translate
any subset of `*prompt-sections*` and inherits English for the rest, so adding
a section never leaves a translated prompt with a hole. Which pack is in force
follows the model's precedence — a journalled `/lang` pick, then the
`:language` setting, then the default — and a `:language` value that names no
pack degrades to what it always meant: a response-language hint on the English
prompt. The scope is exactly what the model reads and writes; evo's own
interface is not translated.

**Templating.** Every section evo owns is a template; `{{NAME}}` tokens are
the injection points where runtime facts reach the model, filled from
`prompt-bindings`: working directory, git repo and branch, platform, OS
version, date, model id, docs path, response language, git status. An unbound
token is left standing rather than blanked, so a missing binding is visible in
the prompt instead of vanishing. Only evo's own sections are rendered — lore,
project context files, and skill text pass through verbatim, so a `{{...}}` in
someone's CLAUDE.md is never expanded behind their back.

**Cost discipline.** The branch comes from reading `.git/HEAD` directly rather
than shelling out, and the date carries no clock, because a prompt prefix that
changes every minute would miss the provider cache on every turn. The one part
that does shell out, `gitStatus`, is snapshotted once per process and cached —
and the prompt says so, telling the model the snapshot is stale by
construction. `## Language` and `## gitStatus` are emitted only when they have
something to say.

## 12. Skills, prompt templates, slash commands, modes

- **Skills**: the Agent Skills standard (SKILL.md plus frontmatter) with
  progressive disclosure — only name, description, and path go into the prompt
  inside `<available_skills>`; the model reads the file on demand.
  `/skill:name` forces one. Markdown stays the format here because the
  standard is external.
- **Prompt templates**: `.md` files whose filename is the command, with
  `$1`..`$9` and `$@` substitution. Purely textual expansion.
- **Slash command resolution**: extension commands → builtins → skills →
  templates → send to the agent. Built-ins are `/goal /lore /global-lore
  /compact /tree /rewind /fork /new /resume /model /thinking /lang /reload
  /export`, plus the TUI's presentational `/help /todo /theme /image /quit
  /exit`.
- **One command layer.** What a command *does* is core code
  (`src/command/`, D20), dispatched by every frontend. A frontend is its
  *host*: it answers a small protocol — is a task running, start a run for
  the steering just queued, start a compaction, show this line in this style,
  offer these choices, hand this text back for editing — and decides only how
  things look. The TUI turns a choice into a picker; serve returns it as data
  and takes the choice as the command's argument (`/model <id>`,
  `/resume <n>`, `/tree <id>`). So a command typed in the TUI and the same
  command sent over HTTP cannot differ in effect: there is one copy of it.
- **One mode.** There is no mode switch and no mode indicator: the agent is
  fully permissive, always, and that is the whole design (§1). What a mode
  would have been built from stays public API, so a userspace extension can
  still impose a policy without the kernel knowing: `set-active-tools` gates
  the tool set as journal state (`:tools-change`), `inject-context` adds a
  keyed `:custom-message`, a `:transform-context` hook filters that key back
  out of the projection, and the `:tool-call` hook is the per-call gate.

## 13. Self-extension

This is the evolution engine. Four mechanisms make it work.

1. **Loader.** `evo:load-extension <path>` compiles and loads a file into a
   userspace package and journals a `:load` entry. CL redefinition semantics
   mean a tool can load code from inside its own execution; the new definition
   applies from the next call, so no trampoline or queued reload is needed.
2. **Registration API.** `(evo:register-tool ...)`,
   `(evo:register-command ...)`, and `(evo:on <event> fn :name ...)`. Mutations
   refresh the tool registry and rebuild the system prompt, so a newly
   registered tool is callable on the next request. The `:tool-call` hook may
   mutate arguments or return `(:block t :reason ...)` — the single
   interception point that permission gates, read-only policies, and sandboxing
   all build on.
   Registrations are **owned** by the loading file's generation, so a reload
   withdraws exactly what that file installed. A named hook replaces its
   previous registration; an anonymous one appends, which is how an unnamed
   hook in a reloadable file ends up firing twice. Anything the kernel cannot
   see — a background loop, a function patch — is declared with
   `(evo:spawn-task ...)` and `(evo:on-unload ...)` so it can be stopped,
   joined and undone (§6).
3. **Filesystem convention.** `~/.evo/extensions/` and
   `<project>/.evo/extensions/` load at boot and are writable by the agent.
   Load order is the sorted file name and nothing else, so the name carries a
   fixed-width rank — `NNN-name.lisp`, `000`–`099` foundations, `100`–`899`
   ordinary extensions, `900`–`999` wrappers that must load last. Hooks fire
   in registration order, so one rank orders both loading and dispatch.
   (Core extensions need no rank: their order is declared in `evo.asd`, and a
   second source of truth could only disagree with it.)
4. **Docs as part of the runtime.** The system prompt names absolute paths to
   evo's own documentation and worked examples, enumerated from what is
   actually installed. Beyond that, CL introspection — `describe`, `apropos`,
   `macroexpand` — lets the agent interrogate the runtime it is executing
   inside. The seed corpus is a real deliverable, not documentation debt:
   API docs plus exemplary hand-written extensions. Models write worse CL than
   they write TypeScript; the corpus and the condition system's error feedback
   are the mitigations.

**Safety rails.**

- **Package locks** (D8): kernel packages are locked through
  `evo.port:lock-package`; userspace is unlocked. The agent *can* unlock the
  kernel — the system is permissive, not childproof — but only as an explicit,
  journaled, deliberate act.
- **Reload discipline**: redefinition affects the next call, not frames
  already running. `/reload` additionally requires an idle session and swaps a
  whole runtime generation: the outgoing generation is disposed first (so two
  generations never overlap), and a build that fails restores the previous
  catalog rather than leaving a half-built runtime (§6).
- **State discipline**: extension in-memory state does not survive a restart.
  Extensions rebuild it from `:custom` journal entries on `session-start`.
- **Repair discipline**: because evolution replays from source, a broken
  runtime is fixed by editing a file (invariant 6), and the supervisor's
  quarantine makes the offending file bisectable (§15).

## 14. Core extensions

The kernel owns the core loop and nothing else (D13). Everything outside it —
**including the TUI** — is a *core extension*: bundled, written against the
same API as user extensions, holding the same privileges. Three things
distinguish core extensions from user ones: we ship them, they load first, and
the essential ones cannot be disabled.

The discipline exists for dogfooding. If the TUI, the todo list, and memory
can be built on `register-tool`, `register-command`, event hooks, and
`:custom` entries, then the API is deep enough for the agent's own extensions.
Anything a core extension needs but cannot get through the public API is an
API gap to fix.

Mechanically, core extensions are compiled into the image at build time — they
are part of the ship, not runtime loads — but they register through the same
API. The runtime loader is for user and agent extensions only.

The TUI sits one step further out than the rest (D18). It is a frontend, so it
and the CLI that composes it live in the `evo` system, on top of `evo/core` —
foundations, kernel, and the core extensions that have no interface — and each
defines its own package beside its code. `evo/core` loads without them, and
`make test` loads it alone first, so the dependency can only point outward: a
core file that named the TUI would not load. Anything a frontend does to a
session goes through the core's session operations (`boot-session`,
`switch-session`, `set-session-model`, `end-session`, …) and its command
layer; no frontend builds a journal entry by hand.

Two questions about the frontend are the core's to answer for extensions,
because extensions must not name a frontend: *is a person at a terminal?*
(`evo:frontend-interactive-p`) and *will somebody start a run for input
queued off-thread?* (`evo:request-run`). The CLI binds the answering object
(`evo.kernel:*frontend*`) before the session boots, so load-time decisions see
it: the LaTeX renderer's prompt note and the IDE bridge's status-line poller
exist only under an interactive frontend, and a notification reply steers the
TUI and a served session alike. Every frontend announces `:session-end` on its
way out, before its task stops.

### 14.1 Todo lists (D14)

Long-running goal work needs a user-visible checklist. Interactive sessions
can do without one; multi-hour unattended runs cannot, and this is the single
deliberate deviation from the minimal omit-list.

- The `todo` tool replaces the whole list per call; items are text plus
  `:pending`, `:in-progress`, or `:done`. Whole-list replacement keeps both
  the schema and the state fold trivial.
- State rides `:custom` entries — invisible to the LLM as entries, since the
  tool call and result already put the list in context when it mattered — so
  the current list is a fold over the path. It survives restart and compaction
  untouched.
- The TUI renders it in the managed bottom region; `/todo` toggles the panel;
  print and event modes emit it as events.
- The goal driver embeds the current todo snapshot in its continuation
  steering, so a run re-steered after a crash or a compaction knows where it
  left off.

## 15. Supervisor and self-healing

The supervisor is the `evo` binary invoked plainly (D17). There is no wrapper
script and no second executable: a wrapper would be another artifact to
install, would break TTY inheritance under POSIX background rules, and buys
nothing the binary cannot do itself. `--no-supervisor` runs the session
in-process.

1. **Spawn.** Re-spawn the same runtime (`evo.port:runtime-pathname`) with
   `EVO_SUPERVISED_CHILD=1` and inherited stdio, so the TTY passes straight
   through. On SBCL the heap is baked in at build time via
   `--dynamic-space-size` and `:save-runtime-options`, because the default
   heap is not sized for a long-running agent. The child loads kernel and core
   extensions, replays the session's `:load` entries, and resumes at the
   journal leaf.
2. **Monitor.** Process exit plus a heartbeat file the kernel touches on every
   event, with a generous configurable hang timeout — tool calls can legally
   run for a long time. A stale heartbeat means the child is killed.
3. **Restart.** Re-launch with `--resume <session>`. If the resumed session
   has an `:active` goal — a turn error leaves it active — the goal driver's
   idle continuation picks it up: crash, reboot, re-steer, with no human in
   the loop.
4. **Boot-failure quarantine.** After N failed boots, retry with
   `--no-userspace` — kernel and core extensions only — and report which
   `:load` entry was reached. The journal makes the culprit bisectable. This
   is the answer to "the agent bricked itself": recovery is editing a source
   file, never surgery on opaque state.
5. **Bounded loss.** Write-ahead journaling means a crash mid-turn loses at
   most the in-flight provider stream. The transcript up to it is on disk.

## 16. Interface

The CLI is newcomer-friendly and the TUI adapts to console size including live
resize (D4). The TUI is itself a core extension (§14), built entirely on the
public API and not disableable.

- Rendering goes into normal terminal scrollback — no alternate screen,
  because scrollback history is part of the UX — plus a managed bottom region
  (editor, status line, goal and budget indicators) repainted differentially.
- The **activity line** is one row and always present, settling to idle rather
  than disappearing. While a task runs it carries a clock and, when the
  transport is re-sending, the attempt and the reason — a spinner alone says
  only that the loop is alive, which is exactly what a hang looks like too.
  The clock times the **current step** — one turn of the loop, or one
  compaction — not the task. A task is not a step: steering drained at a turn
  boundary, a followup, and a goal that keeps going after the model settles all
  extend one task across many turns, so a task-wide clock reports time since
  the user last spoke. That number hides a wedge behind an hour of honest work,
  and telling a slow step from a wedged one is the only reason the clock is
  there.
- Resize is SIGWINCH through `evo.port` (polled on Windows), re-querying the
  size with `stty size` and reflowing the managed region. Long content wraps;
  wide content truncates with indicators.
- Raw mode goes through `stty` (console mode on Windows) and output is plain
  ANSI escapes — no curses, no FFI. That keeps the binary self-contained and
  the renderer debuggable. The scope is a deliberate subset: no windowing, no widgets
  beyond the editor, list-select, and confirm.
- **Editor** (D12): a plain multi-line text editor — no highlighting, and
  completion only for what is unambiguous to complete (Tab on a `/command`
  name or an `/eval` symbol) — because multi-line editing is the part that
  matters.
  - The editor region grows and reflows with content, as part of the managed
    bottom region.
  - **Enter sends; Shift+Enter inserts a newline.**
  - The editor never paints more rows than the screen has: it scrolls with
    the cursor, marking the lines it is hiding, because the managed region is
    drawn with relative cursor movement and a region taller than the terminal
    strands its own top in scrollback and duplicates it on every repaint.
  - **Paste collapse**: a paste too big to read in a three-line editbox —
    over three lines, or over a thousand characters — becomes a placeholder
    token such as `[paste #1: 42 lines]` (`[paste #1: 4200 chars]` for one
    long line), with the content held in a side buffer and substituted back
    in full when the message is sent.
  - **Paste-to-expand**: pasting the exact same content again with the cursor
    right after the placeholder replaces it with the real lines, editable in
    place. Paste once to keep it compact, twice to edit.
  - Shift+Enter is indistinguishable from Enter in legacy terminals, so the
    kitty keyboard protocol or `modifyOtherKeys` (CSI-u) is used where
    available, with Alt+Enter and Ctrl+J as fallbacks elsewhere.
- **Image input across terminals** (§16.1): the one feature whose plumbing is
  entirely terminal-dependent, so it is specified as a ladder rather than a
  gesture.
- Non-interactive modes are first-class: `evo -p "prompt"` for print mode and
  a line-delimited-sexpr event stream on stdout. They make evo scriptable and
  give the supervisor and the tests a UI-less harness.
- Streaming rendering is driven by the loop's event protocol (deltas plus a
  partial accumulator).
- The live image is reachable from inside through `/eval` (and the model's
  `eval` tool); evo ships no Swank listener.

### 16.0 A paste arrives in one of two shapes

Bracketed paste (`CSI ?2004h`) is a request, not a guarantee: a terminal may
ignore it, a multiplexer or ssh hop may eat it, and `tmux send-keys` and every
driver script bracket nothing at all. Both shapes must work, so evo reads
both, and normalizes at one door — `handle-paste`, which every gesture goes
through, so no two of them can drift apart.

- **Bracketed**: the payload arrives as data, between `ESC[200~` and
  `ESC[201~`.
- **Unbracketed**: the clipboard is simply typed at us as fast as the pty will
  carry it, with every line break spelled as the byte Enter sends. Naively
  read, the first line is submitted as a prompt and the rest of the clipboard
  races into the model behind it. Evo recovers the paste from its *arrival
  rate*: the poll loop drains the tty every ~20ms, so one batch of key events
  is one 20ms window — a human fills it with a character or two, a paste fills
  it with as many as the pty will carry. A batch that is nothing but text and
  line breaks and holds at least three characters was pasted, not typed, and
  is folded into the same `:paste` event a bracketed terminal would have sent.
  A line break *inside* the burst is part of the text; one at the very end is
  held for a tick and released as a real Enter, which is what keeps scripted
  drivers (`send-keys "/help\r"`) submitting. `EVO_PASTE_BURST=0` turns the
  detection off.
- **Line endings**: inside a paste, a line break is CR (xterm.js — VS Code,
  Cursor — rewrites every newline in the clipboard to CR, and raw mode does no
  CR→LF translation), CRLF (Windows clipboards), or LF. All three mean *new
  line*; dropping CR instead of translating it welds every pasted line into
  one. ANSI escape sequences and other control characters are dropped: a paste
  is text, not keystrokes.

### 16.1 Image input is a ladder, not a gesture

No terminal hands an application the image on the clipboard; the paste channel
carries text. Every gesture therefore reduces to the same act — *something*
tells evo the user meant "an image", and evo reads the system pasteboard
itself. The design consequence is that both halves must degrade
independently: the **trigger** (a keystroke or a paste that has to survive
whatever the emulator does with it) and the **read** (a platform tool that may
not exist in this session).

Triggers, in the order they are tried by a user and each covering terminals the
one before it does not:

| Trigger | Reaches evo as | Covers |
|---|---|---|
| ctrl+v | `0x16`, `CSI 118;5u`, or `CSI 27;5;118~` | every terminal — one of the three encodings always arrives |
| ctrl+alt+v | `ESC 0x16`, `CSI 118;7u`, `CSI 27;7;118~` | emulators that keep ctrl+v for their own paste (VS Code on Linux/Windows, Windows Terminal under WSL) |
| cmd+v, right-click → Paste | an *empty* bracketed paste | xterm.js emulators (VS Code, Cursor); Terminal.app and Warp send nothing at all and cannot be reached this way |
| cmd+v reported as a key | `CSI 118;9u` (super) | terminals that forward super instead of eating it |
| paste or drop a path | bracketed paste of a POSIX path, `file://` URL, quoted/escaped path, or a Windows path under WSL | every terminal with bracketed paste |
| `/image [path]`, `--image` | typed text | the floor: always available, including where every keystroke above is intercepted |

Two rules keep the trigger half honest. **Never request a key-encoding mode
you do not decode in full** — a half-decoded protocol makes a key do nothing
at all on exactly the terminals that honoured the request, which is worse than
never asking; `modified-key` is therefore one total decoder shared by the
CSI-u and `modifyOtherKeys` paths. And **ask only where the answer can be
understood**: no request under `TERM=dumb` or with no `TERM`, popped exactly as
pushed on exit, with `EVO_KEY_ENHANCEMENT=0` as the escape hatch.

The read half is `*clipboard-readers*`, tried in order: macOS pasteboard
(osascript), Wayland (`wl-paste`), X11 (`xclip`), Windows-from-WSL
(`powershell.exe`). Each takes pixels first and then the file the clipboard
merely *points* at, because a file-manager copy puts no pixels anywhere
(`«class furl»`, `text/uri-list`, `FileDropList`). When all of them come back
empty, the failure message distinguishes "the clipboard holds no image" from
"nothing here can read a clipboard" and names the missing piece — a session
over ssh with no display, or a missing `wl-clipboard`/`xclip`, is not the
user's clipboard being empty, and saying so sends them looking in the wrong
place.

### 16.2 `evo serve`: the session over HTTP

`evo serve` is the headless frontend (D20): the same binary, kernel, journal,
extensions and supervisor, with HTTP where the TUI would be — the base a
coordinator (`evo-swarm`) drives worker evos through, and the server a
program can run its own session on (D21). The protocol reference is
`docs/serve.md`; the shape, and why:

- **evo-native, not MCP or JSON-RPC.** POSTs are commands, GETs read state,
  one SSE stream carries events. What crosses is evo's own vocabulary — the
  `--events` plists and the journal's fold — under one mapping, the inverse
  of `evo:json->sexpr` (keywords ⇄ snake_case keys, keyword values as their
  lowercase names, `nil` as `null`), so JSON → sexpr → JSON is the identity
  on JSON values.
  A protocol designed for someone else's tools would have to be translated
  into this one anyway.
- **Commands are the TUI's.** `/command` dispatches through the command layer
  (§12) in the TUI's order; `/prompt` is the editor's send, `/steer` and
  `/follow-up` the kernel's two queues, `/interrupt` the esc key, `/eval` both
  the `/eval` command (one form) and the `eval` tool (a body). Anything a
  person can do in the TUI a program can do here; the presentational
  commands have no meaning without a screen.
- **Replies are what happened, not a promise.** A command runs to completion
  on the session thread and answers with what it said, its structured data,
  the task it left running, and a cursor into the event log. Asked to stream,
  it answers with SSE instead: the reply, then every event it caused until the
  session settles — the one honest "done" for a run, which is not the HTTP
  request that started it.
- **Events are numbered once.** The log assigns consecutive ids and keeps a
  bounded ring, so `Last-Event-ID` resumes exactly, and a cursor that fell out
  of the ring is told what it missed (`gap`) rather than handed a hole. A
  restarted process is a new log, announced by `hello`.
- **Security is a token and a bind.** Loopback unless `--allow-remote`; a
  bearer token on every request, random per launch and minted by the
  supervisor parent so restarts keep it, delivered through a 0600 file or
  `EVO_SERVE_TOKEN`. `/eval` is remote code execution by design — evo is
  permissive (§1) and says so; the token is the gate.
- **Small and portable.** HTTP/1.1 is ~200 lines over `usocket` octet
  streams (already in the image through dexador), one request per
  connection, `Content-Length` bodies only. No new dependency, the same code
  on SBCL, ECL and SBCL on Windows, and testable with in-memory streams.
- **A program adds two things, and only two.** serve's protocol is one
  frontend's; a program that runs its own session on it supplies an
  *identity* — name, version, features, reported by `/health` — and *routes*
  of its own: exact paths, or prefixes like `/lanes/N/…`, matched by exact
  path first and then longest prefix, behind the same token, the same size
  limits and the same single session thread. Neither seam names a program, so
  `evo` stays free of the swarm while `evo-swarm serve` adds its endpoints
  (D21, §18).

## 17. How evo evolves

Self-extension is a runtime capability (§13); this section is the policy that
governs where new capability *settles*.

### 17.1 The promotion ladder

Capability enters at the bottom and moves up only when it earns the move. Each
rung is more permanent, more reviewed, and harder to undo than the one below.

1. **Session userspace** — the agent writes a tool mid-run, loads it, uses it.
   Journaled, replayed on resume, scoped to the work that needed it. This is
   the default and needs no justification.
2. **Installed extension** — the file moves to `~/.evo/extensions/` or
   `<project>/.evo/extensions/` and loads at boot. Justification: it proved
   useful more than once.
3. **Core extension** — bundled, hand-written, compiled into the image.
   Justification: everyone needs it, and it must exist before the agent is
   trusted to install its own equivalent. The todo list is exactly this case —
   the agent's own plan has to be visible *before* an agent can be trusted to
   render it.
4. **Kernel** — only when the turn loop itself cannot function without it.

The rule that keeps this honest: **nothing enters the kernel to serve one
feature.** If a core extension needs something the public API cannot express,
the fix is to widen the API — which benefits every extension including the
agent's — never to add a private hook. A kernel change that would not survive
being offered to userspace is the wrong change.

### 17.2 Deliberate absences and their re-entry conditions

Each omission is a decision, not an oversight, and each has a condition that
would reopen it.

| Absent | Why | What would change it |
|---|---|---|
| Sub-agents *inside* one agent | Parallel agents shipped as separate processes instead — evo-swarm (§18, D16): a coordinator driving `evo serve` lanes over HTTP. Nothing in the evo binary knows about it | A workload that needs children sharing one image — none foreseen, since a lane is cheaper to reason about than a thread |
| Parallel tool execution | Sequential execution is where the thread-discipline complexity *isn't* (D9) | Measured wall-clock loss on independent calls, plus a thread discipline for the journal writer |
| MCP *in the kernel* | It shipped as a userspace extension instead (`extensions/500-mcp.lisp`: Streamable HTTP, tools only, no auth flow beyond configured headers) — `register-tool` plus an HTTP client is the whole client. The kernel gained no protocol, only the two seams any foreign contract needs: a JSON Schema passed through verbatim, and `:arguments :json` so a tool receives the model's exact JSON rather than the lossy plist spelling | A transport that cannot be written in userspace — stdio servers (child process + pipes) are the candidate |
| Permission prompts | Permissiveness is a defining property; the `:tool-call` hook is the seam, and `permission-gate.lisp` is the worked example | A deployment context where the OS boundary is not the trust boundary |
| Multimodal *output* (image generation, audio) | Input landed (`evo.media` + `:image` blocks, ctrl+v / paste-a-path / `/image` / `--image`); generation is a different shape — artifacts the agent produces, which the journal-as-text model has no place for yet | An artifact store with the same replay guarantees as the journal |
| Cost tables | Token accounting is the honest unit; prices go stale | Nothing foreseen |

### 17.3 What must not break

Change filters, in descending order of severity. Violating one of these is not
a refactor.

- The journal stays the only source of truth, and state stays a fold
  (invariants 1–2). Any feature wanting a mutable field is misdesigned.
- The extension API stays the only door (invariant 3). Two tiers, one API.
- The transcript stays well-formed under every failure (invariant 4).
- Recovery stays "edit a source file" (invariant 6). No opaque state, ever.
- The prompt prefix stays cache-stable. A per-turn-varying prefix is a silent
  cost regression, not a cosmetic one.
- Permissiveness stays the default. Rails exist so the agent breaks itself
  deliberately; they are not there to stop it.

---

## 18. evo-swarm: one coordinator, a pool of lanes

evo-swarm (D16) runs parallel agents as a separate program on top of evo: its
own system (`evo-swarm.asd`, `swarm/`) and binary, depending on `evo`, never
the other way round (`tests/evo-only.lisp`). The reference is
`docs/swarm.md`; the decisions:

- **One human-facing agent.** The coordinator is an ordinary evo session
  (the same `setup-agent`, the same TUI or serve frontend) with the swarm
  tools and a prompt note. Delegation is its own decision, not the user's
  instruction: it
  explores, splits anything non-trivial into lane-sized pieces with checkable
  done criteria, delegates, keeps lanes busy, integrates, verifies, and
  reports. Input always goes to
  it; the human watches lanes read-only (a status-line segment, `/lanes`,
  `/lane N` in the TUI; `GET /lanes` and its siblings when served).
- **Lanes, not roles.** A lane is `evo serve --no-userspace` on loopback: its
  own process, bearer token, port and journal. Default 6; started with the
  swarm, idle until given work. What a lane *is* for a task is the prompt the
  coordinator gives it.
- **Only the public API.** The coordinator reaches lanes through serve's HTTP
  endpoints and nothing else — prompts, steering, interrupts, slash commands,
  eval, state — and subscribes to each lane's `/events`. Lanes never talk to
  each other.
- **Served as well as typed.** `evo-swarm serve` runs the same coordinator and
  lanes under serve's frontend, headless: `/health` names it `evo-swarm` with
  the feature `swarm`, the coordinator is driven through the unchanged
  protocol, and the read-only view of the lanes becomes three endpoints —
  `GET /lanes`, a `lane-state` event on the coordinator's `/events` when a
  lane changes, and a lane's `GET /lanes/N/transcript` and
  `GET /lanes/N/events`. Read-only as in the TUI: lane control stays the
  coordinator's, and a lane's token and URL are never handed out. The swarm's
  notices and repaints go to whichever frontend it runs under (D21), so one
  swarm implementation serves both.
- **Reports are input.** A lane's `report` tool emits a `:report` event;
  reports, finished runs, errors, crashes and restarts become the
  coordinator's input through the frontend protocol (`evo:steer` +
  `evo:request-run`): queued to its next turn boundary when it works, waking
  it when idle. The coordinator never polls.
- **Lanes get the coordinator's setup, plus in-lanes.** A lane loads none of
  the user's files; the coordinator evaluates its setup into it, a form at a
  time — on start, on every restart, then the code the coordinator evaluated
  into it since. The **baseline** gives it the coordinator's providers and
  default model and thinking, then the `in-lanes` forms from `swarm.lisp`,
  then the coordinator's models those did not register and whose API the
  lane has (after them, because a model can depend on an API an extension
  defines; a model on an API the lane lacks is skipped, and only a missing
  default model is an error, one that says which extension to load), and
  last the report tool, the worker note and the tool limit. `swarm.lisp` is evo-swarm's own
  config, loaded as the last step of the userspace build (a
  `*post-init-hooks*` entry, so evo never names the swarm) — it can set the
  coordinator's models for the swarm. `in-lanes` is a macro because its body
  runs elsewhere: it records source, which the coordinator sends to every
  lane, and it binds the lane's number and the lane count in its syntax, as
  `dolist` binds its variable, rather than pretending to be a function.
  **Keys never travel as
  data**: a provider's key reaches a lane as an environment variable, by
  name, and a literal key through a swarm-private variable set in the lane's
  process environment only.
- **Supervision and ownership.** Lanes run under evo's own supervisor (a crash
  restarts them with `--resume`, which finds the lane's own session through
  `EVO_SESSIONS_DIR` — the same variable that keeps lane journals out of the
  coordinator's `/resume` list). One subscriber thread per lane owns its
  stream and notices a restart by the new pid, re-initializes the lane and
  tells the coordinator. Lanes watch the coordinator's pid
  (`EVO_SERVE_WATCH_PID`) and stop when it dies; quitting the coordinator
  stops them all (`:session-end`). One lock guards the lane table (§6).
- **The journal records the swarm.** The coordinator's session carries the
  swarm as `:custom` state (id, lanes, worktrees, evaluated code), so
  `evo-swarm --resume` — or the supervisor's restart — restores coordinator
  and lanes: each resuming its own session in its own directory.
- **Isolation on demand.** Lanes share the coordinator's directory by default;
  the coordinator gives a lane a git worktree and branch when a task needs
  one, and merges the branch itself.

## Appendix A — decision record

| # | Decision | Rationale |
|---|---|---|
| D1 | Sessions are an append-only entry **tree** in a journal file; state is a fold over the root→leaf path. | Branching, rewind, resume, and pause fall out for free; write-ahead makes it crash-safe. |
| D2 | **No image-based session persistence.** The journal is the only source of truth; Lisp images are build artifacts. | Both target APIs are stateless and replay in full, so transcript-as-data is mandatory anyway. Images are opaque, undiffable, and propagate corruption. |
| D3 | **Journal format is native sexprs**, one form per line, and sexprs are the default for every data format evo controls (lore, goal state). Config is not data: init.lisp is evaluated Lisp (D6). | Human-readable and `read`-able from Lisp with no external serialization dependency. |
| D4 | **A CLI with an adaptive TUI**, mandatory live console-resize. Not Emacs/Swank-first. | Approachable for newcomers and adapts to the most contexts. The live image stays reachable from inside (`/eval`), not through an editor. |
| D5 | **A supervisor owns launch, crash detection, restart, and resume.** | Long-running goal pursuit requires surviving self-inflicted death. |
| D6 | **One adapter ships** (Anthropic Messages) as a CLOS *provider API*; **models and endpoints are user-registered from init.lisp**. A wire protocol is an extension point, not a kernel privilege: `evo:register-api` takes any `provider-api` subclass. One unified message model for all APIs. Anthropic's own models (Sonnet 5, Opus 5, Fable 5) and Messages-compatible third-party endpoints (Kimi Code K3, DeepSeek, proxies) all ride the same adapter; the OpenAI Responses adapter was deleted when it stopped earning its parsing surface. | The API/registry split keeps the bundled protocol curated while models stay configuration, avoiding a 40-provider table. Making the protocol registerable follows D13: if the TUI can be a core extension, a wire protocol can be a user one. |
| D7 | The goal system follows **codex's design**: persisted goal, idle-continuation steering, explicit audited completion, budgets. | See §9. |
| D8 | The kernel/userspace split is enforced with **package locks** (SBCL native, ECL `si:package-lock`, both behind `evo.port`). | Permissive but not suicidal: touching the kernel requires an explicit, auditable unlock. |
| D9 | Tool execution is **sequential**. | Parallelism is where the thread-discipline complexity lives, and nothing yet demands it. |
| D10 | **SBCL and ECL on Unix, SBCL on Windows**, through a single portability layer (`evo.port` — the only package permitted to touch `sb-*`, `ext:`, or `si:` symbols, and now the only one that may branch on the platform). Two axes, not one: implementation *and* platform. Windows branches read on an `:evo-windows` feature the layer pushes itself, so a new implementation is one form, not fifty. | The implementation-specific surface proved small: env/argv/exit, processes, locks, fd streams, signals. Windows added a second small one — console mode instead of stty, no SIGWINCH (poll), `taskkill` instead of `pgrep`+`kill`, PowerShell instead of `/bin/sh`, PATHEXT — and asking the console for VT input/output means the key parser, the escape sequences and the renderer are untouched by it. ECL on Windows stays unsupported: it would need its own copy of that surface with no user waiting for it. |
| D11 | Naming: binary `evo`, directories `~/.evo/` and `<project>/.evo/`, package prefix `EVO.`. | Settled to stop revisiting it. |
| D12 | The TUI editor is a **plain multi-line text editor**: Enter sends, Shift+Enter inserts a newline, pastes over three lines collapse to a placeholder that re-pasting expands. No highlighting; Tab completes only `/command` names and `/eval` symbols. | Multi-line editing is crucial UX; editor sophistication is not where the novelty is. |
| D13 | **Slim core: everything outside the core loop ships as a core extension** — bundled, on the same API, with the same control as user extensions; essential ones cannot be disabled. | Dogfooding proves the API's depth and keeps the kernel small and honest. See §14. |
| D14 | **Todo checklists ship**, as a core extension. | Long-running goal work needs user-visible progress. The one deliberate deviation from the minimal omit-list. |
| D16 | **Parallel agents are a swarm of processes, not sub-agents.** `evo-swarm` (§18) runs one coordinator agent — the only one a human talks to — and a pool of interchangeable worker *lanes*, each a whole `evo serve` process driven only through serve's public HTTP API. Lanes never talk to each other; they report to the coordinator, whose input their reports become. Lanes get the coordinator's setup plus code swarm.lisp gives them (`in-lanes`). This replaces "no sub-agents". | Context isolation did demonstrably beat one transcript once goals outgrew one context — the re-entry condition D16 named. Processes rather than in-image children because every guarantee evo has — the journal as truth, supervision, resume, a crash domain of one — then holds per lane for free, and the coordinator exercises serve's API as any client would. Lanes rather than roles because a role is a prompt, which the coordinator can give any lane per task. |
| D17 | **One binary per program, each its own supervisor.** No shell launcher and no separate supervisor executable: `evo` invoked plainly *is* the supervisor parent, re-spawning itself as the session child; `evo-swarm`, a second program (D16), is built the same way on the same supervisor (`evo.cli:supervise` with its own restart arguments). On SBCL the heap is baked in at build time, refining D10. `--no-supervisor` runs in-process. The evo binary contains no swarm code — `make test` loads the `evo` system alone to prove it (`tests/evo-only.lisp`). | A wrapper script is one more artifact to install, breaks TTY inheritance under POSIX background rules, and buys nothing the binary cannot do itself. A second *program* (evo-swarm) is not a wrapper: it has its own users and its own UI, and keeping it out of evo keeps the agent's binary exactly what one agent needs. |
| D18 | **The core is its own system.** `evo/core` (foundations, kernel, interface-free core extensions) loads without the frontends; the TUI and the CLI build on it in `evo` and define their own packages, and `make test` loads `evo/core` alone before the unit suite. | D13 keeps the kernel small by convention; this makes the direction checkable. A core file that names a frontend does not load, so the question "is the core coupled to the TUI" is answered by the build rather than by reading. |
| D19 | **No extension patches the core.** Every seam a bundled extension once reached with a function patch or a private symbol is public: `:busy`/`:idle` for the drive, `:user-message` for user input, generation-owned TUI registries, `provider-registration`, a clipboard reader that explains itself. | A patch is a report of a missing protocol. Patches cannot see each other, have to be undone by hand, and break when the patched function's signature grows; a hook or registry has none of those problems, and it belongs to the extension's generation. |
| D20 | **One command layer, many frontends; `evo serve` is one of them.** What a slash command does is core code (`src/command/`) behind a small host protocol; the TUI and the HTTP frontend both dispatch through it. `evo serve` is a mode of the one binary with an evo-native HTTP/SSE protocol (§16.2), not MCP or JSON-RPC, and not a second program. Extensions ask the core — `evo:frontend-interactive-p`, `evo:request-run` — never a frontend. | A coordinator must be able to do anything a person can, and the only way to keep two frontends from drifting is to give them one copy of every command. A separate server binary would duplicate the supervisor, the boot and the journal; a foreign protocol would need translating into evo's vocabulary on every call. |

| D21 | **One protocol for both programs: serve's program seam is an identity plus routes, and `evo-swarm serve` runs the swarm headless on it.** `/health` reports the server's *identity* — name, version, features; `evo serve` is `evo` with none — and a program adds *routes*, exact paths or prefixes like `/lanes/N/…`, matched by exact path first and then longest prefix, behind the same token, size limits and single session thread. `evo-swarm serve` is `evo-swarm` with the feature `swarm`: `GET /lanes`, a `lane-state` event on the coordinator's `/events` when a lane changes, and a lane's read-only `GET /lanes/N/transcript` and `GET /lanes/N/events`. The swarm's notices and repaints go through the frontend it runs under, so the TUI and serve run the same swarm code. | A GUI drives both binaries with one client and one layout, so it must be able to tell which server it is talking to, and the swarm half must not need a second protocol. Identity and routes are the general form of "this program adds a feature", which keeps both the core and serve free of the swarm's name — `make test` still loads `evo` alone. The lane endpoints are read-only because lane control belongs to the coordinator, the one agent a human talks to (§18), and lane tokens and URLs stay private, so a served swarm keeps the TUI's trust boundary: the client reads lanes, it never talks to them. |

## Appendix B — provenance

evo is not designed from scratch, and the borrowings are deliberate:

- **pi-mono** (`~/Projects/pi`) — the agent loop, session tree, compaction, and
  self-extension anatomy. The journal model is pi's, structurally verbatim,
  with sexprs replacing JSON. Departures are noted where they occur: the
  transcript is owned by the journal rather than dual-copied, todo lists are
  added, and the runtime loader needs no queued reload.
- **codex** (`~/Projects/codex`, `codex-rs/ext/goal/`) — the goal system:
  persisted objective, idle continuation, audited completion, budgets.
- **codex** again (`codex-rs/tui/src/clipboard_paste.rs`,
  `tui/keyboard_modes.rs`) — the image-paste ladder of §16.1. Taken: ctrl+v
  *and* ctrl+alt+v as two doors to one clipboard read; pasted-path
  normalization (`file://`, quotes, shell escapes, Windows paths mapped into
  WSL); the PowerShell bridge that reaches the Windows clipboard from a WSL
  session, including the file-copy case; an env escape hatch for terminals
  that mishandle enhanced key reporting. Taken later, once the same failure
  showed up on POSIX terminals and Windows joined the target list (D10): the
  idea of reconstructing a paste that arrived as a rapid stream of keypresses
  — though evo reads it off the poll batch rather than running codex's timer
  state machine (§16). Added beyond it: reading
  the empty bracketed paste as the cmd+v gesture, downscaling oversized
  images rather than failing at the provider, and images by value in the
  journal (D2) instead of paths that can go stale.
- **lisp-references/** (repo root) — Common Lisp and SBCL reference material
  for whoever is implementing evo, human or agent. If you lack the CL or SBCL
  knowledge for a task, read here before guessing. Browse it fresh each time;
  its contents vary by developer, so assume no particular structure.
