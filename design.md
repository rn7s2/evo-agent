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
  and the seed corpus (§12).
- **Permissive.** There are no permission prompts. The trust boundary is the
  OS or container evo runs in, plus a kernel/userspace split that lets the
  agent break itself deliberately but not accidentally.
- **Self-healing.** Crash, restart, resume the session, continue the goal. The
  supervisor and the journal together make process death a recoverable event
  rather than a lost run.
- **Minimal.** Core functionality of a real agent and nothing ceremonial. The
  omit-list — no permission popups, no MCP, no sub-agents — is deliberate and
  each omission has a stated re-entry condition (§16).

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
evo (one binary)
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
   │    provider APIs        CLOS protocol; anthropic-messages and
   │                         openai-responses bundled; model and provider
   │                         registries fed from init.lisp
   │    tool registry        register / activate / refresh, prompt rebuild
   │    extension API        register-tool, register-command, event hooks —
   │                         both extension tiers build on this
   │    goal driver          idle-continuation loop, budgets, audits
   │    compactor            usage-anchored, self-contained checkpoints
   │    budget guard         per-goal and per-session hard stops
   │    extension loader     compile, load, journal the load
   │    media                images in: clipboard readers, sniffing,
   │                          size cap + downscale, :image blocks
   ├─ CORE EXTENSIONS  (bundled, hand-written, compiled into the image;
   │                    same API and privileges as user extensions)
   │    tui        adaptive renderer, multi-line editor (image attachments
   │               as editable tokens), slash commands
   │    todo       checklist tool + :custom state, rendered by the tui
   ├─ USERSPACE  (unlocked: EVO.USER)
   │    agent-written tools and code — source files plus journal :load
   │    entries; rebuilt from source on every boot
   └─ RESOURCES
        skills/ (progressive disclosure)   prompts/ (templates)
        lore store (out-of-band, injected every turn)
        docs corpus (paths named in the system prompt)
```

Directory conventions:

- Global `~/.evo/` — `init.lisp`, `sessions/`, `extensions/`, `skills/`,
  `prompts/`, `lore.sexp`, `docs/`
- Project `<cwd>/.evo/` — `init.lisp`, `extensions/`, `skills/`, `prompts/`

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
```

Three of these carry architectural weight:

- The `:custom` / `:custom-message` split separates *state* from *context*. It
  is how a tool persists state without polluting the prompt.
- `:compaction` materializes the retained tail on the entry, so rebuilding
  context is `[summary, ...retained-tail, ...entries-after]` — O(1), with no
  walk past the compaction.
- `:load` is what makes runtime evolution replayable. Boot is: load kernel,
  then replay the path's `:load` entries against the source files on disk.

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

One unified message model, two bundled adapters, one extension point.

**Provider APIs are a protocol; models are configuration.** A wire protocol is
a CLOS class implementing `endpoint-path`, `auth-headers`, `build-request`,
`parse-stream`, `thinking-param`, and `perform-request`, dispatched from
`call-provider` on the model's `:api` tag. The bundled protocols live in
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
  to lose. Adapters encode it natively (Anthropic `image`/base64 source,
  OpenAI `input_image` data URL); the handoff pass degrades it to a named text
  placeholder for a model registered `:vision nil`, so a model switch cannot
  poison a transcript that contains one. `evo.media` owns the read path —
  clipboard readers per platform, size cap, downscaling — and nothing above it
  knows where the bytes came from.
- **Provider artifacts are a typed variant, not stringly-typed**: an Anthropic
  thinking signature is a base64 scalar accumulated from chunked
  `signature_delta`; an OpenAI reasoning item is the whole item with
  `encrypted_content`.
- **Stateless replay** everywhere: full history per request, OpenAI
  `store: false`, no `previous_response_id`.
- **Handoff pass** at request build: same-model thinking replays verbatim;
  cross-model thinking degrades to plain text or is dropped; orphaned tool
  calls receive synthetic error results; errored and aborted assistant turns
  are elided; any model switch drops OpenAI `fc_*` item ids, since the server
  validates `fc_*`↔`rs_*` same-response pairing.
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
  message strings.
- **Caching**: Anthropic uses four breakpoints (system prompt, last tool
  definition, last user message); OpenAI uses `prompt_cache_key` = session id.
  Protecting the cache prefix is a constraint the whole prompt design honors
  (§10).
- **Model registry**: user-registered plists (id, context window, max output,
  thinking flag), registration-ordered for the `/model` picker. Token
  accounting only — no cost table.
- CL stack: `dexador` with `:want-stream t` plus `cl+ssl` and explicit read
  timeouts; `com.inuoe.jzon` on the wire. Abort is a cooperative flag plus
  closing the socket from another thread, never `interrupt-thread`.

## 6. Agent loop

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
  continues. The goal driver (§8) plugs in here.
- Tool interface: name, description, sexpr schema (emitted as JSON Schema),
  `execute` function, and a result split into `:content` (model-visible) and
  `:details` (host-visible). Execution is sequential (D9).

## 7. Context management

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

## 8. Goal system (`/goal`)

### 8.1 Model

A goal is journal state; the current goal is a fold over `:goal` entries.

```lisp
(:goal-id "g-01" :objective "..." :status :active
 :token-budget 500000 :tokens-used 123456
 :done-when nil)          ; name of an agent-authored userspace predicate (D15)
```

Statuses are `:active`, `:paused`, `:blocked`, `:budget-limited`, `:complete`.
Through `update_goal` the model may transition to `:complete` or `:blocked`
(under the audit rules below), `:paused` (stop the idle loop when it needs the
user) and back to `:active` (resume), and may **refine** the live goal —
rewrite its `:objective` or attach/replace its `:done-when` verifier. The user
does not drive the goal directly; they express intent and the agent folds it
in (this is by design — the same tool surface serves the human's requests and
the agent's own judgement). Budget transitions belong to the system.

### 8.2 Driver

- `/goal <objective>` creates or refines the goal. Refinement appends a new
  `:goal` entry and, if a run is active, injects an "objective updated"
  steering message.
- **Idle continuation**: whenever the agent goes idle with an `:active` goal,
  the driver opens a new turn seeded with a continuation steering prompt
  carrying the objective (as untrusted data), budget numbers,
  anti-scope-shrinking fidelity rules, a **completion audit** (completion must
  be proven from current evidence — files, test output, runtime behavior —
  requirement by requirement, never from memory or intent), and a **blocked
  audit** (declare `:blocked` only after the same blocker recurs across three
  consecutive goal turns).
- Doing nothing is not completion. An idle active goal is always re-steered.
  Termination is explicit: the model calls `update_goal` (complete/blocked), a
  budget trips, or the goal is paused.
- **Pause/resume**: `update_goal :paused` stops the idle-continuation loop —
  the settled hook only re-steers an `:active` goal, so a paused goal settles
  and waits. It does not auto-resume on session restart either (resumption
  keys on `:active`). The model pauses when it genuinely needs the user before
  proceeding, then resumes with `update_goal :active`. Headless, a paused goal
  exits like a blocked one (code 2, human needed, no auto-restart).
- **Budget accounting** runs every turn over tokens. Exhaustion moves the goal
  to `:budget-limited`, and the next steering is a wrap-up template —
  summarize progress, remaining work, next step, start nothing new. This
  doubles as the runaway-cost brake; a session-level budget exists too.
- A turn error moves the goal to `:blocked`, which is also the supervisor
  hook: on restart, a goal blocked by `turn-error` rather than by model
  decision is eligible for automatic resumption (§14).

### 8.3 Model-facing tools

`get_goal`; `create_goal`, which is for explicit user requests only and
refuses while an unfinished goal exists; and `update_goal`, which changes
status (`complete`/`blocked`/`paused`/`active`) **and** refines the live goal
(`objective` text, `done_when` verifier) — at least one field required, the
audit language carried in the tool description itself.

### 8.4 Verified completion

`:done-when` is designed for the **agent** to fill, not the user (D15). Users
state objectives in prose; when an objective is mechanically checkable the
agent formalizes it — writing a *named* zero-argument predicate into a
userspace file, journaled via `:load` so it survives restart (closures do not
round-trip through sexprs) — and references it by name on the `:goal` entry.
The continuation prompt steers the agent to derive the predicate up front, at
goal start, not at completion time, and nags only while none is attached. A
predicate can be attached at creation (`create_goal done_when`) or added to a
live goal later (`update_goal done_when`) — the latter matters because a goal
the user set with `/goal` has no predicate until the agent writes one.

`update_goal :complete` is then not taken at its word: the kernel runs the
predicate. Failure returns an error carrying the predicate's output and the
goal stays `:active`. The model's completion claim becomes a checked assertion
it wrote against itself. A lazy `(defun goal-done-p () t)` remains possible;
the mitigation is that the predicate is a journaled, user-visible artifact
written before the work, when there is no victory to declare yet. The feature
is optional — `/goal` works without it — and closes the premature-victory hole
in about twenty lines of kernel code.

## 9. Lore system (`/lore`)

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

## 10. The system prompt

The prompt is assembled fresh on every save point, from source, in a fixed
order: base → tool one-liners → guidelines → own-docs paths → lore → project
context files → skills → environment → language → gitStatus. It is cheap and
pure enough to rebuild that often, which is what keeps invariant 5 honest.

Its content is the agent's operating manual, and it states the things the
architecture cannot enforce: that evo is permissive and pre-authorized to
extend itself, how to reach for `load_extension` rather than declaring a
capability missing, how to weigh reversibility when nothing prompts for
permission, how to handle git, and how to treat injected system material.

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

## 11. Skills, prompt templates, slash commands, modes

- **Skills**: the Agent Skills standard (SKILL.md plus frontmatter) with
  progressive disclosure — only name, description, and path go into the prompt
  inside `<available_skills>`; the model reads the file on demand.
  `/skill:name` forces one. Markdown stays the format here because the
  standard is external.
- **Prompt templates**: `.md` files whose filename is the command, with
  `$1`..`$9` and `$@` substitution. Purely textual expansion.
- **Slash command resolution**: extension commands → input hook → skills →
  templates → send to the agent. Built-ins are `/goal /lore /global-lore
  /compact /tree /fork /resume /model /reload /export /help /quit /exit`.
- **One mode.** There is no mode switch and no mode indicator: the agent is
  fully permissive, always, and that is the whole design (D2). What a mode
  would have been built from stays public API, so a userspace extension can
  still impose a policy without the kernel knowing: `set-active-tools` gates
  the tool set as journal state (`:tools-change`), `inject-context` adds a
  keyed `:custom-message`, a `:transform-context` hook filters that key back
  out of the projection, and the `:tool-call` hook is the per-call gate.

## 12. Self-extension

This is the evolution engine. Four mechanisms make it work.

1. **Loader.** `evo:load-extension <path>` compiles and loads a file into a
   userspace package and journals a `:load` entry. CL redefinition semantics
   mean a tool can load code from inside its own execution; the new definition
   applies from the next call, so no trampoline or queued reload is needed.
2. **Registration API.** `(evo:register-tool ...)`,
   `(evo:register-command ...)`, and `(evo:on <event> fn)`. Mutations refresh
   the tool registry and rebuild the system prompt, so a newly registered tool
   is callable on the next request. The `:tool-call` hook may mutate arguments
   or return `(:block t :reason ...)` — the single interception point that
   permission gates, read-only policies, and sandboxing all build on.
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
  already running.
- **State discipline**: extension in-memory state does not survive a restart.
  Extensions rebuild it from `:custom` journal entries on `session-start`.
- **Repair discipline**: because evolution replays from source, a broken
  runtime is fixed by editing a file (invariant 6), and the supervisor's
  quarantine makes the offending file bisectable (§14).

## 13. Core extensions

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

### 13.1 Todo lists (D14)

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

## 14. Supervisor and self-healing

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
   has an `:active` goal, or one `:blocked` by `turn-error`, the goal driver's
   idle continuation picks it up: crash, reboot, re-steer, with no human in
   the loop.
4. **Boot-failure quarantine.** After N failed boots, retry with
   `--no-userspace` — kernel and core extensions only — and report which
   `:load` entry was reached. The journal makes the culprit bisectable. This
   is the answer to "the agent bricked itself": recovery is editing a source
   file, never surgery on opaque state.
5. **Bounded loss.** Write-ahead journaling means a crash mid-turn loses at
   most the in-flight provider stream. The transcript up to it is on disk.

## 15. Interface

The CLI is newcomer-friendly and the TUI adapts to console size including live
resize (D4). The TUI is itself a core extension (§13), built entirely on the
public API and not disableable.

- Rendering goes into normal terminal scrollback — no alternate screen,
  because scrollback history is part of the UX — plus a managed bottom region
  (editor, status line, goal and budget indicators) repainted differentially.
- Resize is SIGWINCH through `evo.port`, re-querying `TIOCGWINSZ` and
  reflowing the managed region. Long content wraps; wide content truncates
  with indicators.
- Raw mode and ANSI escapes go through a thin CFFI termios layer. Avoiding a
  curses dependency keeps the binary self-contained and the renderer
  debuggable. The scope is a deliberate subset: no windowing, no widgets
  beyond the editor, list-select, and confirm.
- **Editor** (D12): a plain multi-line text editor — no completions, no
  highlighting — because multi-line editing is the part that matters.
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
    available, with a documented fallback of Alt+Enter or
    backslash-then-Enter elsewhere.
- **Image input across terminals** (§15.1): the one feature whose plumbing is
  entirely terminal-dependent, so it is specified as a ladder rather than a
  gesture.
- Non-interactive modes are first-class: `evo -p "prompt"` for print mode and
  a line-delimited-sexpr event stream on stdout. They make evo scriptable and
  give the supervisor and the tests a UI-less harness.
- Streaming rendering is driven by the loop's event protocol (deltas plus a
  partial accumulator).
- `--swank <port>` is the developer side-door for live image inspection, off
  by default.

#### 15.0.1 A paste arrives in one of two shapes

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

### 15.1 Image input is a ladder, not a gesture

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

## 16. How evo evolves

Self-extension is a runtime capability (§12); this section is the policy that
governs where new capability *settles*.

### 16.1 The promotion ladder

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

### 16.2 Deliberate absences and their re-entry conditions

Each omission is a decision, not an oversight, and each has a condition that
would reopen it.

| Absent | Why | What would change it |
|---|---|---|
| Sub-agents | The journal tree already models a child session as a forked journal; nothing forces the feature yet (D16) | A workload where context isolation demonstrably beats one transcript |
| Parallel tool execution | Sequential execution is where the thread-discipline complexity *isn't* (D9) | Measured wall-clock loss on independent calls, plus a thread discipline for the journal writer |
| MCP | `register-tool` plus a userspace client covers the same ground without a protocol in the kernel | A tool ecosystem that cannot be reached any other way |
| Permission prompts | Permissiveness is a defining property; the `:tool-call` hook is the seam, and `permission-gate.lisp` is the worked example | A deployment context where the OS boundary is not the trust boundary |
| Multimodal *output* (image generation, audio) | Input landed (`evo.media` + `:image` blocks, ctrl+v / paste-a-path / `/image` / `--image`); generation is a different shape — artifacts the agent produces, which the journal-as-text model has no place for yet | An artifact store with the same replay guarantees as the journal |
| Cost tables | Token accounting is the honest unit; prices go stale | Nothing foreseen |

### 16.3 What must not break

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

## Appendix A — decision record

| # | Decision | Rationale |
|---|---|---|
| D1 | Sessions are an append-only entry **tree** in a journal file; state is a fold over the root→leaf path. | Branching, rewind, resume, and pause fall out for free; write-ahead makes it crash-safe. |
| D2 | **No image-based session persistence.** The journal is the only source of truth; Lisp images are build artifacts. | Both target APIs are stateless and replay in full, so transcript-as-data is mandatory anyway. Images are opaque, undiffable, and propagate corruption. |
| D3 | **Journal format is native sexprs**, one form per line, and sexprs are the default for every data format evo controls (lore, goal state). Config is not data: init.lisp is evaluated Lisp (D6). | Human-readable and `read`-able from Lisp with no external serialization dependency. |
| D4 | **A CLI with an adaptive TUI**, mandatory live console-resize. Not Emacs/Swank-first. | Approachable for newcomers and adapts to the most contexts. Swank stays a developer side-door, not the product. |
| D5 | **A supervisor owns launch, crash detection, restart, and resume.** | Long-running goal pursuit requires surviving self-inflicted death. |
| D6 | **Both adapters ship** (Anthropic Messages, OpenAI Responses) as CLOS *provider APIs*; **models and endpoints are user-registered from init.lisp**. A wire protocol is an extension point, not a kernel privilege: `evo:register-api` takes any `provider-api` subclass. One unified message model for all APIs. | The API/registry split keeps bundled protocols curated while models stay configuration, avoiding a 40-provider table. Making the protocol registerable follows D13: if the TUI can be a core extension, a wire protocol can be a user one. |
| D7 | The goal system follows **codex's design**: persisted goal, idle-continuation steering, explicit audited completion, budgets. Optional Lisp acceptance predicate as a kernel-side verifier. | See §8. |
| D8 | The kernel/userspace split is enforced with **package locks** (SBCL native, ECL `si:package-lock`, both behind `evo.port`). | Permissive but not suicidal: touching the kernel requires an explicit, auditable unlock. |
| D9 | Tool execution is **sequential**. | Parallelism is where the thread-discipline complexity lives, and nothing yet demands it. |
| D10 | **SBCL and ECL on Unix, SBCL on Windows**, through a single portability layer (`evo.port` — the only package permitted to touch `sb-*`, `ext:`, or `si:` symbols, and now the only one that may branch on the platform). Two axes, not one: implementation *and* platform. Windows branches read on an `:evo-windows` feature the layer pushes itself, so a new implementation is one form, not fifty. | The implementation-specific surface proved small: env/argv/exit, processes, locks, fd streams, signals. Windows added a second small one — console mode instead of stty, no SIGWINCH (poll), `taskkill` instead of `pgrep`+`kill`, PowerShell instead of `/bin/sh`, PATHEXT — and asking the console for VT input/output means the key parser, the escape sequences and the renderer are untouched by it. ECL on Windows stays unsupported: it would need its own copy of that surface with no user waiting for it. |
| D11 | Naming: binary `evo`, directories `~/.evo/` and `<project>/.evo/`, package prefix `EVO.`. | Settled to stop revisiting it. |
| D12 | The TUI editor is a **plain multi-line text editor**: Enter sends, Shift+Enter inserts a newline, pastes over three lines collapse to a placeholder that re-pasting expands. No completions or highlighting. | Multi-line editing is crucial UX; editor sophistication is not where the novelty is. |
| D13 | **Slim core: everything outside the core loop ships as a core extension** — bundled, on the same API, with the same control as user extensions; essential ones cannot be disabled. | Dogfooding proves the API's depth and keeps the kernel small and honest. See §13. |
| D14 | **Todo checklists ship**, as a core extension. | Long-running goal work needs user-visible progress. The one deliberate deviation from the minimal omit-list. |
| D15 | `:done-when` predicates are **agent-authored, not user-written**: named userspace functions journaled via `:load` and referenced by name on the `:goal` entry. | Users state objectives in prose; the agent formalizes them. Named, journaled functions survive restart; closures do not round-trip through sexprs. |
| D16 | **No sub-agents.** | The journal-tree model extends naturally to them (a child session is a forked journal) whenever they are wanted, so nothing is lost by waiting. |
| D17 | **One binary total.** No shell launcher and no separate supervisor executable: `evo` invoked plainly *is* the supervisor parent, re-spawning itself as the session child. On SBCL the heap is baked in at build time, refining D10. `--no-supervisor` runs in-process. | A wrapper script is one more artifact to install, breaks TTY inheritance under POSIX background rules, and buys nothing the binary cannot do itself. |

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
  `tui/keyboard_modes.rs`) — the image-paste ladder of §15.1. Taken: ctrl+v
  *and* ctrl+alt+v as two doors to one clipboard read; pasted-path
  normalization (`file://`, quotes, shell escapes, Windows paths mapped into
  WSL); the PowerShell bridge that reaches the Windows clipboard from a WSL
  session, including the file-copy case; an env escape hatch for terminals
  that mishandle enhanced key reporting. Taken later, once the same failure
  showed up on POSIX terminals and Windows joined the target list (D10): the
  idea of reconstructing a paste that arrived as a rapid stream of keypresses
  — though evo reads it off the poll batch rather than running codex's timer
  state machine (§15). Added beyond it: reading
  the empty bracketed paste as the cmd+v gesture, downscaling oversized
  images rather than failing at the provider, and images by value in the
  journal (D2) instead of paths that can go stale.
- **lisp-references/** (repo root) — Common Lisp and SBCL reference material
  for whoever is implementing evo, human or agent. If you lack the CL or SBCL
  knowledge for a task, read here before guessing. Browse it fresh each time;
  its contents vary by developer, so assume no particular structure.
