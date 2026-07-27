# evo-agent design

`evo-agent` is an agent who self-evolves.

This is the living design document. Decisions get recorded in §2 as we make them; everything else
is current-best design and open to revision.

References:
- `~/Projects/pi` — pi-mono: agent loop, session tree, compaction, self-extension.
- `~/Projects/codex` — codex: goal system (`codex-rs/ext/goal/`).
- `lisp-references/` (repo root) — Common Lisp / SBCL reference material for
  whoever (human or agent) is implementing evo. **If you lack CL/SBCL knowledge
  for a task, read here before guessing.** Browse the folder fresh each time —
  its contents vary by developer, so don't assume any particular structure.

---

## 1. Vision & principles

- **Minimalist harness.** Core functionality of a real agent, nothing
  ceremonial. Pi proves the omit-list works: no permission popups, no MCP, no
  sub-agents. One interception point + tool-set control covers what a
  permission subsystem would. (One deliberate deviation from pi's omit-list:
  todo checklists — goal-length runs need visible progress; §11.)
- **Goal-oriented.** You define what "done" means; the agent decides how.
  Long-running tasks are the norm, not the exception.
- **Self-extending.** If it lacks a tool, it writes one, loads it into its own
  runtime, and keeps going. Common Lisp makes the load/redefine half of this
  nearly free; the design work goes into safety rails and the seed corpus.
- **Permissive by design.** No permission prompts. The boundary is the
  OS/container it runs in, plus a kernel/userspace split so the agent breaks
  itself deliberately, never accidentally.
- **Self-healing.** Crash, restart, resume the session, continue the goal.
  The supervisor + journal make death a recoverable event.

## 2. Decisions

| # | Decision | Rationale (short) |
|---|---|---|
| D1 | Sessions are an append-only entry **tree** in a journal file; state = fold over root→leaf path. Pi's model. | Branching/rewind/resume/pause fall out free; write-ahead = crash-safe. |
| D2 | **No image-based session persistence.** Journal is the only source of truth. Lisp images are build/packaging artifacts only. | Both target APIs are stateless (full replay) so transcript-as-data is mandatory anyway; images are opaque, undiffable, and propagate corruption. |
| D3 | **Journal format is native sexprs** (one form per line), and sexprs are the default for every data format we control (lore, goal state). Config is not data: init.lisp is evaluated Lisp (see D6). | Human-readable, `read`-able from Lisp with zero external serialization deps. |
| D4 | **Interface is a CLI** with an adaptive TUI (mandatory: adapts to console resize). Not Emacs/Swank-first. | Approachable for newcomers; CLI adapts to the most contexts. Swank remains a developer side-door, not the product. |
| D5 | **Supervisor architecture: yes.** A tiny outer process owns launch, crash detection, restart, resume. | Long-running goal pursuit requires surviving self-inflicted death. |
| D6 | Providers: **both adapters ship** (Anthropic Messages, OpenAI Responses) as kernel-curated CLOS *provider APIs*; **models and endpoint configs are user-registered from init.lisp** (config-as-code, no built-in model table, no settings.sexp). One unified message model for all APIs. | Historically Anthropic-only-then-OpenAI-post-v1 for scope control. The API/registry split keeps new wire protocols a kernel concern while models/providers stay config — still skips pi-ai's 40-provider sprawl. |
| D7 | Goal system follows **codex's design**: persisted goal + idle-continuation steering + explicit audited completion + budgets. Optional Lisp acceptance predicate as kernel-side verifier. | See §8. |
| D8 | Kernel/userspace split enforced with **package locks** (SBCL native, ECL `si:package-lock`, behind `evo.port`). | Permissive but not suicidal: touching the kernel requires an explicit, auditable unlock. |
| D9 | Tool execution is **sequential** in v1. | Parallel is where pi's thread-discipline complexity lives; revisit later. |
| D10 | **SBCL and ECL**, through a single portability layer (`evo.port` — the only package allowed to touch `sb-*`/`ext:`/`si:` symbols). On SBCL the launcher always passes an explicit `--dynamic-space-size` (settings-overridable, generous default e.g. 4096); ECL's heap grows on demand. | Originally SBCL-only; portability was added once the impl-specific surface proved small (env/argv/exit, processes, locks, fd streams, signals). Everything else is portable CL + uiop + the existing deps. SBCL's default heap is not sized for a long-running agent. |
| D11 | Naming locked: binary `evo`, dirs `~/.evo/` + `<project>/.evo/`, package prefix `EVO.`. | As assumed throughout this doc; settled to stop revisiting. |
| D12 | v1 TUI editor is a **plain multi-line text editor**: Enter sends, Shift+Enter inserts a newline, pastes >3 lines collapse to a placeholder (re-pasting the same content right after it expands it inline). No completions or highlighting yet. | Multi-line editing is crucial UX; editor sophistication beyond that isn't where the novelty is. |
| D13 | **Slim core; everything outside the core loop ships as a core extension** — bundled, built on the same extension API with the same level of control as user extensions; essential ones (tui) cannot be disabled. | Dogfooding proves the API depth; keeps the kernel small and honest. See §11. |
| D14 | **Todo checklists: yes**, shipped as a core extension. | Long-running goal work needs user-visible progress; the one deliberate deviation from pi's omit-list. |
| D15 | `:done-when` predicates are **agent-authored, not user-written**: named userspace functions journaled via `:load`, referenced by name in the `:goal` entry. | Users state objectives in prose; the agent formalizes them. Named+journaled functions survive restart; closures don't round-trip through sexprs. |
| D16 | **No sub-agents in v1.** Revisit after M5. | The journal-tree model extends naturally (child session = forked journal) when we want them. |
| D17 | **One binary total.** No shell launcher, no separate supervisor executable: the `evo` binary invoked plainly IS the supervisor parent (D5's "tiny outer process"), re-spawning itself (`evo.port:runtime-pathname`, `EVO_SUPERVISED_CHILD=1`, inherited stdio) as the session child. On SBCL the heap is baked at build time (`--dynamic-space-size` + `:save-runtime-options`), refining D10's launcher-passes-it rule. `--no-supervisor` runs the session in-process. | User decision. A wrapper script is one more artifact to install, breaks TTY inheritance under POSIX background rules, and buys nothing the binary can't do itself. |

## 3. Architecture overview

```
evo-supervisor (tiny: shell or ~100 lines of CL)
  │  spawn (explicit --dynamic-space-size) / monitor / restart / resume policy
  ▼
evo image (SBCL or ECL process)
├─ KERNEL  (locked packages: EVO.KERNEL, EVO.PROVIDER, EVO.JOURNAL, …)
│    turn loop            errors-as-data, steering queues, save points
│    journal              append-only sexpr entry tree + leaf pointer
│    provider APIs        anthropic-messages + openai-responses (CLOS protocol);
│                         model/provider registries fed by init.lisp through
│                         exported kernel functions (locks unaffected)
│    tool registry        register/activate/refresh, system-prompt rebuild
│    extension API        register-tool/-command, event hooks — both
│                         extension tiers build on this, nothing bypasses it
│    goal driver          idle-continuation loop, budgets, audits
│    compactor            usage-anchored, self-contained checkpoints
│    budget guard         per-goal + per-session hard stops
│    extension loader     load extension source, journal the load
├─ CORE EXTENSIONS  (bundled; same API & privileges as user extensions;
│                    shipped by us, loaded first, essential ones not disableable)
│    tui    adaptive renderer (scrollback + managed bottom region, SIGWINCH
│           reflow), multi-line editor, slash commands: /goal /lore /mode
│           /compact /tree /resume /todo …
│           optional Swank listener for the developer
│    todo   checklist tool + :custom state, rendered by the tui
├─ USERSPACE  (unlocked packages: EVO.USER, …)
│    all agent-written tools & code — persisted as source files
│    + journal load-entries; rebuilt from source on every boot
└─ RESOURCES
     skills/ (progressive disclosure)   prompts/ (templates)
     lore store (out-of-band, injected every turn)
     docs corpus (evo's own docs, paths named in system prompt)
```

Directory conventions (mirroring pi's `.pi`):
- Global: `~/.evo/` — `init.lisp`, `sessions/`, `extensions/`, `skills/`,
  `prompts/`, `lore.sexp`, `docs/` (the shipped corpus)
- Project: `<cwd>/.evo/` — `init.lisp`, `extensions/`, `skills/`, `prompts/`

## 4. The journal (sessions)

### 4.1 Model

Pi's session model, verbatim in structure (see research §2.2), sexpr in syntax:

- One file per session under `~/.evo/sessions/<encoded-cwd>/<timestamp>_<uuid>.sexp`.
- Line 1 is a header form; every subsequent line is one entry form with
  `:id` (short random), `:parent-id` (nil for a root), `:timestamp`.
- The file is a **tree**: branching = move the leaf pointer; next append
  becomes a sibling. Entries are never modified or deleted.
- Context, model, thinking level, active tools, goal state, session name —
  everything — is a **fold over the root→leaf path**. No mutable fields.
- Write-ahead: append the entry before acting on it. Nothing is written until
  the first assistant message exists (no abandoned-session litter).

### 4.2 Entry types

Adopted from pi (9 types) plus goal entries:

```
:message              payload is a message plist (in LLM context)
:model-change         provider + model id           (state fold)
:thinking-change      thinking level                (state fold)
:tools-change         active tool names             (state fold)
:compaction           summary + retained tail — self-contained checkpoint
:branch-summary       summary of an abandoned branch
:custom               extension/tool state, INVISIBLE to the LLM
:custom-message       extension-injected content, visible to the LLM
:label                bookmark on an entry (targetId + label)
:session-info         session name etc.
:goal                 goal created/updated: objective, status, budgets, usage
:load                 userspace source file loaded (path + reason)
```

The `:custom` vs `:custom-message` split (state vs context) is load-bearing —
it's how tools persist state without polluting the prompt.

`:compaction` entries carry the retained tail materialized on the entry
(pi-harness `retainedTail` style): context rebuild is
`[summary, ...retained-tail, ...entries-after]` — O(1), no walk past the
compaction.

`:load` entries are what makes runtime evolution replayable: boot = load
kernel, then replay the path's `:load` entries against the source files on
disk. A corrupted runtime is repaired by fixing/removing a source file, never
by surgery on opaque state.

### 4.3 Format rules

- One form per line; `write` with `*print-readably*`-compatible output.
- Read with `*read-eval*` **nil**, standard readtable, into a dedicated
  package. Journals are data, not code — even though they are sexprs.
- Value vocabulary is deliberately restricted to a "sexpr-JSON" subset:
  plists, keywords, strings, integers, ratios/floats, `t`/`nil`, and vectors.
  **No non-keyword symbols, no arbitrary objects, no cycles.** This keeps
  read/print round-tripping trivial and journals hand-editable.
- Example:

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

- `/resume` — list sessions (bounded header scan), reopen, rebuild context
  from leaf. Pause is free: stop the process; the journal *is* the state.
- `/tree` — navigate entries, move the leaf (rewind). Selecting a user message
  moves the leaf to its parent and puts the text in the editor
  (edit-and-resubmit → new branch). Optional branch summary on abandonment.
- `/fork` — copy root→entry path into a new file.
- Double-escape = one-keystroke rewind (pi's `doubleEscapeAction`).
- File-state undo is **not** built in; the canonical answer is a small
  extension using `git stash create` keyed by entry id (pi's
  `git-checkpoint.ts`, 53 lines).

## 5. Provider layer

One unified model, two shipped adapters (D6), one architecture:
**provider APIs** — kernel-curated CLOS classes (`src/provider/` module,
one file per API) implementing `endpoint-path` / `auth-headers` /
`build-request` / `parse-stream` / `thinking-param` / `perform-request` —
dispatched from `call-provider` via the model's `:api` tag. Everything
below is pi-validated (research §2.5); deltas from pi noted.

- **APIs are kernel, models are config.** A new wire protocol is a new
  file in `src/provider/` implementing the generics (registered at load
  time, not user-extensible — replay/caching correctness is curated).
  Models and provider endpoints come from init.lisp via ordered registries
  (`provider/registry.lisp`): `register-model` (re-register replaces in
  place), `register-provider` (field-wise merge; stock endpoints
  pre-seeded from each API's defaults, so an env API key alone works). No
  built-in model table; a missing/unknown model is a loud config error at
  CLI preflight (usage-error, exit 64 — never enters the supervisor
  restart loop).
- **The adapter contract** (written down in `provider/api.lisp`'s header):
  `parse-stream` returns `(:content :model :stopped-p :error-message
  :stop-reason :usage [:aborted-p])`; stop reasons
  `:stop :length :tool-use :error :aborted`; usage buckets
  `:input/:output/:cache-read/:cache-write` with `:input` excluding
  cached/cache-written tokens; events `:message-start :text-delta
  :thinking-delta` (`:tool-call-start` moved to the kernel: it fires from
  `run-tool-call` with the fully-parsed `:arguments`, which do not exist
  yet when a stream block opens). Runtime errors are data;
  config-resolution errors signal (caught by preflight).

- **Message model**: 4 content blocks (`:text`, `:thinking`, `:image`,
  `:tool-call`), 3 roles (`:user`, `:assistant`, `:tool-result` as a top-level
  role — history stays a flat list). Assistant messages self-identify:
  `:api :provider :model` triple. Token usage tracked per message.
- **Provider artifacts are a typed variant, not stringly-typed** (unlike pi):
  Anthropic thinking signature = base64 scalar (chunked `signature_delta`,
  must append); OpenAI = the whole reasoning item with `encrypted_content`.
- **Stateless replay** everywhere: full history each request; OpenAI
  `store: false`; no `previous_response_id`.
- **Handoff pass** run at request build: same-model thinking replays verbatim;
  cross-model → plain text or dropped; orphaned tool calls get synthetic error
  results; errored/aborted assistant turns elided; on any model switch drop
  OpenAI `fc_*` item ids (server validates `fc_*`↔`rs_*` same-response
  pairing).
- **Streaming**: hand-rolled SSE with one shared framing loop
  (`map-sse-events`: event/data accumulation, CR trim, abort flag) —
  APIs supply only per-event dispatchers; terminal-event guards — a
  stream ending without `message_stop` / terminal response event is an
  error and retryable. Tolerant partial-JSON parsing of tool arguments on
  every delta. `perform-request` has a default SSE-over-dexador method;
  a future non-SSE framing overrides it.
- **Errors are data**: the provider layer never signals into the loop;
  failures become an assistant message with `:stop-reason :error`/`:aborted`
  plus `:error-message`. Stop reasons normalized to
  `:stop :length :tool-use :error :aborted`; OpenAI's missing tool-use status
  inferred from content; unknown reasons are loud errors.
- **Retry**: three layers — in-request HTTP retry (honor `retry-after`, exp
  backoff + jitter, refuse silently-long server delays), error normalization,
  turn-level retry on finished error messages. Classify on **HTTP status +
  typed error codes**, not pi's regex-over-strings (that was 40-provider
  necessity).
- **Caching**: Anthropic — 4 breakpoints (system prompt, last tool def, last
  user message); OpenAI — `prompt_cache_key` = session id. The whole design
  protects the prompt-cache prefix (see progressive disclosure, §12).
- **Model registry**: user-registered plists (id, context window, max
  output, thinking flag) from init.lisp, registration-ordered for the
  /model picker. No models.dev pipeline, no dollar-cost table — token
  accounting only.
- CL stack: `dexador` (`:want-stream t`) + `cl+ssl`, explicit read timeouts;
  `com.inuoe.jzon` for the JSON wire; abort = cooperative flag + close the
  socket from another thread (no `interrupt-thread`).

## 6. Agent loop

Pi's loop (research §2.1) with its known weaknesses fixed:

- A **run** = many **turns**; a turn = one assistant message + its tool batch.
  Loop: poll steering → LLM call → execute tools → `turn-end` → save point →
  repeat while tool calls or queued messages remain; then poll follow-ups.
- **Errors as data at the provider boundary; conditions at the tool boundary**
  (tools signal; loop `handler-case`s and converts to an error tool-result).
  The transcript is always well-formed.
- **Save point between turns** (`prepare-next-turn`): the whole context
  snapshot — system prompt, tools, model, messages rebuilt from the journal —
  is wholesale replaced between turns, never mutated mid-request. This is
  where compaction, tool changes, and model switches take effect.
- **Steering queues** (steer / follow-up / next-turn): polled at turn
  boundaries only; never preempts a tool batch. `/lore` and `/goal` mid-run
  ride this.
- **Truncation guard**: `:stop-reason :length` → tool calls are NOT executed
  (salvaged JSON can validate yet be incomplete); each gets an error result
  asking the model to re-issue.
- Fixes over pi: every event carries a **run id + monotonic turn index**;
  abort is checked in the loop predicate (no wasted post-abort provider
  call); one transcript, owned by the journal — the loop context is always a
  derived snapshot (pi-harness model, skipping pi's L2 dual-copy smell).
- **Run-until-settled is first-class kernel code** (pi buried it in app code):
  outer driver = run → post-run check (retryable error? compaction needed?
  queued messages? active goal?) → continue. The goal driver (§8) plugs in
  here.
- Tool interface: name, description, sexpr schema (emits JSON Schema),
  `execute` function, `:content` (model-visible) vs `:details` (host-visible)
  in the result. Sequential execution (D9).

## 7. Context management

Pi's design nearly verbatim (research §2.3–2.4):

- **Projection pipeline**: journal entries → agent messages →
  (transform-context) → (convert-to-llm) → provider messages. Runs once per
  turn; output never written back. In CL: a generic function
  `entry->llm-messages` with methods per entry type, `nil` to elide.
- **Compaction**:
  - Trigger: `context-tokens > context-window - reserve` (defaults: reserve
    16k, keep-recent 20k) — plus overflow-error recovery (compact + retry
    once) and manual `/compact`.
  - Token accounting anchored on the last valid provider-reported usage;
    estimate only the tail (chars/4, images flat ~4800).
  - Cut points never at a tool result; split-turn handling when one turn
    exceeds the keep budget.
  - Structured summary prompt (Goal / Constraints / Progress / Key Decisions
    / Next Steps / Critical Context; "preserve exact paths, names, errors")
    and a separate **iterative UPDATE prompt** fed the previous summary.
  - Deterministic facts alongside prose: read/modified file sets accumulate
    across compactions.
  - Summarization calls: fresh session id, no cache writes.
  - Result: a `:compaction` entry with retained tail — a self-contained
    checkpoint.
- Compaction summaries reach the model as ordinary user messages in
  `<summary>` tags. No provider features needed.

## 8. Goal system (`/goal`)

Codex-derived (D7). Source: `~/Projects/codex/codex-rs/ext/goal/`.

### 8.1 Model

A goal is journal state (`:goal` entries; current goal = fold):

```lisp
(:goal-id "g-01" :objective "..." :status :active
 :token-budget 500000 :tokens-used 123456 :time-used-seconds 840
 :done-when nil)          ; optional: name of an agent-authored userspace predicate (D15)
```

Statuses: `:active :paused :blocked :budget-limited :complete`.
Status transitions the *model* may make: → `:complete`, → `:blocked` (via the
`update-goal` tool, under audit rules). Pause/resume/budget transitions belong
to the user/system only.

### 8.2 Driver

- `/goal <objective>` creates or refines the goal (a new `:goal` entry;
  refinement injects an "objective updated" steering message if a run is
  active — codex's `objective_updated` template).
- **Idle continuation**: whenever the agent goes idle and the goal is
  `:active`, the driver starts a new turn seeded with a **continuation
  steering prompt** containing: the objective (as untrusted data), budget
  numbers (used/budget/remaining), anti-scope-shrinking fidelity rules, a
  **completion audit** (completion must be proven from current evidence —
  files, test output, runtime behavior — requirement by requirement; not from
  memory or intent), and a **blocked audit** (declare `:blocked` only after
  the same blocker recurs 3 consecutive goal turns).
- "Doing nothing" is NOT completion — an idle active goal always gets
  re-steered. Termination is explicit: the model calls
  `update-goal :status :complete` (or `:blocked`), or a budget/usage limit
  trips, or the user pauses.
- **Budget accounting** every turn (tokens + wall time). Budget exhausted →
  status `:budget-limited`, next steering is the wrap-up template (summarize
  progress, remaining work, next step — start nothing new). This doubles as
  the kernel's runaway-cost brake; a session-level budget exists too.
- Turn error → goal `:blocked` (codex behavior) — and this is the supervisor
  hook: on restart, a goal blocked by `turn-error` (not by model decision) is
  eligible for auto-resume (§13).

### 8.3 Model-facing tools

`get-goal`, `create-goal` (only when explicitly asked; refuses if an
unfinished goal exists), `update-goal` (status only: complete/blocked, with
the audit language in the tool description). Codex's tool descriptions are
good; adapt them.

### 8.4 The Lisp addition: verified completion

`:done-when` is designed for the **agent to fill, not the user** (D15). Users
state objectives in prose; when the objective is mechanically checkable, the
agent formalizes it — writes a *named* predicate function into userspace
(e.g. `(defun goal-done-p () (zerop (sh "./test.sh")))` in a source file,
journaled via `:load`, so it survives restart — closures don't round-trip
through sexprs) and references it by name in the `:goal` entry. The goal
creation flow steers the agent to derive the predicate from the objective up
front, at goal start, not at completion time.

Then `update-goal :complete` is not taken at its word: the kernel **runs the
predicate**. Failure → the tool call returns an error carrying the
predicate's output, and the goal stays `:active`. The model's completion
claim becomes a checked assertion it wrote against itself. A lazy
`(defun goal-done-p () t)` is possible in principle; the mitigation is that
the predicate is a journaled, user-visible artifact written before the work,
when the model has no victory to declare yet. Optional — `/goal` works fine
without it — but it closes the premature-victory hole with ~20 lines of
kernel code.

## 9. Lore system (`/lore`)

Human knowledge, guidance, and constraints, durable across the whole session
and immune to summarization:

- `/lore <text>` appends to an out-of-band lore store (sexpr file per scope:
  global `~/.evo/lore.sexp`, project `.evo/lore.sexp`, plus session-scoped
  entries in the journal).
- Lore is injected into the system-prompt region **every turn** — never
  entrusted to the compactor's summarizer. (Pi's nearest analog is AGENTS.md +
  steering; lore is first-class here.)
- Mid-run `/lore` rides the steering queue: acknowledged at the next turn
  boundary, durable thereafter.
- Context files (`AGENTS.md`, `CLAUDE.md`) are also loaded pi-style (walk
  root→cwd, nearest last) — lore complements, not replaces, repo conventions.

## 10. Self-extension

The four requirements (pi's anatomy, research §2.6), in CL:

1. **Loader**: `evo:load-extension <path>` — `compile-file` + `load` into a
   userspace package; journal a `:load` entry. CL redefinition semantics mean
   a tool can load code from within its own execution — the new definition
   applies from the next call; no trampoline needed (pi needs a queued
   `/reload` command because jiti reloads the whole module graph; we don't).
2. **Registration API**: `(evo:register-tool ...)`, `(evo:register-command ...)`,
   `(evo:on <event> fn)` — mutations refresh the tool registry and rebuild the
   system prompt; a newly registered tool is callable on the next request.
   Interception: a `tool-call` hook that can mutate args or
   `(:block t :reason ...)` — the one point permission gates / plan mode /
   sandboxing build on.
3. **Filesystem convention**: `~/.evo/extensions/`, `<project>/.evo/extensions/`
   — loaded at boot (and journaled), writable by the agent.
4. **Docs as part of the runtime**: the system prompt names absolute paths to
   evo's own docs and examples. Additionally, CL introspection (`describe`,
   `apropos`, `macroexpand`) lets the agent interrogate the *running* runtime.
   **The seed corpus is a real deliverable**: extension-API docs plus a set of
   exemplary hand-written extensions (git-checkpoint and permission gate are
   the pi-proven starter set; plan mode is the same shape, promoted into the
   core because a read-only mode has to be there before the agent is trusted
   to install it). Expect models to write worse CL than
   TS — the corpus and the condition-system error feedback are the
   mitigations.

Safety rails:
- **Package locks (D8)**: kernel packages locked via `evo.port:lock-package`;
  userspace unlocked. The agent *can* unlock the kernel — permissive — but
  only as an explicit, journaled, deliberate act.
- Reload discipline: redefinition affects the next call, not running frames
  (same footgun as pi's reload; document it in the corpus).
- Extension in-memory state does not survive restart; extensions rebuild it
  from `:custom` journal entries on `session-start` (pi's discipline).

## 11. Core extensions (slim-core discipline)

The kernel owns the core loop and nothing else (D13). Everything outside it —
**including the TUI** — ships as a *core extension*: bundled with evo, written
against the same extension API (§10) with the same level of control as user
extensions. Only three things distinguish them: we ship them, they load first,
and the essential ones (tui) cannot be disabled.

Why this discipline: dogfooding. If the TUI and the todo list can be built on
`register-tool` / `register-command` / event hooks / `:custom` entries, the
API is proven deep enough for the agent's own self-extensions. Anything a core
extension needs but can't get through the public API is an API gap to fix,
never a private kernel hook to add.

Mechanically, core extensions are compiled into the image at build time (they
are part of the ship, not runtime loads) but register through the same API.
The runtime loader (§10) is for user/agent extensions only. Sequencing
consequence: the registration/event API must exist by M2, because the TUI
needs it (§15).

### 11.1 Todo lists (D14)

Long-running goal work needs a user-visible checklist — pi omits to-dos, but
pi's sessions are interactive; here multi-hour goal runs are the norm, and the
user must be able to see structure and progress at a glance.

- A `todo` tool: the model replaces the whole list per call (items = text +
  `:pending` / `:in-progress` / `:done`). Whole-list replacement keeps the
  schema and the state fold trivial.
- State rides `:custom` entries (invisible to the LLM as entries — the tool
  call/result already put it in context when it mattered), so the current
  list = fold over the path; it survives restart and is untouched by
  compaction.
- The tui renders the current list in the managed bottom region; `/todo`
  toggles the panel; print/event modes emit it as events.
- Goal-driver tie-in: the continuation steering prompt embeds the current
  todo snapshot, so a re-steered run after crash or compaction knows where
  it left off.

## 12. Skills, prompt templates, slash commands, modes

- **Skills**: Agent Skills standard (SKILL.md + frontmatter), progressive
  disclosure — only name/description/path in the prompt inside
  `<available_skills>`; the model reads the file on demand. `/skill:name`
  forces one. Markdown stays the format here (external standard).
- **Prompt templates**: `.md` files, filename = command, `$1`/`$@`-style
  substitution, purely textual expansion.
- **Slash command resolution** (pi's order): extension commands → input hook →
  skills → templates → send to agent. Built-ins: `/goal /lore /mode
  /compact /tree /fork /resume /model /reload /export /help /quit /exit`.
- **Modes** (`src/plan-mode.lisp`, package `EVO.PLAN`): `auto` (default:
  fully permissive) and `plan`. The mode is journal state (`:custom "mode"`),
  never a flag, so it survives restart and every frontend reads the same
  value. Switching applies policy through the public API — tool gating
  (`set-active-tools` down to `*plan-tools*`), instructions injected as a
  keyed `:custom-message` — and two hooks enforce it: a `:tool-call` gate
  (allowlist, not blocklist: a tool absent from `*plan-tools*` is blocked,
  and bash is scanned quote-aware so every chained segment must have an
  allowlisted head, with command substitution and output redirection out)
  and a `:transform-context` filter that removes the injected instructions
  from the projection once the mode is off. The TUI is presentation only:
  shift+tab toggle, `/mode` choose box built from `EVO.PLAN:*MODES*`,
  status-line indicator.
- **System prompt assembly** (pi's order): base → tool one-liners (opt-in) →
  guidelines → own-docs paths → lore → project context files → skills → cwd.
  Rebuilt on any tool-set change.

## 13. Supervisor & self-healing

The supervisor is deliberately dumb (shell script or ~100 lines of CL):

1. Spawn the agent process (on SBCL with an explicit `--dynamic-space-size`
   from settings (D10) — the default heap is not sized for a long-running
   agent — loading kernel + core extensions, replaying the session's
   `:load` entries, resuming the journal leaf).
2. Monitor: process exit + a heartbeat file the kernel touches on every event
   (configurable hang timeout, generous default — tool calls can be long).
3. On crash: restart with `--resume <session>`. If the resumed session has a
   goal that is `:active`, or `:blocked` with reason `turn-error`, the goal
   driver's idle-continuation picks it up automatically — crash → reboot →
   re-steer toward the goal, no human needed.
4. **Boot-failure quarantine**: if boot fails N times, retry with
   `--no-userspace` (kernel + core extensions only) and report which `:load` entry was reached —
   the journal makes the culprit bisectable. The agent (or the user) then
   fixes the offending source file. This is the answer to "the agent bricked
   itself": recovery is editing a source file, never surgery on opaque state.
5. Write-ahead journaling means a crash mid-turn loses at most the in-flight
   provider stream; the transcript up to it is on disk.

## 14. Interface (CLI / TUI)

Requirements: newcomer-friendly CLI; **adaptive to console size, including
live resize — mandatory** (D4). The tui is itself a core extension (§11):
built entirely on the public extension API, shipped by us, not disableable.

Approach (pi-tui is the reference design, `~/Projects/pi/packages/tui`):
- Render into normal terminal scrollback (no ncurses alternate screen —
  scrollback history is part of the UX) + a managed bottom region (editor,
  status line, goal/budget indicator) repainted differentially.
- Resize: SIGWINCH (via `evo.port` signal handling) → re-query size (`TIOCGWINSZ`)
  → reflow the managed region; long content wraps, wide content truncates
  with indicators.
- Raw mode + ANSI escapes directly via a thin CFFI termios layer — avoiding a
  curses dependency keeps the binary self-contained and the renderer
  debuggable. (This is a real chunk of work — pi's TUI is a whole package —
  but a modest subset suffices: no windowing, no widgets beyond the editor +
  list-select + confirm.)
- **Editor** (D12): a pure text editor for now — no completions, no
  highlighting — but genuinely **multi-line**, because multi-line editing is
  crucial UX:
  - normal text input; the editor region grows and reflows with content
    (it's part of the managed bottom region);
  - **Enter sends**; **Shift+Enter inserts a newline**;
  - **paste collapse**: a paste of more than 3 lines becomes a placeholder
    token (e.g. `[paste #1: 42 lines]`); the content lives in a side buffer
    and is substituted back in full when the message is sent;
  - **paste-to-expand**: pasting the *exact same content* again with the
    cursor right after the placeholder replaces it with the real lines,
    editable in place — paste once to keep it compact, paste twice to edit.
  - Implementation notes: paste detection requires bracketed paste mode
    (`CSI ?2004h`). Shift+Enter is indistinguishable from Enter in legacy
    terminals; use the kitty keyboard protocol / `modifyOtherKeys` (CSI-u)
    where the terminal supports it, with a documented fallback (Alt+Enter
    or `\`-then-Enter) elsewhere.
- Non-interactive modes from day one: `evo -p "prompt"` (print mode) and an
  event-stream mode (line-delimited sexprs on stdout) — they're nearly free,
  make evo scriptable, and give the supervisor/tests a UI-less harness.
- Streaming rendering driven by the loop's event protocol (deltas + partial
  accumulator), same as pi.
- Optional `--swank <port>` for developers: the live-image inspection
  side-door, off by default.

## 15. Milestones

- **M0 — provider core**: unified message model, Anthropic adapter (SSE,
  thinking, caching, retries), model table. Exit: streamed
  tool-call round-trip from a REPL.
- **M1 — kernel loop + journal**: turn loop, sequential tools
  (read/write/edit/bash), journal tree + resume, save points, run-until-
  settled driver, registration/event API (core extensions build on it in
  M2). Exit: multi-turn task in print mode, kill -9 mid-task, resume
  cleanly.
- **M2 — core extensions: CLI/TUI + todo**: adaptive renderer, multi-line
  editor (D12: Enter/Shift+Enter, paste placeholders), slash commands,
  skills + prompt templates, `/tree` `/resume` `/fork`, todo extension —
  all built on the M1 extension API.
- **M3 — context management**: compaction (threshold/overflow/manual),
  branch summaries, lore system.
- **M4 — goals + supervisor**: goal driver (continuation steering, budgets,
  audits, `:done-when`), supervisor with crash-resume + quarantine. Exit:
  overnight goal run that survives an induced crash and a context overflow.
- **M5 — self-extension**: runtime loader (`load-extension` + `:load`
  replay), package-lock rails, seed corpus (docs + example extensions incl.
  plan mode). Exit: evo writes, loads, and uses a novel tool to satisfy a
  goal.

(The OpenAI Responses adapter is out of the milestones entirely — post-v1,
D6. The unified message model keeps the door open from M0.)

## 16. Open questions

All five questions from the previous revision are resolved into decisions:
TUI depth → D12, naming → D11, CL policy → D10, `:done-when` ergonomics →
D15, sub-agents → D16. New questions get added here as they come up.
