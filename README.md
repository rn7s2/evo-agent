# evo

`evo` is a goal-oriented, self-evolving software agent written in Common
Lisp. It runs in your terminal, pursues objectives you define, and — when it
lacks a capability — writes the missing tool into its own running image and
keeps going.

Five properties define the system:

- **Goal-oriented.** You state what *done* means; evo decides how. Long-running,
  unattended pursuit is the normal case, not an edge case.
- **Self-extending.** A missing tool is never a blocker. Evo writes Lisp, loads
  it into its own runtime, and continues — no restart, no recompile.
- **Permissive.** No permission prompts. The trust boundary is the OS or
  container evo runs in, plus a kernel/userspace split that lets the agent
  break itself *deliberately* but not *accidentally*.
- **Self-healing.** Crash, restart, resume, continue the goal. The supervisor
  and the journal together make process death a recoverable event.
- **Minimal.** Real agent functionality and nothing ceremonial. Every omission
  (no MCP, no sub-agents, no permission popups) is deliberate, with a stated
  re-entry condition.

See [design.md](design.md) for the full architecture — invariants, layers,
decision record, and provenance.

## Quick start

```sh
make build                # requires SBCL + Quicklisp (or: make build LISP=ecl)
make install              # copies to /usr/local/bin/evo (PREFIX=…)
make install-home         # seeds ~/.evo with docs + example extensions
```

evo ships **no built-in model table**. At minimum, register one model and pick
it — copy the sample at [docs/examples/init.lisp](docs/examples/init.lisp) to
`~/.evo/init.lisp` and edit:

```lisp
(evo:register-model "claude-sonnet-5"
  :provider :anthropic :api :anthropic-messages
  :context-window 200000 :max-output 64000 :thinking t)
(evo:set-setting :model "claude-sonnet-5")
```

Without that, evo exits with a pointer to the sample.

## Usage

```sh
evo                                     # interactive TUI (on a tty)
evo -p "run ls and summarize"           # print mode: text on stdout
evo --events -p "..."                  # line-delimited sexpr events
evo --goal "make ./test.sh pass"       # goal run; survives its own death
evo --resume                           # reopen the latest session here
evo --list-sessions
```

Invoked plainly, `evo` is its own supervisor: the parent process re-spawns the
same binary as a supervised child (inherited stdio, so the TUI just works),
monitors a heartbeat file, restarts with `--resume` after crashes and hangs,
and quarantines repeated boot failures with `--no-userspace`. Exit codes:
`0` done · `1` error · `2` goal blocked · `3` budget-limited · `64` usage error.

`--no-supervisor` (or `EVO_NO_SUPERVISOR=1`) runs the session in-process.

## What's inside

### Provider layer

One unified message model, two bundled wire-protocol adapters, one extension
point.

- **Adapters**: Anthropic Messages and OpenAI Responses — both stateless replay
  (`store: false`, no `previous_response_id`), with prompt-cache breakpoints
  tuned per provider.
- **Provider APIs are a protocol, not a kernel privilege.** A wire protocol is
  a CLOS class implementing `endpoint-path`, `auth-headers`, `build-request`,
  `parse-stream`, `thinking-param`, and `perform-request`. The bundled
  protocols live in `src/provider/`; an extension registers its own through the
  same public `EVO` surface. See [docs/extension-api.md](docs/extension-api.md).
- **Models and endpoints are configuration**, fed from `init.lisp` through
  ordered registries. There is no 40-provider table. Stock endpoints
  (`:anthropic`, `:openai`) are pre-seeded from env vars, so an API key alone
  suffices.
- **Hand-rolled SSE streaming** over one shared framing loop; per-API dispatch.
  Terminal-event guards, tolerant partial-JSON tool-argument parsing, and
  cooperative abort (flag + socket close, never `interrupt-thread`).
- **Handoff pass** at request build: same-model thinking replays verbatim;
  cross-model thinking degrades gracefully; orphaned tool calls get synthetic
  error results; errored and aborted turns are elided.
- **Retry** in three layers: in-request HTTP retry with `retry-after` and
  backoff, error normalization on typed codes (not regex), and turn-level retry
  on finished error messages.
- **Per-message token accounting** — `:input`, `:output`, `:cache-read`,
  `:cache-write`. No cost tables; prices go stale.

### Kernel loop + journal

- **The journal is the only source of truth.** Session state lives in an
  append-only sexpr entry **tree** — one file per session, one form per line,
  read with `*read-eval*` nil over a restricted value vocabulary. No Lisp image
  carries session state.
- **State is a fold, never a mutable field.** Context, model, thinking level,
  active tools, goal, todo list, mode — all derived by folding the root→leaf
  path. Branching, rewind, resume, and pause fall out of the data structure.
- **Write-ahead**: the entry is appended before it is acted on. A crash mid-turn
  loses at most the in-flight provider stream.
- **Turn loop** with steering queues (polled at turn boundaries, never
  preempting a tool batch), save points (the context snapshot is rebuilt
  wholesale from the journal every turn), and a truncation guard (on
  `:stop-reason :length`, tool calls are not executed — each gets an error
  result asking the model to re-issue).
- **`run-until-settled`** is kernel code: an outer driver that asks whether the
  error is retryable, whether compaction is needed, whether messages are queued,
  whether a goal is active — and continues.

### Context management

- **Compaction** triggered by token-threshold at save points, overflow-error
  recovery (compact + retry once), or manual `/compact`. Token accounting is
  anchored on the last valid provider-reported usage; only the tail is
  estimated. Cut points are never at a tool result.
- **Structured summary prompts** (Goal, Constraints, Progress, Key Decisions,
  Next Steps, Critical Context) with a separate iterative UPDATE prompt.
  Deterministic facts — read and modified file sets — travel alongside the
  prose. The result is a `:compaction` entry carrying its retained tail: a
  self-contained checkpoint, so rebuilding context is O(1).
- **Lore** (`/lore`): human knowledge and constraints, durable across a whole
  session and immune to summarization. Stored out-of-band and injected into the
  system prompt every turn. Global (`~/.evo/lore.sexp`), project
  (`.evo/lore.sexp`), and session scopes.
- **Memory** (`/memory`, `/global-memory`): curated structured memory with kinds
  (constraint, convention, decision, procedure, fact, issue), snapshotted once
  per session into the transcript. The agent queries and refines the store
  through `project_memory` and `global_memory` tools.

### Goals + supervisor

- **Goals are journal state.** `/goal <objective>` creates a persisted goal;
  the current goal is a fold over `:goal` entries. Statuses: `:active`,
  `:paused`, `:blocked`, `:budget-limited`, `:complete`.
- **Idle continuation**: whenever the agent goes idle with an active goal, the
  driver opens a new turn seeded with a continuation prompt carrying the
  objective, budget numbers, anti-scope-shrinking fidelity rules, a completion
  audit (prove it from current evidence, requirement by requirement), and a
  blocked audit (declare `:blocked` only after the same blocker recurs across
  three consecutive goal turns). Doing nothing is never completion.
- **Budgets** run every turn over tokens and wall time. Exhaustion moves the
  goal to `:budget-limited` and the next steering is a wrap-up template. A
  session-level budget exists too.
- **Verified completion**: when an objective is mechanically checkable, the
  agent writes a named zero-argument predicate into a userspace file, journals
  it via `:load`, and references it by name on the `:goal` entry. On
  `update_goal :complete`, the kernel runs the predicate — failure returns an
  error and the goal stays active. The model's completion claim becomes a
  checked assertion it wrote against itself.
- **Supervisor**: the `evo` binary invoked plainly *is* the supervisor parent.
  It re-spawns itself as the session child, monitors process exit and a
  heartbeat file, restarts with `--resume`, and on repeated boot failures
  retries with `--no-userspace` (kernel and core extensions only), reporting
  which `:load` entry was reached — making the culprit bisectable. A goal
  blocked by `turn-error` is eligible for automatic resumption on restart.

### Self-extension

This is the evolution engine. Four mechanisms make it work:

1. **Loader.** `evo:load-extension <path>` compiles and loads a file into
   userspace and journals a `:load` entry. CL redefinition semantics mean a
   tool can load code from inside its own execution; the new definition applies
   from the next call, so no trampoline or queued reload is needed.
2. **Registration API.** `(evo:register-tool ...)`, `(evo:register-command
   ...)`, and `(evo:on <event> fn)`. Mutations refresh the tool registry and
   rebuild the system prompt, so a newly registered tool is callable on the next
   request. The `:tool-call` hook may mutate arguments or return
   `(:block t :reason ...)` — the single interception point that permission
   gates, plan mode, and sandboxing all build on.
3. **Filesystem convention.** `~/.evo/extensions/` and
   `<project>/.evo/extensions/` load at boot and are writable by the agent.
4. **Docs as part of the runtime.** The system prompt names absolute paths to
   evo's own documentation and worked examples. CL introspection (`describe`,
   `apropos`, `macroexpand`) lets the agent interrogate the runtime it is
   executing inside.

**Safety rails:**

- **Package locks** — kernel packages are locked; userspace (`EVO.USER`) is
  unlocked. The agent *can* unlock the kernel — the system is permissive, not
  childproof — but only as an explicit, journaled, deliberate act.
- **Recovery is editing a source file.** Runtime evolution replays from `:load`
  entries against source on disk. A runtime the agent broke is repaired by
  fixing or removing a file, never by surgery on opaque state.

Seed corpus: [docs/](docs/) (extension API, journal format, self-extension
guide) and example extensions — git-checkpoint and permission-gate
([extensions/examples/](extensions/examples/)).

### Core extensions

The kernel owns the core loop and nothing else. Everything outside it —
**including the TUI** — is a core extension: bundled, written against the same
API as user extensions, holding the same privileges. Three things distinguish
core extensions from user ones: they ship with the binary, they load first, and
the essential ones cannot be disabled.

- **TUI** — adaptive renderer in normal scrollback + managed bottom region,
  SIGWINCH live reflow, multi-line editor (Enter sends, Shift+Enter newline,
  paste >3 lines collapses to a placeholder, paste-to-expand), slash commands,
  streaming rendering, `--swank` developer side-door.
- **Todo** — checklist tool rendered in the panel; state rides `:custom`
  entries (invisible to the LLM), survives restart and compaction, embedded in
  goal continuation steering.
- **Plan mode** — `auto` (fully permissive default) and `plan` (read-only).
  Mode is journal state, not a flag. Switching applies policy through the public
  API: tool gating down to an allowlist, quote-aware bash segment scanning, and
  a `:transform-context` filter that removes injected instructions when the mode
  is off. The reference implementation of the extension API's depth.
- **Memory** — structured global/project memory, injected once per session.

### Skills, templates, slash commands

- **Skills**: the Agent Skills standard (SKILL.md + frontmatter) with
  progressive disclosure — only name, description, and path go into the prompt;
  the model reads the file on demand. `/skill:name` forces one.
- **Prompt templates**: `.md` files whose filename is the command, with `$1`..`$9`
  and `$@` substitution.
- **Slash command resolution**: extension commands → builtins → skills →
  prompt templates → send to the agent. Built-ins: `/goal /lore /memory
  /global-memory /permission /compact /eval /tree /fork /resume /model /reload
  /export /help /quit /exit`.
- **`/eval <sexpr>`**: a REPL into the live image. The content is read and
  evaluated in `EVO.USER` — the same package extensions and agent-written code
  live in — so registered tools, extension state and `evo:*agent*` are all
  reachable. Exactly one form: reading happens with `*read-eval*` off, so
  anything else (empty, unreadable, or several forms) is rejected with the
  reason before a thing runs, and several forms are pointed at `(progn ...)`.
  Printed output is captured, not written over the frame. Tab completes the
  image's own **functions and variables** — unqualified, `pkg:` exports,
  `pkg::` internals, keywords, and package names — each labelled with what it
  is; only what can actually be called or read is offered. A name that nothing
  else extends completes itself out of existence — no popup showing you your
  own word back.
- **Up/down input history** recalls everything submitted — ordinary text and
  every `/command`, whole or partial. Nothing is filtered out, because the
  popup is kept out of the way instead: a suggestion list is something new
  input asks for, never something recalled content arrives with. (A popup
  captures up/down, so one opening on a recalled entry would strand browsing
  there.) Edit a recalled line and it is new input again, suggestions and all;
  Tab asks for them outright whatever put the text there.

## Configuration

Config is code: global `~/.evo/init.lisp`, then project `<cwd>/.evo/init.lisp`,
evaluated in that order on every boot — an override is just a later call.
`EVO_HOME` overrides `~/.evo`. `--no-userspace` skips config and extensions.

```lisp
(evo:register-model "claude-sonnet-5"
  :provider :anthropic :api :anthropic-messages   ; :api = wire protocol
  :context-window 200000 :max-output 64000 :thinking t)
(evo:set-setting :model "claude-sonnet-5")

;; Optional (kernel defaults exist for all of these):
(evo:set-setting :thinking :medium)          ; off low medium high xhigh
(evo:set-setting :goal-token-budget 2000000) ; per-goal token cap; omit = no limit
;; :compact-reserve / :compact-keep-recent tune compaction.

;; Endpoints: :anthropic/:openai are pre-seeded (ANTHROPIC_API_KEY /
;; OPENAI_API_KEY); register-provider overrides field-wise, e.g. a proxy:
(evo:register-provider :anthropic :base-url "http://127.0.0.1:8787" :api-key "sk-...")
```

Two dialects ship bundled (`:anthropic-messages`, `:openai-responses`); models
and providers are yours, and so is the protocol — subclass `evo:provider-api`,
implement the generics, `evo:register-api` it, and a model can name it via
`:api`. Config runs in userspace with the full extension API, so
`register-tool`, hooks, and `load-extension` work here too.

## Tests

```sh
make test           # unit: sexpr IO, journal, schema, registries, provider
                    #       APIs, SSE + transport, request builders, handoff,
                    #       init files, preflight, editor, input parser,
                    #       templates, compaction, lore, plan-mode
make integration    # live e2e: tool round-trip, kill -9 + manual resume,
                    #       goal completion, induced-crash supervision,
                    #       mid-task compaction. Backend via env (skips if
                    #       unreachable): EVO_TEST_BASE_URL / _API_KEY / _MODEL
make tui-test       # expect-driven TUI under a pty
tests/plan-mode.exp # plan/auto mode wiring e2e
```

## Layout

Every directory under `src/` is one component owning exactly one package;
`evo.asd` lists them in load order, foundations first.

```text
src/packages.lisp        the whole package graph (kernel locked; EVO.USER open;
                         EVO = public API) — the one file shared by all components

src/port/                EVO.PORT — implementation portability layer, the only
  port.lisp              code allowed to touch sb-* / ext: / si: symbols
src/util/                EVO.UTIL
  util.lisp              safe sexpr IO, settings store, ids
src/journal/             EVO.JOURNAL
  journal.lisp           entry tree, write-ahead append, fold, fork, sessions

src/provider/            EVO.PROVIDER
  api.lisp               provider-API protocol (CLOS) + API registry
  registry.lisp          model + provider registries (populated from init.lisp)
  core.lisp              shared provider core: handoff, SSE transport, retries
  anthropic.lisp         Anthropic Messages API
  openai.lisp            OpenAI Responses API

src/kernel/              EVO.KERNEL — the core loop and nothing else
  tools.lisp             tool registry, sexpr schema -> JSON Schema
  prompt.lisp            system prompt assembly, skills, templates
  loop.lisp              agent, turn loop, run-until-settled, hooks, heartbeat
  lore.lisp              lore stores
  compact.lisp           compaction
  extension.lisp         load-extension, boot/replay, locks, EVO public API
  builtin-tools.lisp     read / write / edit / bash
  goal.lisp              goal driver, audited tools, done-when

src/core-ext/            core extensions: bundled, but built on the same public
  todo.lisp              API as user ones — EVO.TODO: todo checklists
  plan-mode.lisp         EVO.PLAN: plan/auto modes, policy + enforcement hooks
  memory.lisp            EVO.MEMORY: global/project memory stores

src/tui/                 EVO.TUI — also a core extension, essential so
  term.lisp              it cannot be disabled
  input.lisp
  editor.lisp
  render.lisp
  markdown.lisp
  tui.lisp
  commands.lisp

src/cli/                 EVO.CLI
  cli.lisp               arg parsing, print/event modes, session bring-up
  supervisor.lisp        in-binary supervision

docs/                    seed corpus (also installed to ~/.evo/docs)
extensions/examples/     reference-only example extensions (installed to
                         ~/.evo/docs/examples) — user extensions, distinct from
                         the bundled core extensions in src/core-ext/
```

## License

MIT.
