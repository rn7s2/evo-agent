# evo-agent

`evo` is an agent who self-evolves. See [design.md](design.md) for the full
design; this README covers what is implemented and how to run it.

## Status: v1 scope implemented

- **Provider core** — unified message model; Anthropic Messages adapter
  with hand-rolled SSE streaming, thinking blocks (chunked signatures),
  handoff pass (errored turns elided, cross-model thinking dropped, orphaned
  tool calls given synthetic results), errors-as-data, status-classified
  retries with backoff/retry-after, prompt-cache breakpoints; per-message
  token accounting.
- **Kernel loop + journal** — append-only sexpr entry **tree** per
  session (write-ahead; `*read-eval*` nil; restricted sexpr-JSON vocabulary;
  form-based reading); all state a fold over the root→leaf path; turn loop
  with steering queues, save points, truncation guard; sequential
  read/write/edit/bash tools; run-until-settled driver; kill -9 mid-task
  resumes cleanly.
- **TUI + core extensions** — adaptive renderer in normal scrollback +
  managed bottom region, SIGWINCH live reflow; multi-line editor (Enter sends,
  Shift+Enter newline, paste >3 lines collapses to a
  placeholder, paste-to-expand); slash commands with the resolution
  order (extension commands → builtins → skills → prompt templates → agent);
  `/tree` `/resume` `/fork`, double-escape rewind, ESC aborts; todo
  checklist core extension rendered in the panel; plan/auto modes
  (shift+tab, `/mode`) as a core extension — journaled mode state, gated
  tool set, injected instructions filtered back out of context, and a
  read-only `:tool-call` gate that judges every chained bash segment;
  skills (Agent Skills standard, progressive disclosure) and `$1`/`$@`
  prompt templates.
- **Context management** — compaction (threshold at save points,
  manual `/compact`, overflow-error compact+retry-once), usage-anchored
  token accounting, cut points never at a tool result, structured + iterative
  UPDATE summary prompts, accumulated file sets, `:compaction` entries with
  materialized retained tails; lore (`/lore`, global/project/session scopes)
  injected into the system prompt every turn.
- **Goals + supervisor** — persisted `:goal` entries; idle-continuation
  steering with budgets, anti-scope-shrinking fidelity rules, completion and
  blocked audits; kernel-verified agent-authored `done_when` predicates;
  built-in supervision (see below) with heartbeat hang detection,
  crash-restart-resume, and boot-failure quarantine (`--no-userspace`).
- **Self-extension** — `load_extension` tool: the agent writes Lisp
  into `EVO.USER`, loads it into its own runtime, journaled as `:load` and
  replayed on resume; package locks on the kernel; seed corpus:
  [docs/](docs/) (extension API, journal format, self-extension guide) and
  example extensions — git-checkpoint and permission-gate
  ([extensions/examples/](extensions/examples/)); nothing ships active in
  `~/.evo/extensions`, and the bundled extensions ([todo](src/core-ext/todo.lisp),
  [plan mode](src/core-ext/plan-mode.lisp)) are built on the same public API rather
  than reaching past it.

- **Two provider adapters** on one unified message model — Anthropic
  Messages and OpenAI Responses (stateless replay with `store: false`,
  encrypted reasoning round-trip, `prompt_cache_key` = session id).

Post-v1 by design: sub-agents.

## One binary

`make build` produces a single executable, `build/evo`. Invoked plainly it
is its own supervisor: the parent process re-spawns the same binary as a
supervised child (inherited stdio, so the TUI just works), monitors a
heartbeat file, restarts with `--resume` after crashes and hangs, and
quarantines repeated boot failures with `--no-userspace`. Exit codes:
0 done · 1 error · 2 goal blocked · 3 budget-limited · 64 usage error.
`--no-supervisor` (or `EVO_NO_SUPERVISOR=1`) runs the session in-process.

```sh
make build                # requires SBCL + Quicklisp (or: make build LISP=ecl)
make install              # copies to /usr/local/bin/evo (PREFIX=…)
make install-home         # seeds ~/.evo with docs + example extensions

evo                       # interactive TUI (on a tty)
evo -p "run ls and summarize"          # print mode: text on stdout
evo --events -p "..."                  # line-delimited sexpr events
evo --goal "make ./test.sh pass"       # goal run; survives its own death
evo --resume                           # reopen the latest session here
evo --list-sessions
```

## Configuration

Config is code: global `~/.evo/init.lisp`, then project
`<cwd>/.evo/init.lisp`, evaluated in that order on every boot — an
override is just a later call. `EVO_HOME` overrides `~/.evo`.
`--no-userspace` skips config and extensions.

evo ships **no built-in model table**: at minimum register one model and
pick it. Without that, evo exits with a pointer to the sample at
[docs/examples/init.lisp](docs/examples/init.lisp).

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

Two dialects ship bundled (`:anthropic-messages`, `:openai-responses`);
models and providers are yours, and so is the protocol — subclass
`evo:provider-api`, implement the generics, `evo:register-api` it, and a
model can name it via `:api` ([docs/extension-api.md](docs/extension-api.md)).
Config runs in userspace with the full extension API, so `register-tool`,
hooks, and `load-extension` work here too.

## Tests

```sh
make test           # unit: sexpr IO, journal, schema, registries, provider
                    #       APIs, SSE + transport, request builders, handoff,
                    #       init files, preflight, editor, input parser,
                    #       templates, compaction, lore, plan-mode
make integration    # live (needs the proxy): tool round-trip, kill -9 +
                    #       manual resume, goal completion, induced-crash
                    #       supervision, mid-task compaction
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

src/tui/                 EVO.TUI — also a core extension (§11), essential so
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
