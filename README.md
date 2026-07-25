# evo-agent

`evo` is an agent who self-evolves. See [design.md](design.md) for the full
design; this README covers what is implemented and how to run it.

## Status: v1 scope implemented (M0–M5)

- **Provider core (M0)** — unified message model; Anthropic Messages adapter
  with hand-rolled SSE streaming, thinking blocks (chunked signatures),
  handoff pass (errored turns elided, cross-model thinking dropped, orphaned
  tool calls given synthetic results), errors-as-data, status-classified
  retries with backoff/retry-after, prompt-cache breakpoints; model table
  with rational cost accounting.
- **Kernel loop + journal (M1)** — append-only sexpr entry **tree** per
  session (write-ahead; `*read-eval*` nil; restricted sexpr-JSON vocabulary;
  form-based reading); all state a fold over the root→leaf path; turn loop
  with steering queues, save points, truncation guard; sequential
  read/write/edit/bash tools; run-until-settled driver; kill -9 mid-task
  resumes cleanly.
- **TUI + core extensions (M2)** — adaptive renderer in normal scrollback +
  managed bottom region, SIGWINCH live reflow; multi-line editor (D12:
  Enter sends, Shift+Enter newline, paste >3 lines collapses to a
  placeholder, paste-to-expand); slash commands with the §12 resolution
  order (extension commands → builtins → skills → prompt templates → agent);
  `/tree` `/resume` `/fork`, double-escape rewind, ESC aborts; todo
  checklist core extension (D14) rendered in the panel; skills (Agent
  Skills standard, progressive disclosure) and `$1`/`$@` prompt templates.
- **Context management (M3)** — compaction (threshold at save points,
  manual `/compact`, overflow-error compact+retry-once), usage-anchored
  token accounting, cut points never at a tool result, structured + iterative
  UPDATE summary prompts, accumulated file sets, `:compaction` entries with
  materialized retained tails; lore (`/lore`, global/project/session scopes)
  injected into the system prompt every turn.
- **Goals + supervisor (M4)** — persisted `:goal` entries; idle-continuation
  steering with budgets, anti-scope-shrinking fidelity rules, completion and
  blocked audits; kernel-verified agent-authored `done_when` predicates
  (§8.4); built-in supervision (see below) with heartbeat hang detection,
  crash-restart-resume, and boot-failure quarantine (`--no-userspace`).
- **Self-extension (M5)** — `load_extension` tool: the agent writes Lisp
  into `EVO.USER`, loads it into its own runtime, journaled as `:load` and
  replayed on resume; SBCL package locks on the kernel (D8); seed corpus:
  [docs/](docs/) (extension API, journal format, self-extension guide) and
  example extensions — [plan-mode](extensions/plan-mode.lisp) (shipped
  active: `/plan` `/auto` via tool gating + hidden `:custom-message` +
  transform-context filtering), git-checkpoint and permission-gate
  ([extensions/examples/](extensions/examples/)).

Post-v1 by design: OpenAI Responses adapter (D6), sub-agents (D16).

## One binary (D17)

`make build` produces a single executable, `build/evo`. Invoked plainly it
is its own supervisor: the parent process re-spawns the same binary as a
supervised child (inherited stdio, so the TUI just works), monitors a
heartbeat file, restarts with `--resume` after crashes and hangs, and
quarantines repeated boot failures with `--no-userspace`. Exit codes:
0 done · 1 error · 2 goal blocked · 3 budget-limited · 64 usage error.
`--no-supervisor` (or `EVO_NO_SUPERVISOR=1`) runs the session in-process.

```sh
make build                # requires SBCL + Quicklisp
make install              # copies to /usr/local/bin/evo (PREFIX=…)
make install-home         # seeds ~/.evo with docs + example extensions

evo                       # interactive TUI (on a tty)
evo -p "run ls and summarize"          # print mode: text on stdout
evo --events -p "..."                  # line-delimited sexpr events
evo --goal "make ./test.sh pass"       # goal run; survives its own death
evo --resume                           # reopen the latest session here
evo --list-sessions
```

## Settings

Sexpr plists (D3): global `~/.evo/settings.sexp`, project
`<cwd>/.evo/settings.sexp` (project wins). `EVO_HOME` overrides `~/.evo`.

```lisp
(:model "ark-deepseek-v4-pro"
 :thinking :xhigh               ; off low medium high xhigh
 :goal-token-budget 2000000
 :providers (:anthropic (:base-url "http://127.0.0.1:8787"
                         :api-key "sk-...")))
```

Against the real API omit `:base-url`/`:api-key` — `ANTHROPIC_API_KEY` is
the default source. `:compact-reserve` / `:compact-keep-recent` tune
compaction.

## Tests

```sh
make test           # unit: sexpr IO, journal, schema, SSE, handoff, editor,
                    #       input parser, templates, compaction, lore, plan-mode
make integration    # live (needs the proxy): M0/M1 round-trip, kill -9 +
                    #       manual resume, goal completion, induced-crash
                    #       supervision, mid-task compaction
make tui-test       # expect-driven TUI under a pty
tests/plan-mode.exp # plan/auto mode wiring e2e
```

## Layout

```text
src/packages.lisp      package graph (kernel locked; EVO.USER open; EVO = public API)
src/util.lisp          safe sexpr IO, settings, ids
src/journal.lisp       entry tree, write-ahead append, fold, fork, sessions
src/model-table.lisp   models, costs, thinking budgets
src/provider.lisp      Anthropic adapter: handoff, SSE, retries, cost
src/tools.lisp         tool registry, sexpr schema -> JSON Schema
src/prompt.lisp        system prompt assembly, skills, templates
src/loop.lisp          agent, turn loop, run-until-settled, hooks, heartbeat
src/lore.lisp          lore stores (M3)
src/compact.lisp       compaction (M3)
src/extension.lisp     load-extension, boot/replay, locks, EVO public API
src/builtin-tools.lisp read / write / edit / bash
src/todo.lisp          todo core extension (D14)
src/goal.lisp          goal driver, audited tools, done-when (§8)
src/tui/               term, input, editor, render, tui, commands (M2)
src/cli.lisp           arg parsing, print/event modes, session bring-up
src/supervisor.lisp    in-binary supervision (§13, D17)
docs/                  seed corpus (also installed to ~/.evo/docs)
extensions/            plan-mode (shipped) + examples/
```
