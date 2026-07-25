# evo-agent

`evo` is an agent who self-evolves. See [design.md](design.md) for the full
design; this README covers the implemented MVP and how to run it.

## MVP status

Implemented (design milestones M0 + M1, plus the day-one pieces of later
milestones):

- **Provider core (M0)** — unified message model; Anthropic Messages adapter
  with hand-rolled SSE streaming, thinking blocks (chunked signatures),
  tolerant tool-argument accumulation, prompt-cache breakpoints, errors-as-data
  (`:stop-reason :error`, never a signal into the loop), status-classified
  retries with backoff + retry-after; hand-written model table with rational
  cost accounting.
- **Kernel loop + journal (M1)** — append-only sexpr entry **tree** per
  session (write-ahead, one form per line, `*read-eval*` nil, restricted
  sexpr-JSON vocabulary); all state a fold over the root→leaf path; branching
  by leaf move; turn loop with steering queues, save points (context rebuilt
  wholesale from the journal every turn), truncation guard, sequential
  read/write/edit/bash tools; run-until-settled driver; kill -9 mid-task
  resumes cleanly.
- **CLI (print + event modes)** — `evo -p "prompt"` streams text to stdout
  (trace on stderr); `--events` emits line-delimited sexpr events; `--resume`
  reopens the latest (or a named) session.
- **Goal system** — `--goal` creates a persisted `:goal` entry; idle
  continuation re-steers the agent with budgets and completion/blocked audits;
  `get_goal`/`create_goal`/`update_goal` tools; optional agent-authored
  `done_when` predicate that the kernel **runs** before accepting completion
  (§8.4) — premature completion claims are mechanically rejected.
- **Extension API + self-extension seed** — `evo:register-tool`,
  `evo:register-command`, `evo:on` event hooks (including `:tool-call`
  interception with block/mutate), `load_extension` tool: the agent writes a
  `.lisp` file into `EVO.USER`, loads it into its own runtime, the load is
  journaled as a `:load` entry and replayed on resume. Kernel packages are
  locked (SBCL package locks, D8); `--no-userspace` boots quarantined (§13).

Deferred (per the design's own staging): the TUI (M2), compaction + lore
(M3), the supervisor process (M4), the full seed corpus (M5).

## Build & run

Requires SBCL (tested with 2.6.6) and Quicklisp (deps: dexador,
com.inuoe.jzon, flexi-streams, bordeaux-threads).

```sh
make build          # builds build/evo-core, run it via bin/evo
bin/evo -p "run ls with the bash tool and summarize"
bin/evo --goal "make ./test.sh pass"
bin/evo --resume                 # continue latest session for this cwd
bin/evo --events -p "..."        # sexpr event stream on stdout
bin/evo --list-sessions
bin/evo --help
```

The launcher always passes `--dynamic-space-size` (default 4096, override
with `EVO_DYNAMIC_SPACE_SIZE`, D10).

## Settings

Sexpr plists (D3): global `~/.evo/settings.sexp`, project
`<cwd>/.evo/settings.sexp` (project wins). `EVO_HOME` overrides `~/.evo` (used
by the tests). Example pointing at a local Anthropic-compatible proxy:

```lisp
(:model "ark-deepseek-v4-pro"
 :thinking :xhigh
 :providers (:anthropic (:base-url "http://127.0.0.1:8787"
                         :api-key "sk-...")))
```

Against the real API, set `:providers (:anthropic (:api-key-env
"ANTHROPIC_API_KEY"))` or omit — the env var is the default.

## Tests

```sh
make test           # unit: sexpr IO, journal tree/fold, schema, SSE, handoff
make integration    # live, needs the proxy on 127.0.0.1:8787:
                    #   M0/M1 tool round-trip, kill -9 + resume, goal run
```

## Layout

```
src/packages.lisp     package graph (kernel locked, EVO.USER open, EVO = public API)
src/util.lisp         safe sexpr IO, settings, ids (reseeded per process)
src/journal.lisp      entry tree, write-ahead append, fold, sessions
src/model-table.lisp  models, costs, thinking budgets
src/provider.lisp     Anthropic adapter: request build, handoff pass, SSE, retries
src/tools.lisp        tool registry, sexpr schema -> JSON Schema
src/prompt.lisp       system prompt assembly
src/loop.lisp         agent struct, turn loop, run-until-settled, event hooks
src/extension.lisp    load-extension, boot/replay, package locks, EVO public API
src/builtin-tools.lisp read / write / edit / bash
src/goal.lisp         goal entries, continuation steering, audited tools, done-when
src/cli.lisp          arg parsing, print/event modes, session lifecycle
```
