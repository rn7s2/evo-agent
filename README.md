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

On Windows, `make.ps1` beside the Makefile takes the same targets, with the
variables as parameters (SBCL only — see [Windows](#windows)):

```powershell
.\make.ps1 build          # requires SBCL + Quicklisp
.\make.ps1 install        # builds, seeds $HOME\.evo, installs $HOME\.evo\bin\evo.exe
```

evo ships **no built-in model table**. At minimum, register one model and pick
it — copy the sample at [docs/examples/init.lisp](docs/examples/init.lisp) to
`~/.evo/init.lisp` and edit:

```lisp
(evo:register-provider :deepseek
  :base-url "https://api.deepseek.com/anthropic" :api-key-env "DEEPSEEK_API_KEY")
(evo:register-model "deepseek-v4-pro"
  :provider :deepseek :api :anthropic-messages
  :context-window 1000000 :max-output 192000
  :thinking t :effort t)
(evo:set-setting :model "deepseek-v4-pro")
```

Without that, evo exits with a pointer to the sample.

## Usage

```sh
evo                                     # interactive TUI (on a tty)
evo -p "run ls and summarize"           # print mode: text on stdout
evo --events -p "..."                  # line-delimited sexpr events
evo --goal "make ./test.sh pass"       # goal run; survives its own death
evo --resume                           # reopen the latest session here
evo --image shot.png -p "what broke?"  # attach an image to the prompt
evo --list-sessions
```

Invoked plainly, `evo` is its own supervisor: the parent process re-spawns the
same binary as a supervised child (inherited stdio, so the TUI just works),
monitors a heartbeat file, restarts with `--resume` after crashes and hangs,
and quarantines repeated boot failures with `--no-userspace`. Exit codes:
`0` done · `1` error · `2` goal blocked/paused · `3` budget-limited · `64` usage error.

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
  error results; errored and aborted turns are elided; images degrade to a
  named text placeholder for a model registered `:vision nil`, so switching to
  a text-only model mid-session costs a screenshot, not every turn after it.
- **Images are first-class input**: an `:image` block carries base64 bytes and
  a sniffed media type, encoded as an Anthropic `image` source or an OpenAI
  `input_image` data URL. See *Images in* below for how one gets there.
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
  active tools, goal, todo list — all derived by folding the root→leaf
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
  system prompt every turn, each tagged with an `[id]`. Global
  (`~/.evo/lore.sexp`, via `/global-lore`), project (`.evo/lore.sexp`, via
  `/lore`), and session scopes. The agent can edit or remove entries by id
  via the `lore` tool, but only when you explicitly ask it to.
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
- **The agent owns the goal, not just its status.** `update_goal` lets the
  model refine the live objective, attach or replace the `done_when` verifier,
  pause the goal when it needs the user (which stops the idle loop), and resume
  it — as well as complete/block it. The user doesn't drive the goal directly;
  they state intent and the agent folds it in.
- **Budgets** run every turn over tokens. Exhaustion moves the goal to
  `:budget-limited` and the next steering is a wrap-up template. A
  session-level budget exists too.
- **Verified completion**: when an objective is mechanically checkable, the
  agent writes a named zero-argument predicate into a userspace file, journals
  it via `:load`, and references it by name on the `:goal` entry (at creation
  or attached later with `update_goal done_when`). On `update_goal :complete`,
  the kernel runs the predicate — failure returns an error and the goal stays
  active. The model's completion claim becomes a checked assertion it wrote
  against itself.
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
   gates, read-only policies, and sandboxing all build on.
3. **Filesystem convention.** `~/.evo/extensions/` and
   `<project>/.evo/extensions/` load at boot and are writable by the agent.
   File name is load order: `NNN-name.lisp`, `000`–`099` foundations,
   `100`–`899` ordinary extensions, `900`–`999` wrappers that must load last.
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
guide, Windows field manual) and example extensions — git-checkpoint and
permission-gate ([extensions/examples/](extensions/examples/)).

### Core extensions

The kernel owns the core loop and nothing else. Everything outside it —
**including the TUI** — is a core extension: bundled, written against the same
API as user extensions, holding the same privileges. Three things distinguish
core extensions from user ones: they ship with the binary, they load first, and
the essential ones cannot be disabled.

- **TUI** — adaptive renderer in normal scrollback + managed bottom region,
  SIGWINCH live reflow, multi-line editor (Enter sends, Shift+Enter newline,
  a big paste collapses to a placeholder, paste-to-expand), pasting with or
  without bracketed paste, image paste (ctrl+v, cmd+v, drop a path), slash
  commands, streaming rendering, `--swank` developer side-door.
- **Todo** — checklist tool rendered in the panel; state rides `:custom`
  entries (invisible to the LLM), survives restart and compaction, embedded in
  goal continuation steering.
- **Memory** — structured global/project memory, injected once per session.

### Images in

No terminal hands an application the image on the clipboard — pasting is a
*text* channel. So every gesture below is really the same one: something tells
evo the user meant "an image", and evo reads the system pasteboard itself
(macOS pasteboard, `wl-paste`, `xclip`, PowerShell under WSL). All end as a
`[Image #n]` token in the editor:

- **ctrl+v** — the gesture that reaches evo in every terminal. The reader
  takes pixels (a screenshot, a copied image) or a file the clipboard merely
  points at, so copying an image *file* in Finder, Nautilus, Dolphin or
  Explorer works too.
- **ctrl+alt+v** — the same request, for terminals that keep ctrl+v for their
  own paste command (VS Code on Linux/Windows, Windows Terminal under WSL).
- **cmd+v / right-click → Paste** — the terminal finds no text on the
  clipboard and pastes the empty string; that *empty bracketed paste* is the
  only trace of the gesture, and evo takes it as one. Emulators built on
  xterm.js (VS Code, Cursor) send it; Terminal.app and Warp send nothing at
  all, and there ctrl+v or `/image` is the door.
- **Pasting or dropping an image file's path** attaches the file — POSIX
  paths, `file://` URLs, quoted and backslash-escaped paths, and Windows
  paths inside WSL (mapped through `wslpath`). A paste attaches only when it
  is *nothing but* paths to images — prose that mentions a `.png` stays prose.
- **`/image [path ...]`** does the same for a typed path, or the clipboard
  with no argument; `--image` does it for headless runs.

A keystroke only counts if it survives the trip: terminals encode a modified
key in one of three ways (a legacy control byte, kitty's `CSI u`, xterm's
`modifyOtherKeys`), evo asks for the latter two at startup, and it decodes all
three — anything less makes a key silently do nothing on exactly the terminals
that honoured the request. The request is skipped where the answer cannot be
understood (`TERM=dumb`, no `TERM` at all), popped exactly as pushed on exit,
and `EVO_KEY_ENHANCEMENT=0` turns it off for an emulator that claims a
protocol and then mangles it.

When no image comes back, evo says which of the two things went wrong: the
clipboard was read and held no image, or nothing in this session can read a
clipboard at all — no `wl-clipboard`, no `xclip`, no display (a plain ssh
session), no PowerShell bridge under WSL. "No image on the clipboard" is a
lie when the image *is* on the user's clipboard and evo simply cannot see it.

The token is the whole interface: it shows the image is attached, marks where
in the message it sits, and deletes with backspace like any other text — only
attachments whose token survives to Enter are sent. Images travel by value
(base64 in the block, and so in the journal), which keeps a session
self-contained and replayable with no side files to lose. Oversized images are
downscaled first (`sips`, ImageMagick) rather than rejected, and a model
registered without vision gets a named placeholder instead of a 400.

### Math rendering

LaTeX math in agent output — `$…$`, `$$…$$`, `\(…\)`, `\[…\]` — renders as a
real typeset image inline in scrollback, the way KaTeX/MathJax draw it, not an
ASCII approximation: inline formulas sit ON the prose baseline (pixel-exact,
via the formula's own reported baseline metrics) and flow with the text, which
wraps by formula rather than through one; display equations get their own
block. The split is deliberate: the TUI core only *finds* the math and *places*
whatever a renderer returns (falling back to the raw LaTeX source), and the
bundled `extensions/300-latex-math.lisp` is the renderer — it rasterizes each
formula with the LaTeX toolchain (`latex` + `dvipng`) and emits it via the
kitty graphics protocol (VS Code's integrated terminal renders it on all three
platforms with `terminal.integrated.enableImages` and GPU acceleration on; so
does kitty itself). So the heavy, optional, platform-bound half is an
extension; with it absent, math is just shown as source. It stays off unless
the toolchain is present, caches every formula by content hash, registers a
system-prompt note asking the agent to write real LaTeX while rendering is
active, and never paints an image into the managed bottom region (the live
preview shows source; the image lands only when the line reaches scrollback).
Prerequisites, calibration, and settings: [docs/math.md](docs/math.md); the
renderer seam: [docs/extension-api.md](docs/extension-api.md#math-rendering-evotui).

### Bionic reading

Agent prose can be shown "bionic reading" style — the leading letters of each
word bolded, so the eye fixates on the stem and skims the rest. It rides the
same kind of seam as math: the TUI core hands each run of plain prose (never
`code`, link URLs, or already-bold text) to a pluggable `*prose-styler*`, and
the bundled `extensions/350-bionic-reader.lisp` is one such styler. It only
touches runs of ASCII Latin letters, so English is bolded while accented Latin,
CJK, Cyrillic and other scripts pass through untouched. On by default with the
extension loaded; `/bionic status | on | off | fixation <0..1>` tunes it, and
the seam is documented in
[docs/extension-api.md](docs/extension-api.md#prose-styling-evotui).

### Skills, templates, slash commands

- **Skills**: the Agent Skills standard (SKILL.md + frontmatter) with
  progressive disclosure — only name, description, and path go into the prompt;
  the model reads the file on demand. `/skill:name` forces one.
- **Prompt templates**: `.md` files whose filename is the command, with `$1`..`$9`
  and `$@` substitution.
- **Slash command resolution**: extension commands → builtins → skills →
  prompt templates → send to the agent. Built-ins: `/goal /lore /global-lore
  /memory /global-memory /compact /image /eval /tree /fork /resume
  /model /reload /export /help /quit /exit`.
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
(evo:register-model "deepseek-v4-pro"
  :provider :deepseek :api :anthropic-messages    ; :api = wire protocol
  :context-window 1000000 :max-output 192000 :thinking t
  :vision nil                   ; text-only endpoint: pasted images degrade to
                                ;   a text placeholder for it.  :vision
                                ;   defaults to t, so declare nil for a model
                                ;   that rejects images — or, worse, accepts
                                ;   and silently ignores them
  :effort t)                    ; effort ladder the endpoint accepts (t = all
                                ;   of low/medium/high/xhigh/max, or a subset);
                                ;   a level above it is clamped, not rejected.
                                ;   Omit it and the level can only travel as
                                ;   thinking.budget_tokens — which some
                                ;   endpoints accept and ignore
;; :thinking-mode :adaptive lets the model choose when to think (what
;; Anthropic 4.6+ wants); the default :extended sends budget_tokens.
(evo:set-setting :model "deepseek-v4-pro")

;; Optional (kernel defaults exist for all of these):
(evo:set-setting :thinking :medium)          ; low medium high xhigh max (no off rung)
(evo:set-setting :goal-token-budget 2000000) ; per-goal token cap; omit = no limit
;; :compact-reserve / :compact-keep-recent tune compaction.

;; Endpoints: :anthropic/:openai are pre-seeded (ANTHROPIC_API_KEY /
;; OPENAI_API_KEY); any other key you register yourself, and re-registering
;; overrides field-wise, e.g. to point a stock name at a proxy:
(evo:register-provider :deepseek
  :base-url "https://api.deepseek.com/anthropic" :api-key-env "DEEPSEEK_API_KEY")
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
                    #       templates, compaction, lore, images
make integration    # live e2e: tool round-trip, kill -9 + manual resume,
                    #       goal completion, induced-crash supervision,
                    #       mid-task compaction, --image into a vision model.
                    #       Backend via env (skips if unreachable):
                    #       EVO_TEST_BASE_URL / _API_KEY / _MODEL
                    #       (+ optional _VISION_MODEL for the image test)
make tui-test       # expect-driven TUI under a pty: image paste
                    #       (EVO_TEST_VISION_MODEL), pasting in every shape a
                    #       terminal sends it, model routing, the IDE bridge
```

`.\make.ps1 test` runs the unit suite on Windows. The integration and TUI
suites are POSIX-only (a shell script and expect against a pty), and CI
builds Windows without testing it — see below.

## Windows

Supported, SBCL only: `src/port/port.lisp` is the whole platform surface, and
its ECL branches are Unix facilities (fork+setsid, pgrep, stty, isatty) with
no Windows twin. `make.ps1` refuses `-Lisp ecl` rather than failing halfway
through a build.

What the port layer swaps out on Windows:

| Facility | Unix | Windows |
| --- | --- | --- |
| Terminal mode/size | `/bin/stty` | `SetConsoleMode` + `GetConsoleScreenBufferInfo` (kernel32 via `sb-alien`) |
| Keys and colour | raw mode + ANSI | `ENABLE_VIRTUAL_TERMINAL_INPUT`/`_PROCESSING`, so the same parser and the same escapes work unchanged |
| Live resize | `SIGWINCH` | polled once per tick — there is no such signal |
| The `bash` tool | `/bin/sh -c` | PowerShell (`cmd.exe` fallback), and the tool description says so, because a model told it has `/bin/sh` writes commands that cannot run |
| Killing a process tree | `pgrep -P` + `kill -9` | `taskkill /F /T` |
| `PATH` lookup | `:`-separated | `;`-separated, `PATHEXT` suffixes |
| Environment | `environ` | the Win32 environment block |
| Session file names | `/` → `-` | drive colon and `\` go too (`C:\Users\me` is not a legal file name) |
| Clipboard images | `osascript` / `wl-paste` / `xclip` | PowerShell (`Get-Clipboard`), natively as well as through WSL |
| The TUI's own stdout/stdin | descriptors 0 and 1 | whichever descriptor SBCL used for its own standard streams — on Windows an OS handle, and a literal 1 there is an invalid handle, not stdout |

Requirements and caveats:

- 64-bit SBCL. The kernel32 calls are declared without an explicit calling
  convention, which is right for x64 and wrong for 32-bit builds.
- Windows 10 1607 or newer, for the virtual-terminal console modes. Windows
  Terminal is the comfortable place to run it; conhost works.
- A shell command runs through a temp script file rather than an argv,
  because the Windows command line is one string that each interpreter
  re-splits by its own rules — and most commands an agent writes contain
  quotes.
- Line endings are not depended on. The repo is LF (`.gitattributes`), but
  nothing that has to build assumes it: the sources use no `~<newline>` FORMAT
  continuation — the one construct a CR breaks, with "Unknown directive
  (character: Return)" at compile time — writing long control strings as
  `(cat "..." "...")` instead, and a unit test fails if one reappears. Text
  that crosses a boundary is normalized (`evo.util:normalize-newlines`): what
  the model is sent, what `read` shows it, what `bash` returns from a Windows
  console. `edit` matches whichever endings the file itself uses and preserves
  them. The `.sh`/`.exp` suites are Unix-only and rely on `.gitattributes`.
- No `--new-session` equivalent: on Unix a tool's child is detached from the
  controlling terminal so a `sudo` prompt fails fast instead of hanging. A
  Windows console process that wants to prompt opens its own window instead,
  so there is nothing to detach from.
- Verified on real hardware, not just built: the console input path (every
  key in the virtual-key table — arrows, home/end/delete/insert/pgup/pgdn —
  plus Enter-as-CR, alt, CJK and surrogate pairs), the output path (box
  drawing and double-width CJK glyphs), terminal size, raw-mode set/restore,
  and the supervisor's crash → restart → quarantine loop.
- `.\make.ps1 console-test` runs the proofs: `tests/windows-input-live.lisp`
  and `tests/windows-console-live.lisp` open `CONIN$`/`CONOUT$`, inject real
  `INPUT_RECORD`s with `WriteConsoleInputW`, and read glyphs back with
  `ReadConsoleOutputCharacterW`, asserting the exact bytes evo produces. They
  need a real console, so they run here and not in CI (which stays build-only
  on Windows).

## Layout

Every directory under `src/` is one component owning exactly one package;
`evo.asd` lists them in load order, foundations first.

```text
src/packages.lisp        the whole package graph (kernel locked; EVO.USER open;
                         EVO = public API) — the one file shared by all components

src/port/                EVO.PORT — implementation AND platform portability
  port.lisp              layer, the only code allowed to touch sb-* / ext: /
                         si: symbols or to branch on the OS
src/util/                EVO.UTIL
  util.lisp              safe sexpr IO, settings store, ids, base64
src/media/               EVO.MEDIA
  media.lisp             images in: clipboard readers, media-type sniffing,
                         size cap + downscaling, :image block construction
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
extensions/              vendored user extensions — copied ACTIVE into
                         ~/.evo/extensions by `make install-home`:
  020-claude-oauth-provider.lisp     Claude Pro/Max OAuth provider
  020-kimi-provider.lisp             Moonshot AI Kimi K3 over chat completions
  300-latex-math.lisp                LaTeX math rendered as inline images
  350-bionic-reader.lisp             bionic reading for agent prose (ASCII)
  400-efficiency.lisp                working/reasoning prompt section
  900-ide-context.lisp               editor/IDE bridge
extensions/examples/     reference-only example extensions (installed to
                         ~/.evo/docs/examples) — user extensions, distinct from
                         the bundled core extensions in src/core-ext/

Makefile                 build/install on Unix
make.ps1                 the same targets on Windows (SBCL only)
build.lisp               shared by both: saves build/evo (build/evo.exe)
```

## License

MIT.
