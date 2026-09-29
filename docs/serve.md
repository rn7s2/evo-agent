# serve — one session, controlled over HTTP

`evo-agent serve` runs an agent session with no terminal attached and hands
its controls to HTTP: anything a person can do in the TUI, a program can do by
request. It exists to be driven — a coordinator process
([`evo-swarm`](swarm.md)) starts worker evos this way and talks to them
through nothing else — but curl works just as well.

It is also the frontend a program can run its own session on: the program
sets what the server *is* and adds its own routes, and everything below
stays serve's ([the two seams](#the-two-seams-a-program-adds)). `evo-swarm
serve` is that program, and it is documented in
[`docs/swarm.md`](swarm.md#serving-the-swarm).

```sh
evo-agent serve --token-file ~/.evo/serve.token    # 127.0.0.1:8421
TOKEN=$(cat ~/.evo/serve.token)
curl -s -H "Authorization: Bearer $TOKEN" localhost:8421/state
curl -sN -H "Authorization: Bearer $TOKEN" \
     -d '{"text": "run the tests", "stream": true}' localhost:8421/prompt
```

It is a mode of the agent binary (`evo-agent`), not a separate program: the
same kernel, journal, extensions and supervisor, with an HTTP frontend where
the TUI would be. The session is a normal session — its journal lands in
`~/.evo/sessions/`, `evo-agent --resume` opens it in the TUI later, and the
reverse works too.

## Starting it

```text
evo-agent serve [--host <addr>] [--port <n>] [--token-file <path>] [--allow-remote]
                [--resume [path]] [--model <id>] [--thinking <level>]
                [--no-userspace] [--no-supervisor]
```

| Flag | Meaning |
|---|---|
| `--host` | Address to bind. Default `127.0.0.1`. Anything that is not loopback needs `--allow-remote` too. |
| `--port` | Port to bind. Default `8421`; `0` lets the OS pick (the chosen one is printed). |
| `--token-file` | Write the bearer token here, mode 0600, deleted again on a clean shutdown. |
| `--allow-remote` | Permit a non-loopback `--host`. |
| `--resume`, `--model`, `--thinking`, `--no-userspace`, `--no-supervisor` | As for plain `evo-agent`. |

On start it prints one line to stdout, `evo-agent serve: listening on
http://127.0.0.1:8421/ (token in …)`, and serves until `POST /shutdown`,
which exits 0. A port it cannot bind is a usage error (exit 64), which the
supervisor never restarts. `-p`, `--events`, `--image` and `--goal` are
refused: serve has exactly one driver, and it is HTTP.

## Environment

| Variable | Meaning |
|---|---|
| `EVO_SERVE_TOKEN` | The bearer token, instead of a random one. |
| `EVO_SESSIONS_DIR` | Keep this process's sessions in that directory instead of `~/.evo/sessions/<cwd>/` (evo-swarm lanes do; their `--resume` then finds their own session). |
| `EVO_SERVE_WATCH_PID` | Shut down cleanly when the process with that pid is gone — how an evo-swarm lane follows its coordinator. |

## Security

- **Loopback by default.** `--host` must name this machine (`127.x.x.x`,
  `localhost`, `::1`) unless `--allow-remote` says otherwise. There is no TLS;
  put a proxy in front if the network is not yours.
- **A bearer token on every request**, health checks included:
  `Authorization: Bearer <token>`. A missing or wrong token is `401` and does
  nothing. Comparison is constant-time.
- **Where the token comes from.** `EVO_SERVE_TOKEN` when set; otherwise 32
  random bytes from the OS, as 64 hex characters, minted once per launch. It
  reaches you through `--token-file` (created empty, restricted to 0600, then
  written — the secret is never on disk under a looser mode). One of the two
  is required: a token nobody can read would lock everybody out, so serve
  refuses to start without a way to hand it over.
- **`/eval` is remote code execution, by design.** It evaluates Lisp in the
  session's image, exactly as `/eval` in the TUI does; `/command /eval …`,
  `/load-extension` and every tool the agent has do the same in other words.
  evo is permissive (design.md §1) and serve does not pretend otherwise: the
  token is the gate. Guard it like an SSH key.
- **No secrets in state.** `GET /registry` reports providers with
  `has_api_key` and the *name* of the variable a key comes from, never a key;
  settings whose names mention a key, token, secret, password, credential or
  auth are left out.

## Supervision

Invoked plainly, `evo-agent serve` runs under evo's own supervisor like any
other session (design.md §15): the parent mints the token once and passes it to the
child in `EVO_SERVE_TOKEN`, so a restarted child keeps the token clients
already hold. The child touches the heartbeat file from its session loop; a
crash or a hang restarts it with `--resume` and the server flags (`--host`,
`--port`, `--token-file`, `--allow-remote`, `--no-userspace`) — not
`--model`/`--thinking`, which the journal already carries and which would
otherwise override a `/model` switch made since. Repeated fast boot failures
quarantine to `--no-userspace`. A restart is a new event log: ids start again
at 1, announced by a `hello` event with the new pid.

## Requests and replies

HTTP/1.1, one request per connection (`Connection: close`). Request bodies are
JSON objects with a `Content-Length` (chunked uploads get `411`); replies are
JSON, or Server-Sent Events where noted. Field names are the snake_case of
evo's keywords (below).

### Command replies

Every `POST` is a command. It runs on the session's own thread, in arrival
order — the same thread and the same code the TUI uses — and answers with:

```json
{"ok": true, "status": 200, "error": null,
 "output": [{"style": "notice", "text": "◆ goal created: …"}],
 "data": {"goal": {…}},
 "choices": null,
 "cursor": 41,
 "task": {"id": "3f2a9c1e", "kind": "run", "started": 3999585125,
          "age": 0, "step_age": 0}}
```

- `output` — what the TUI would have printed, one entry per line it would
  have scrolled, with `style` one of `plain dim notice success error`.
- `data` — the structured result, per command (see each).
- `choices` — when a command would open a picker in the TUI (`/model`,
  `/lang`, `/tree`, `/resume` with no argument): `{"title", "index",
  "items": [{"label", "value", "description"}]}`. Send the command again with
  the chosen `value` as its argument.
- `cursor` — the id of the last event published *before* the command ran.
  `GET /events?since=<cursor>` shows everything the command caused.
- `task` — the session's task after the command, or `null` when idle.

**Streaming.** Add `"stream": true` to the body (or send `Accept:
text/event-stream`) and the reply comes as SSE instead: first an event
`result` carrying the reply above, then — if a task is running — every
session event from `cursor` on, ending after the `settled` event that says the
session is idle again. A command that leaves nothing running ends its stream
right after `result`.

### Statuses

| Status | Meaning |
|---|---|
| 200 | Done. |
| 400 | Bad request: malformed JSON, a missing field, a bad argument (`/thinking hot`, `/model no-such-id`). |
| 401 | Missing or wrong token. |
| 404 | No such endpoint, no such command, no such session or entry. |
| 405 | Wrong method for the endpoint. |
| 409 | **Not now**: the session is busy. A command that needs a quiet session (below) found a task running or input queued; a goal is not in the state the command needs; `/steer` with nothing running. Nothing happened. |
| 411, 413, 431 | Chunked body; body over 64 MiB; headers over 64 KiB. |
| 422 | The command ran and failed: an extension command signalled, an evaluation signalled, a file failed to load. |
| 503 | The server is shutting down. |

### Concurrency: 409, never a race

The session is owned by one thread (design.md §6). Every command is queued to
it and runs to completion before the next, so two clients can never interleave
inside one command. The task — the one run or compaction — is started and
reaped by that thread alone.

- **Switching journals** (`/new`, `/fork`, `/resume`, `/tree`, `/rewind`)
  needs a *quiescent* session: no task, and no input queued. Input queued
  against one journal must not be answered in another. Otherwise `409`.
- **Rebuilding the runtime or the context** (`/reload`, `/compact`) needs
  only an idle session: no task. Queued input stays welcome — releasing input
  the model gate held is why one reloads.
- **Input** never conflicts: `/prompt` while a run is going lands at its next
  turn boundary, exactly like typing into the TUI mid-run.
- **Interrupting** is a message, not a mutation: `/interrupt` posts the abort
  and the run's own thread tears down what it owns.

## Endpoints

All take the token. `GET`s read state; `POST`s are commands with the reply
above.

### Input and control

**`POST /prompt`** `{"text": "…", "images": [...], "stream": bool}` — the
user's turn, as if typed in the TUI and sent. Starts a run, or lands at the
running one's next turn boundary. `images` is a list of `{"path": "…"}` (read
from disk) and/or `{"data": "<base64>", "name": "…"}` (by value; the media
type is sniffed from the bytes — png, jpeg, gif, webp). The text is literal:
a leading `/` is not a command here (use `/command`). If no model resolves,
the input stays queued and `output` says why; `/model` or a registration
releases it. `data`: `{"queued": true}`.

**`POST /steer`** `{"text", "images", "stream"}` — mid-run input for the
running task's next turn boundary. `409` when nothing runs (`/prompt` starts
a run).

**`POST /follow-up`** `{"text", "stream"}` — input for after the run settles
(the kernel's follow-up queue). With nothing running it is simply the next
prompt.

**`POST /interrupt`** `{"stream"}` — interrupt the running task (the TUI's
esc). `data`: `{"interrupted": true}`, or `null` when nothing was running
(not an error). Stream it to wait for the run to settle.

**`POST /command`** `{"text": "/goal ship it"}` or `{"name": "goal", "args":
"ship it"}` — a slash command, resolved exactly as the TUI resolves one:
extension commands, then builtins, then skills (`/skill:name` or `/name`),
then prompt templates. An unknown command is `404`. The builtins — from the
shared command layer (`src/command/command.lisp`), except `/memory`,
`/global-memory` and `/eval`, which core extensions register
(`src/core-ext/memory.lisp`, `src/core-ext/eval.lisp`):

| Command | Effect | `data` |
|---|---|---|
| `/goal` | Show the goal. | `goal` |
| `/goal <text>` | Create a goal and start driving it; on an active goal, refine its objective (steering the run if one is going). | `goal` |
| `/goal pause`, `/goal resume` | The human's goal controls; `409` unless the goal is active / paused. | `goal` |
| `/model` | List models (`choices`). | — |
| `/model <id>` | Journal the model for the next turn. | `model` `{id, provider}` |
| `/thinking <level>` | `low medium high xhigh max`. | `thinking` |
| `/lang`, `/lang <code>` | List / set the prompt language. | `language` |
| `/compact [hint]` | Compact now, as a task (stream it). `409` while busy. | — |
| `/lore [text]`, `/global-lore [text]` | List, or add project / global lore. | `lore` or `id` |
| `/memory [request]`, `/global-memory [request]` | Show memory, or hand a request to the agent. | — |
| `/tree` | List the entries on the path (`choices`). | — |
| `/tree <id>` | Move the leaf there. On a user message, the leaf goes above it and its text comes back to edit. | `leaf`, `draft` |
| `/rewind` | Move the leaf above the last user message (the TUI's esc esc). | `leaf`, `draft` |
| `/fork` | Copy the path into a new session and switch to it. | `session` |
| `/new` | Switch to a fresh session. | `session` |
| `/resume` | List this directory's sessions (`choices`). | — |
| `/resume <n>` or `/resume <path>` | Switch to that session (n counts the list, 1 = last worked in); an active goal there resumes. | `session` |
| `/export [path]` | Write the transcript as markdown. | `path` |
| `/reload` | Rebuild userspace (init files, extensions, post-init). | — |
| `/eval <sexpr>` | The TUI's `/eval`. | — |

The TUI's presentational commands — `/help`, `/todo`, `/theme`, `/image`,
`/quit` — have no meaning without a screen; `/shutdown` is serve's `/quit`.

**`POST /eval`** `{"form": "(+ 1 2)"}` — evaluate exactly one sexpr in
`EVO.USER`, as `/eval` does (two forms, or none, is `400`). Or `{"code":
"(defun f () 1) (f)"}` — a body, as the agent's `eval` tool takes it: every
form in order, the last one's values back. `data`: `{"values": ["3"],
"output": "<anything printed>", "result": "⇒ 3"}`. A condition is `422` with
it in `error`. Nothing here is journaled; for something durable, load a file.

**`POST /load-extension`** `{"path": "/abs/file.lisp"}` — compile and load a
file into userspace and journal the load, so a resumed session replays it
(`evo:load-extension`). `data`: `{"path"}`; `422` if it fails.

**`POST /shutdown`** — end the session: `:session-end` fires for extensions,
the task is interrupted and reaped, open event streams are flushed and
closed, the token file is removed, and the process exits 0.

### State

**`GET /health`** — `{"ok": true, "pid", "cursor", "name", "version",
"features"}`. The last three say what this server is: the agent's serve names
itself `evo-agent`, with the binary's version and no features, and a program that
runs its own session on serve names itself and lists what it adds
([below](#the-two-seams-a-program-adds)). A client that ignores them sees the
`/health` it always saw.

**`GET /state`** — everything a status line shows, and then some:

```json
{"status": "idle | running | compacting",
 "task": {"id", "kind", "started", "age", "step_age"} | null,
 "turn": 3, "cursor": 118,
 "model": "claude-opus-5", "provider": "anthropic", "model_ready": true,
 "thinking": "high", "language": "en",
 "context_tokens": 48211, "context_window": 1000000,
 "goal": {"goal_id", "objective", "status", "token_budget", "tokens_used",
          "tokens_used_live"} | null,
 "todos": [{"text", "status"}],
 "jobs": {"count", "since", "command"} | null,
 "session": "/…/sessions/…/2026…_….sexp", "session_id": "…",
 "session_started": true, "leaf": "a1b2c3d4", "pending_input": null}
```

`step_age` is the TUI's activity clock: seconds in the current *step* (one
turn, or one compaction), not the whole task — how a slow step is told from a
wedged one.

**`GET /transcript[?limit=N]`** — `{"messages": [...]}`, the context the next
turn sends (the journal fold), last N if given. Image blocks keep their name,
type and size; their base64 is replaced by `"data_omitted": true`.

**`GET /journal[?limit=N]`** — `{"path", "header", "leaf", "entries"}`: the
entries on the root→leaf path, as journaled (images elided the same way).

**`GET /lore`** — `{"entries": [{"id", "text", "scope", "timestamp"}]}`.

**`GET /sessions`** — `{"current", "sessions": [{"n", "path", "label",
"timestamp", "summary"}]}`, last worked in first; `n` is what `/resume <n>`
takes.

**`GET /registry`** — what the session can use: `models`, `providers`
(`key`, `base_url`, `api_key_env`, `has_api_key`), `apis`, `tools`
(`name`, `description`), `active_tools`, `commands` (`name`,
`description`), `skills`, `templates`, `languages`, `settings` — no secrets.

### Events

**`GET /events`** — a long-lived SSE stream of every session event:

```text
id: 57
event: text-delta
data: {"type":"text-delta","text":"Looking at the","run_id":"c4112f4b","turn":0}

```

- With `Last-Event-ID: N` (what an `EventSource` sends when it reconnects) or
  `?since=N`, it starts right after event N; with neither, from now on.
  `?since=0` replays everything the log still holds.
- Ids are consecutive from 1 within one process. The log keeps the last
  20,000 events; a cursor older than that gets an event `gap` `{"from",
  "to"}` naming what it missed — never a silent hole.
- A comment line (`: keepalive`) every 15 s of silence.
- The stream ends when the server shuts down, after the last event.

The events are the kernel's `--events` plists, unchanged, plus the ones in the
table — serve's own. A program that runs its own session on serve publishes
its events into the same stream, with the same ids, resume and
[mapping](#the-mapping); `lane-state` is `evo-swarm`'s
([docs/swarm.md](swarm.md#serving-the-swarm)).

| Event | Payload | From |
|---|---|---|
| `run-start` `turn-start` `message-start` `text-delta` `thinking-delta` `tool-call-start` `tool-result` `message-end` `run-end` `steering` `compaction-start` `compaction-end` `provider-retry` | as in `--events`; each carries `run_id` and `turn` | the kernel |
| `todo-changed` | `todos` | the todo tool |
| `task-start` / `task-end` | `task_id`, `kind`, (`outcome`, `error`) | serve: a run or compaction began / its thread was reaped |
| `settled` | `outcome`, `goal` | serve: the task ended and nothing else started — the session is idle; `goal` is the session goal's status (`active`, `complete`, `paused`, `budget-limited`), `null` with no goal |
| `output` | `style`, `text` | what a command (or a finishing task) said, the TUI's scrollback |
| `user-input` | `text` | input an extension queued off-thread (`evo:request-run`) |
| `session-switched` | `session` | `/new`, `/fork`, `/resume` |
| `hello` `ready` `shutdown` `bye` | `pid`, `port`, `session` / `resumed` / — / — | serve's lifecycle |
| `gap` | `from`, `to` | events this stream missed |
| `unprintable-event` | `original_type` | an event holding a value the mapping cannot carry |

### The mapping

Events and state are evo sexprs (the journal's vocabulary, design.md §4.3).
They cross the wire through exactly one mapping, the inverse of
`evo:json->sexpr`:

| sexpr | JSON |
|---|---|
| plist with keyword keys | object; `:line-count` → `"line_count"` |
| any other list, any vector (not a string) | array |
| string, integer | string, number |
| ratio, float | number |
| `t` | `true` |
| `nil` | `null` |
| keyword as a value | its name in lower case: `:text-delta` → `"text-delta"` |

Serve's own replies use `false` where a boolean must read as one (`"ok"`).
`evo:json->sexpr` reads it all back: objects become keyword plists under the
same keys, arrays vectors, `null` and `false` `nil`. What JSON cannot say is
a keyword *value* (it comes back as its string) and list-versus-vector (it
comes back a vector); so the round trip is exact at the JSON level — encode,
decode, encode again gives the same JSON value (object key order is not
defined, and differs between SBCL and ECL) — and the unit suite checks that
for every event shape the kernel emits.

## The two seams a program adds

serve is evo's headless frontend, and it is also a *host*: a program runs its
own session on the same server, speaking the same protocol, and adds exactly
two general things to it. Neither seam knows what the program is — a name, a
version, a feature list and some routes are the whole surface — which is why
nothing in the core names `evo-swarm`.

**Identity.** The server says what it is, in `/health`:

```json
{"ok": true, "pid": 51234, "cursor": 118,
 "name": "evo-agent", "version": "0.1.0", "features": []}
```

`name` is the program (`evo-agent` or `evo-swarm`), `version` its version, and
`features` the capability names it serves beyond the protocol above — always
an array, empty when the program adds nothing. The agent's serve is
`evo-agent` with none; `evo-swarm serve` is `evo-swarm` with `["swarm"]`, whose meaning is
[`GET /lanes` and its siblings](swarm.md#serving-the-swarm). A client reads
them to learn what it is talking to — and what it may ask for — before it asks
for anything. They are additive: a client that ignores them sees the
`/health` it always saw.

The program states it once for every server it builds (`evo.serve:*identity*`)
or for one server (`evo.serve:make-server :identity`); a route handler reads
it back with `evo.serve:server-identity`.

**Routes.** The program brings its own endpoints, and serve serves them from
the same listener, under the same bearer token and HTTP limits as the ones
above. `evo.serve:add-route` adds one — or `make-server :routes` gives one
server extra routes considered before the built-ins, so it can override a
specific path without losing the unchanged protocol. Program-wide routes added
after a server was built still reach it. A route is an **exact path** (`/lanes`) or a **prefix**, with `:prefix t` and a
pattern like `/lanes/`, so one route can answer a family of paths: what
followed the pattern arrives in `evo.serve:*route-tail*` (`/lanes/3/transcript`
through `/lanes/` is `3/transcript`), and the handler works the rest out. A
handler takes the same arguments the built-in ones do — the server, the
request, the parsed JSON body and the stream — and answers with serve's
helpers (`evo.serve:write-json`, `write-response`, `write-error`, the
`write-sse-*` family) and the [mapping](#the-mapping) above, so its JSON is
snake_case evo sexprs like everything else. Re-adding the same path and method
replaces the route, so reloading a file of routes does not stack copies.

Matching is deterministic: exact routes in table order first, then prefix
routes with the longest pattern first; a path nothing matches is `404`, and a
path that matches with another method is `405`. And a program cannot loosen
what serve already guarantees — the token check, the body and header limits
and the concurrency rules happen before any handler, built-in or not.

A program's events join the same stream: `evo.serve:server-publish` appends an
event to this server's log — numbered once, in order, seen by every `/events`
client and by anyone reconnecting with `Last-Event-ID` — exactly as a session
event is. `evo.serve:server-cursor` is the id to resume from.

On the Lisp side that is all of it: the program building the server sets an
identity (`*identity*`, or `make-server :identity`) and adds routes
(`add-route`, or `make-server :routes`), and publishes its own events
(`server-publish`). Nothing else about a program enters serve.

## What serve is, underneath

- **One command layer.** What `/goal`, `/model`, `/compact`, `/tree`,
  `/resume` … *do* lives in `src/command/command.lisp`, in the core, and both
  the TUI and serve dispatch through it. A frontend implements a small *host*
  protocol — is a task running, start one, show this line, offer these
  choices — and never re-implements a command. The TUI shows a refusal as a
  dim line; serve turns it into `409`/`400`/`404`. The words cannot drift
  apart between the two, because there is only one copy of them.
- **The frontend protocol.** Extensions ask the core, not a frontend, whether
  a person is at a terminal (`evo:frontend-interactive-p` — serve says no)
  and whether a run can be started for input they queued off-thread
  (`evo:request-run` — serve says yes). So the LaTeX renderer does not tell a
  headless session its formulas become pictures, the IDE bridge does not poll
  for a status line nobody draws, and a notification reply steers a served
  session like a TUI one.
- **Threads.** The session thread (the process's main thread) owns the task
  and runs every command; the listener accepts; one short-lived thread per
  connection parses, authenticates, and waits for the session thread or
  streams the log; the run worker runs the task. Events are encoded once, on
  the thread that owns their values, into a lock-guarded ring that streams
  poll.
- **No new dependencies.** HTTP is ~200 lines over `usocket` octet streams
  (already in the image through dexador), JSON is jzon, both portable to SBCL,
  ECL and SBCL on Windows.
- **Tested end to end.** `make serve-test` (`tests/serve-e2e.py`) drives a
  real `build/evo-agent serve` over HTTP only, against a stub Messages endpoint
  (`tests/stub-messages.py`): tokens, registration by eval, a streamed prompt,
  interrupt, steer, 409s, `/compact`, the goal controls, model and thinking
  switches, a tool registered mid-session offered on the next turn,
  fork/new/resume, state and transcript, and a clean shutdown under the
  supervisor with `:session-end` fired.

## Example: a minimal coordinator loop

```python
import http.client, json, time

TOKEN = open("/path/to/serve.token").read().strip()

def call(method, path, body=None):
    c = http.client.HTTPConnection("127.0.0.1", 8421)
    c.request(method, path, json.dumps(body) if body else None,
              {"Authorization": "Bearer " + TOKEN})
    return json.loads(c.getresponse().read())

call("POST", "/command", {"text": "/goal make the test suite pass"})
while call("GET", "/state")["status"] != "idle":
    time.sleep(5)
print(call("GET", "/state")["goal"]["status"])
```
