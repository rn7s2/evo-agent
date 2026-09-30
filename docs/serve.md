# serve — one session, watched and driven over HTTP

`evo-agent serve` runs an agent session with no terminal attached and hands
its controls to HTTP: anything a person can do in the TUI, a program can do by
request. It exists to be driven — a GUI, or a coordinator process
([`evo-swarm`](swarm.md)) that starts worker evos this way — but curl works
just as well.

Two ideas hold the protocol together. A client *does not receive events*; it
receives **ops**, the changes to apply to the state it already has, and asks
for a **snapshot** when it has reason to doubt that state. And a client
*does not send requests that mean something*; it sends one of a small set of
**operations** from a published table (`GET /catalog` lists them, with their
arguments).

```sh
evo-agent serve --ready-file ~/.evo/serve.json    # 127.0.0.1:8421
TOKEN=$(python3 -c 'import json;print(json.load(open("'"$HOME"'/.evo/serve.json"))["token"])')
curl -s -H "Authorization: Bearer $TOKEN" 'localhost:8421/snapshot?topics=session'
curl -s -H "Authorization: Bearer $TOKEN" -d '{"rid":"1","op":"input.send","args":{"text":"run the tests"}}' \
     localhost:8421/ops
curl -sN -H "Authorization: Bearer $TOKEN" 'localhost:8421/stream?topics=session'
```

It is a mode of the agent binary (`evo-agent`), not a separate program: the
same kernel, journal, extensions and supervisor, with an HTTP frontend where
the TUI would be. The session is a normal session — its journal lands in
`~/.evo/sessions/`, `evo-agent --resume` opens it in the TUI later, and the
reverse works too.

## Starting it

```text
evo-agent serve [--host <addr>] [--port <n>] --ready-file <path> [--watch-stdin]
                [--allow-remote] [--no-http-eval]
                [--resume [path]] [--model <id>] [--thinking <level>]
                [--no-userspace] [--no-supervisor]
```

| Flag | Meaning |
|---|---|
| `--host` | Address to bind. Default `127.0.0.1`. Anything that is not loopback needs `--allow-remote` too. |
| `--port` | Port to bind. Default `8421`; `0` lets the OS pick. |
| `--ready-file` | Where to publish the port, the token and the session (below). Written atomically, mode 0600, rewritten after every restart, deleted on a clean shutdown. Required unless `EVO_SERVE_TOKEN` is set (tests set it and already know the token). |
| `--watch-stdin` | Shut down cleanly when stdin closes. The parent that started the session holds the write end, so EOF means it is gone — which is immediate and immune to pid reuse. This is how a swarm's lanes follow their coordinator. |
| `--no-http-eval` | Do not offer the `eval` operation (it is remote code execution, by design). |
| `--allow-remote` | Permit a non-loopback `--host`. |
| `--resume`, `--model`, `--thinking`, `--no-userspace`, `--no-supervisor` | As for plain `evo-agent`. |

**The ready file** is the one place a client learns how to talk to a session,
and it is written atomically (a temp file in the same directory, then a
rename), so a reader sees either the previous file or a complete new one:

```json
{"epoch":"7f3a91c2","pid":123,"supervisor_pid":120,"port":53701,
 "url":"http://127.0.0.1:53701/","token":"<64 hex>",
 "session":{"id":"a91f…","path":"/abs/journal.sexp"},
 "program":"evo-agent","version":"0.1.0","restarts":0}
```

`epoch` names this process: it is what makes a cursor from a previous process
recognisable, so a restart is detected in band and pids are never compared.

On start it prints one line to stdout, `evo-agent serve: listening on
http://127.0.0.1:8421/ (ready file …)`, and serves until the
`server.shutdown` operation, which exits 0. A port it cannot bind is a usage
error (exit 64), which the supervisor never restarts. `-p`, `--events`,
`--image` and `--goal` are refused: serve has exactly one driver, and it is
HTTP.

## Environment

| Variable | Meaning |
|---|---|
| `EVO_SERVE_TOKEN` | The bearer token, instead of a random one. |
| `EVO_SESSIONS_DIR` | Keep this process's sessions in that directory instead of `~/.evo/sessions/<cwd>/` (evo-swarm lanes do; their `--resume` then finds their own session). |
| `EVO_SUPERVISOR_PID`, `EVO_RESTARTS` | Who supervises this process and how many times it has been restarted; reported by `/health` and in the ready file. |

## Security

- **Loopback by default.** `--host` must name this machine (`127.x.x.x`,
  `localhost`, `::1`) unless `--allow-remote` says otherwise. There is no TLS;
  put a proxy in front if the network is not yours.
- **A bearer token on every request**, health checks included:
  `Authorization: Bearer <token>`. A missing or wrong token is `401` and does
  nothing. Comparison is constant-time.
- **Where the token comes from.** `EVO_SERVE_TOKEN` when set; otherwise 32
  random bytes from the OS, as 64 hex characters, minted once per launch (by
  the supervisor, so a restarted child keeps the token clients already hold).
  It reaches you through the ready file, which is created empty and
  restricted to 0600 before anything is written to it — the secret is never on
  disk under a looser mode.
- **`eval` is remote code execution, by design.** It evaluates Lisp in the
  session's image, exactly as `/eval` in the TUI does; `extension.load`,
  `command.run` and every tool the agent has do the same in other words. evo is
  permissive (design.md §1) and serve does not pretend otherwise: the token is
  the gate. Guard it like an SSH key. `--no-http-eval` removes the operation
  and its catalog entry.
- **No secrets in the catalog.** A provider reports whether it *has* a key and
  the name of the variable the key comes from, never a key. Error messages
  never quote a value the caller sent, so nothing a client holds can come back
  out of an error. Whether a credential is there is the *API's* answer
  (`evo:api-credentials-available-p`), not the catalog's guess: an API that
  keeps its own — Claude OAuth's token file, say — is asked, and answers
  whether, never what.

## Lifecycle and supervision

Invoked plainly, `evo-agent serve` runs under evo's own supervisor like any
other session (design.md §15). The parent mints the token once and passes it
to the child in `EVO_SERVE_TOKEN`; the child publishes itself in the ready
file and rewrites it after every restart. The restarted child is given
`--resume <the journal it was on>` and the port it had bound — a bare
`--resume` (whatever was last worked in) is only ever typed by a human — plus
the server flags (`--host`, `--port`, `--ready-file`, `--allow-remote`,
`--no-userspace`), and never `--model`/`--thinking`, which the journal already
carries and which would override a `/model` switch made since. Repeated fast
boot failures quarantine to `--no-userspace`.

A restart is a new **epoch**: the op log starts again at seq 1, and any client
that reconnects with a cursor from the old process is told `stream.reset`
rather than being left to guess.

## Requests and replies

HTTP/1.1, loopback, one request per connection (`Connection: close`). Request
bodies are JSON objects with a `Content-Length` (chunked uploads get `411`).
Replies are JSON; `/stream` is Server-Sent Events. Keys and enum values are
snake_case (`lane_report`, `after_run`). Times are epoch **milliseconds**.

| Status | When |
|---|---|
| `200` | Any request that was answered, including an operation that failed: the failure is in the body. |
| `400` | The envelope is malformed: not JSON, not an object, no `rid`, no `op`. |
| `401` | Missing or wrong bearer token. |
| `404` / `405` | No such endpoint / that endpoint with another method. |
| `503` | The server is shutting down. |

### Reads

| Endpoint | Returns |
|---|---|
| `GET /health` | `{ok, program, version, epoch, pid, supervisor_pid, restarts, started_at, session_loop_age_ms}`. Answered without touching the session thread, so a wedged session still answers: `session_loop_age_ms` is how long the session thread has been inside one operation. |
| `GET /snapshot?topics=session,swarm,lane:*&items=200` | `{epoch, seq, topics:{NAME:{state, items[], has_more}}}`. **Atomic across the topics asked for**: every op with seq ≤ `seq` is reflected and none after it. `items` is the newest N per topic (default 200); `lane:*` means every registered lane. |
| `GET /stream?topics=…&since=<epoch>.<seq>` | SSE of ops (below). |
| `GET /items?topic=session&before=<id>&limit=100` | `{items, has_more}` — items older than `before`, newest N of them. |
| `GET /items/<id>?topic=session` | `{item}` — one item whole: the thinking the snapshot bounded, the tool result the snapshot truncated, all of it. |
| `GET /media/<id>/<n>?topic=session` | The raw bytes of image N of that item, with its own `Content-Type`. |
| `GET /catalog` | Everything a client needs to draw its choices (below). |
| `GET /sessions[?scope=cwd\|all&program=]` | `{sessions:[{id, path, cwd, program, swarm_id, title, created_at, updated_at, entries}]}`, newest first. |
| `GET /debug/context`, `GET /debug/journal` | What the model sees, and the raw entries on the path. For tools and tests, never for rendering. |

Reads never queue behind a run: a snapshot is served from the published view,
and `/items` pages out of it.

### The stream

```text
id: 7f3a91c2.1042
event: op
data: {"seq":1042,"ts":1759…,"topic":"session","op":"item.append","id":"e_41af","field":"text","text":"Looking at the"}
```

The first frame is always `hello` — `{op:"hello", epoch, seq}` — so a client
knows which process it is reading and where the stream starts. Then ops, each
with `seq`, `ts` and `topic`:

| op | fields | means |
|---|---|---|
| `item.add` | `item`, `after` | A new item, and which item it follows (`null` = at the end). |
| `item.append` | `id`, `field` (`text`\|`thinking`), `text` | Concatenate `text` onto that field. |
| `item.patch` | `id`, `patch` | A JSON merge patch on the item; arrays are replaced whole. |
| `item.remove` | `id` | The item is gone (a queued input that was cancelled and never ran). |
| `state.patch` | `patch` | A merge patch on the topic's state. |
| `topic.reset` | `reason` | `session_switched`\|`leaf_moved`\|`lane_restarted`\|`swarm_switched`: re-snapshot *that* topic. |
| `stream.reset` | `reason` | `restarted`\|`cursor_too_old`\|`cursor_unknown`: re-snapshot everything and reconnect with the new cursor. |

Rules that make a reconnect boring:

- `since` names the cursor a snapshot returned. Ops after it are exactly what
  the client has not applied.
- `since` from another epoch ⇒ `hello`, then `stream.reset{restarted}`.
- `since` older than what the log still holds (ten minutes, 50 000 ops) ⇒
  `stream.reset{cursor_too_old}`; a `since` the log never had, or one that is
  not a cursor at all, ⇒ `cursor_unknown`. Either way the client re-snapshots:
  a snapshot is cheap, and replaying is not the only way to catch up.
- No `since` ⇒ the stream starts where the log is, after `hello`.
- `item.append` ops for one item are **coalesced over at most 50 ms**, which
  turns a two-thousand-op answer into a handful without visible latency.
- `topics` filters what arrives, and `lane:*` stays a pattern: a lane that
  appears later is still subscribed. Reconnect to change the filter.
- A `: ping` comment every 15 s keeps middleboxes honest. Waiting is by
  condition variable — the server never polls for the next op.

### Operations

Every write is `POST /ops` with an envelope of `rid`, `op` and `args`:

```json
{"rid": "6f1c…", "op": "input.send", "args": {"text": "run the tests"}}
```

and the reply is always HTTP 200 when it was answered:

```json
{"rid": "6f1c…", "ok": true,  "seq": 1043, "result": {"item_id": "e_41af", "queued": true, "blocked": null}}
{"rid": "6f1c…", "ok": false, "seq": 1043, "error": {"code": "busy", "message": "the session is running a task", "detail": null}}
```

`seq` is the op-log position at which the operation's effects are visible: a
client that was streaming can apply its own ops to the same cursor. `rid` is
the client's uuid and is **idempotent**: the server remembers the last 256
replies, so a retry after a dropped connection returns the same answer and
does nothing a second time. A client branches on `error.code` alone.

| op | args → result |
|---|---|
| `input.send` | `{text, images:[{name,media_type,data(base64)}], queue:"now"\|"after_run", topic?}` → `{item_id, queued, blocked}`. The id is the entry id the input will be journaled under, so the row a client draws now keeps its identity when it is sent. `blocked` is `model_not_ready` when the model does not resolve; the input waits, and runs once it does. |
| `input.cancel` | `{item_id}` → `{}`. Input that has not been drained leaves the transcript (the view publishes the `item.remove`); one that has been drained is history — `already_sent`. |
| `run.interrupt` | `{scope:"session"\|"swarm"\|"lane", lane?}` → `{interrupted:["session", …]}` — the topics that were actually stopped, empty when nothing was running. |
| `goal.set` / `goal.pause` / `goal.resume` / `goal.clear` | `{objective, budget?}` / `{}` / `{}` / `{}` → `{goal}`. Only a person pauses or resumes; a cleared goal reads as `null`. |
| `model.set` / `thinking.set` / `language.set` | `{id, provider?}` / `{level}` / `{code}` → `{model}` / `{thinking}` / `{language}`. |
| `session.new` / `session.fork` / `session.resume` / `session.rewind` / `session.move` | `{}` / `{}` / `{session_id\|path}` / `{entry_id?}` / `{entry_id}` → `{session, draft?}`. All of them need a quiescent session: no task and nothing queued. |
| `context.compact` | `{hint?}` → `{task_id}`. Needs an idle session. |
| `lore.add` / `memory.request` | `{scope:"project"\|"global", text}` / `{text}` |
| `command.run` | `{name, args}` → `{notices:[…], data:{…}, choices:{title,index,items:[…]}\|null}` — every other slash command, builtin, skill, template or extension command. It is resolved exactly as the TUI resolves one. |
| `extension.load` / `eval` | `{path}` / `{code}` → `{value}`. `eval` is remote code execution; the token is the gate, and `--no-http-eval` removes it. |
| `server.shutdown` | `{}` — the session ends cleanly and the process exits 0. |

Error codes: `busy`, `not_quiescent`, `no_task`, `goal_state`, `already_sent`,
`unknown_op`, `invalid_args`, `not_found`, `model_not_ready`, `op_failed`,
`shutting_down`.

### The catalog

`GET /catalog` is the whole of what a client can offer, in one document:

```json
{"models":[{"id","provider","name","api","context_window","reasoning","images","ready","reason"}],
 "providers":[{"name","api","has_key","key_env"}],
 "default_model":{"id","provider"}|null,
 "thinking_levels":["low","medium","high","xhigh","max"],
 "languages":[{"code","name"}],
 "ops":[{"name","args":{…schema…},"precondition":"none|idle|quiescent"}],
 "commands":[{"name","description","args_hint"}],
 "skills":[{"name","description"}],"tools":[{"name","description"}],
 "warnings":["…"]}
```

It never contains a key. The builder is total: an entry that raises is dropped
and named in `warnings`, because one broken extension must not cost a client
the whole catalog.

`lanes` — the models a lane can run — is the one half a program adds. For a
server that runs no lanes the key is *absent*, not null: absent reads as "not
a swarm", where null reads as an object that is not there.

## The seams a program adds

serve is evo's headless frontend, and it is also a *host*: a program runs its
own session on the same server, speaking the same protocol, and adds four
general things to it. None of them knows what the program is, which is why
nothing in the core names `evo-swarm`.

**Identity.** The program says what it is, in `/health` and `/catalog`:
`evo.serve:*identity*` for every server it builds, or `make-server :identity`
for one. `evo.serve:server-identity` reads it back. `evo-swarm serve` answers
`evo-swarm` and adds its own topics and routes.

**The catalog's lanes half.** `evo.serve:*catalog-lanes-hook*`, set to a
function of one argument — the `warnings` list the builder is filling — that
returns that half's plist. `evo-swarm` sets it to `evo.serve:lane-catalog`,
which computes the models a lane can run from the kernel API set without
starting a lane, so `GET /catalog` answers `lanes.models[]` from the first
request rather than after the first lane boots:

```lisp
(setf evo.serve:*catalog-lanes-hook* #'evo.serve:lane-catalog)
```

A program that can answer better passes its half directly, as
`evo.serve:catalog-plist`'s `:swarm` argument — `evo-swarm`'s offline
`catalog`/`check` do, having evaluated the lanes' own configuration. The key
is the whole half (`(:models […])`), so a program either hands one in or lets
the kernel-API-set answer stand; both beat not answering at all, since the
hook's absence is what makes `lanes` absent.

A hook that raises costs the document its `lanes` key and names the fact in
`warnings`, like every other entry.

**Routes.** `evo.serve:add-route` adds an endpoint, or `make-server :routes`
gives one server extra routes considered before the built-ins. A route is an
exact path or a prefix (`:prefix t`, pattern `/lanes/`), and what followed the
pattern arrives in `evo.serve:*route-tail*`. A handler takes the server, the
request, the parsed JSON body and the stream, and answers with serve's helpers
(`write-json`, `write-error`, `write-response-octets`, the `write-sse-*`
family). Re-adding the same path and method replaces the route. A program
cannot loosen what serve guarantees: the token check, the body limits and the
concurrency rules happen before any handler.

**Topics.** A client's world is topics, and a program adds its own without
serve knowing them:

```lisp
(evo.serve:register-topic server "lane:3" provider)   ; or "swarm"
(evo.serve:publish-op server '(:op "item.add" :topic "lane:3" :item (…)))
```

The provider answers four generics — `topic-snapshot`, `topic-items-before`,
`topic-item` and `topic-media` — and publishes ops with `publish-op`, which
assigns their `seq` and `ts` and wakes every stream. The one rule a provider
must keep: publish an op from inside the same critical section that changes
the state it describes, so a client can fold the ops onto a snapshot and land
on the state itself. `topic:*` never needs telling about a topic's meaning.

serve installs exactly one topic itself: `session`, whose provider is the view
(`evo.view`, CONTRACT §7).  The view derives the whole of that topic — items,
status, task, model, context, goal, the queue — from the journal and the
kernel's events, and serves nothing to serve's own bookkeeping; serve's part is
to publish the ops it emits, to feed it the events (`view-on-event`), the
journal appends (a journal listener, moved when the session switches journals)
and the input a client queues before it is journaled
(`view-input-queued` / `view-input-cancelled`), and to tell it when the fold
moved in a way no append reports (a leaf move: `topic.reset`).

## What serve is, underneath

- **The op log.** One log per process: a monotonic `seq`, an `epoch` minted
  per process, ops encoded to JSON once (on the thread that owns the values in
  them), retention by time (ten minutes, at most 50 000 ops), and
  condition-variable wake-ups — no sleep-polling anywhere in the server.
  `item.append` ops for one item are held for at most 50 ms and merged, which
  is what keeps a long answer from costing thousands of ops.
- **Snapshots.** A snapshot flushes the coalesced appends, reads the seq, asks
  every provider, and reads the seq again: if it moved, the snapshot is taken
  again. The op log's lock is not held across the providers
  (a provider publishes from inside its own critical section, so holding it
  there would deadlock two threads taking the same two locks in opposite
  orders); the second read is what replaces it.
- **One command layer.** What `/goal`, `/model`, `/compact`, `/tree`,
  `/resume` … *do* lives in `src/command/command.lisp`, in the core, and both
  the TUI and serve dispatch through it. A frontend implements a small *host*
  protocol — is a task running, start one, show this line, offer these
  choices — and never re-implements a command. The words cannot drift apart
  between the two, because there is only one copy of them.
- **The frontend protocol.** Extensions ask the core, not a frontend, whether
  a person is at a terminal (`evo:frontend-interactive-p` — serve says no)
  and whether a run can be started for input they queued off-thread
  (`evo:request-run` — serve says yes).
- **Threads.** The session thread (the process's main thread) owns the task
  and runs every operation, in arrival order; the listener accepts; one thread
  per connection parses, authenticates and answers; one thread writes the
  coalesced appends out; one thread off each `/stream` writes ops from its own
  cursor; the run worker runs the task. Nothing but the session thread touches
  the agent.
- **No new dependencies.** HTTP is ~250 lines over `usocket` octet streams
  (already in the image through dexador), JSON is jzon, both portable to SBCL,
  ECL and SBCL on Windows.
- **Tested end to end.** `make serve-test` (`tests/serve-e2e.py`) drives a
  real `build/evo-agent serve` over HTTP only, against a stub Messages
  endpoint (`tests/stub-messages.py`): the ready file, auth, `/health`,
  snapshots, a stream and folding its ops onto the snapshot it started from,
  reconnecting with a cursor, `stream.reset` for a cursor that cannot be
  continued, `input.send`/`input.cancel`, rid idempotency, every error code,
  `command.run`, the goal operations, paging, `/catalog`, `/sessions`,
  `/debug/*`, a restart's new epoch and `--no-http-eval`.

## Example: a GUI-shaped client

Snapshot, stream from the snapshot's cursor, fold the ops, re-snapshot when
told to.

```python
import http.client, json

ready = json.load(open("/path/to/ready.json"))
PORT, TOKEN = ready["port"], ready["token"]

def call(method, path, body=None):
    c = http.client.HTTPConnection("127.0.0.1", PORT)
    headers = {"Authorization": "Bearer " + TOKEN}
    if body is not None:
        headers["Content-Type"] = "application/json"
    c.request(method, path, json.dumps(body) if body is not None else None, headers)
    return json.loads(c.getresponse().read())

snapshot = call("GET", "/snapshot?topics=session")
items = {i["id"]: i for i in snapshot["topics"]["session"]["items"]}
state = snapshot["topics"]["session"]["state"]

call("POST", "/ops", {"rid": "1", "op": "input.send", "args": {"text": "run the tests"}})

conn = http.client.HTTPConnection("127.0.0.1", PORT)
conn.request("GET", f"/stream?topics=session&since={snapshot['epoch']}.{snapshot['seq']}",
             headers={"Authorization": "Bearer " + TOKEN})
response = conn.getresponse()
while True:                                  # SSE frames: id / event / data
    frame = response.fp.readline()
    if frame == b"\n":                       # end of one frame
        continue
    if not frame:
        break
    field, _, value = frame.decode().rstrip("\n").partition(": ")
    if field != "data":
        continue
    op = json.loads(value)
    if op["op"] == "stream.reset":           # cannot continue: re-snapshot
        break
    if op["op"] == "item.add":
        items[op["item"]["id"]] = op["item"]
    elif op["op"] == "item.append":
        items[op["id"]][op["field"]] += op["text"]
    elif op["op"] == "item.patch":
        items[op["id"]].update(op["patch"])
    elif op["op"] == "state.patch":
        state.update(op["patch"])
```
