# evo journal format

One file per session: `~/.evo/sessions/<encoded-cwd>/<timestamp>_<id>.sexp`.
Line 1 is a header form (`:type :session`, `:id`, `:cwd`, `:program` — the
frontend that opened it, `evo-agent` or `evo-swarm` or `lane` — plus
`:swarm-id` for a coordinator and `:timestamp`); every following form is one
entry. The file is an append-only **tree**: entries are never modified or
deleted; branching moves the leaf pointer so the next append becomes a sibling.
ALL session state — messages, model, thinking level, active tools, goal,
extension state — is a fold over the root→leaf path.

`~/.evo/sessions/index.jsonl` is a cache of the headers, one JSON object per
line (last line per id wins: `id`, `path`, `cwd`, `program`, `swarm_id`,
`title` — the first user text, one line — `created_at`, `updated_at` (epoch
ms) and `entries`). It is what `evo-agent sessions --json` prints; it may be
stale, and `--rescan` rebuilds it from the files.

## Reading rules

Journals are data, not code: read with `*read-eval*` nil. The value
vocabulary is deliberately a "sexpr-JSON" subset — plists with keyword keys,
keywords, strings, integers, ratios/floats, `t`/`nil`, and vectors. No other
symbols, no objects, no cycles. Anything else is rejected loudly.

Writing keeps one form per line, but strings may contain newlines, so read
form-by-form, not line-by-line.

## Entry types

| type | meaning |
|---|---|
| `:message` | payload `:message` is a message plist (in LLM context) |
| `:model-change` | `:model` id, and `:provider` when the id is registered under more than one (state fold) |
| `:thinking-change` | `:thinking` level (state fold) |
| `:tools-change` | `:tools` vector of active tool names (state fold) |
| `:compaction` | `:summary` + `:retained-tail` — self-contained checkpoint; context rebuild = [summary, …tail, …entries-after]. Also `:tokens-before`, `:tokens-after` (what the context was and is) and `:manual` (`t` when a person asked: `/compact`, `context.compact`) |
| `:branch-summary` | summary of an abandoned branch |
| `:custom` | `:key`/`:data` extension state, INVISIBLE to the LLM |
| `:notice` | what was shown to the user: `:severity` (`:info`/`:warn`/`:error`), `:text`, `:source` (`:command`/`:extension`/`:swarm`/`:serve`/`:goal`) and `:data` — INVISIBLE to the LLM, ignored by the fold. The durable half of a frontend's output: goal transitions, a run's internal error, a compaction's result |
| `:custom-message` | extension-injected content, visible to the LLM (`:key` lets a transform hook remove it later) |
| `:label` | bookmark (`:target-id` + `:label`) |
| `:goal` | goal created/updated: `:goal-id :objective :status :token-budget :tokens-used` (`:token-budget` nil = no limit, the default) |
| `:load` | userspace source file loaded (`:path` + `:reason`) — replayed on boot |
| `:provider-retry` | a provider request re-sent: `:attempt :max :delay :reason` — for postmortems; the fold ignores it |
| `:recover` | the supervisor's account of how the previous run ended, appended by the child booting after a restart: `:status` (`:exited`/`:signaled`), `:code` (exit code or signal), `:attempt`, `:duration` (seconds), `:reason` (the supervisor's words, or nil) |

Every entry carries `:id` (short random hex), `:parent-id` (nil for a root),
`:timestamp` (ISO-8601 UTC). An id may be **pre-minted**: a frontend that has
to name an entry before it exists — a streaming message, a queued input —
mints one and passes it to `APPEND-ENTRY :id`, so the row it drew and the
entry on disk are the same thing.

A `:message` entry may carry `:origin`: a plist saying who injected it when
nobody typed it, invisible to the model. `(:kind :user)` or absent is the
person; otherwise `:kind` names the injector — `:lane-report`, `:lane-event`,
`:goal`, `:command-note`, `:human-action`, `:context`, `:recovery` — with its
own fields (`(:kind :goal :event :continue :goal-id "g-1" :objective "…")`).

`:recover` folds into `custom-state "recovery"` and is accompanied by a
`:custom-message` (user role, key `"recovery"`) stating the same facts in one
sentence — the transcript's record of why the run before it ended.

## Message plists

```lisp
(:role :user :content ((:type :image :media-type "image/png" :data "<base64>"
                        :name "shot.png" :bytes 12345 :source "clipboard")
                       (:type :text :text "...")))
(:role :assistant :api :anthropic-messages :provider :anthropic :model "..."
 :stop-reason :tool-use   ; :stop :length :tool-use :error :aborted
 :usage (:input i :output o :cache-read r :cache-write w)  ; older journals may add :cost-usd — ignored
 :content ((:type :thinking :thinking "..." :signature "...")
           (:type :text :text "...")
           (:type :tool-call :id "..." :name "bash" :arguments (:command "ls"))))
(:role :tool-result :tool-call-id "..." :tool-name "bash" :is-error nil
 :content ((:type :text :text "output")))
```

JSON arrays are CL **vectors**, JSON objects are plists — never mix.

An `:image` block carries its bytes inline (base64), so a session is
self-contained: no sidecar files to lose, and a resumed transcript still shows
the model what it saw. `:name`, `:bytes` and `:source` are for humans and the
UI; the wire only needs `:media-type` and `:data`. That is also why
`evo.media:*max-image-bytes*` exists — an unbounded paste would be an
unbounded session file.

## Why it matters to you

- Crash-safety: entries are appended (write-ahead) before being acted on.
  Kill the process at any moment; resume rebuilds everything from the file.
- Repairability: a corrupted runtime is fixed by editing/removing a source
  file named in a `:load` entry — never by surgery on opaque state.
- Your own state: use `:custom` entries (via `evo:set-custom-state`), and it
  survives restarts and compaction for free.
