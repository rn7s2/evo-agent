# evo journal format

One file per session: `~/.evo/sessions/<encoded-cwd>/<timestamp>_<id>.sexp`.
Line 1 is a header form; every following form is one entry. The file is an
append-only **tree**: entries are never modified or deleted; branching moves
the leaf pointer so the next append becomes a sibling. ALL session state —
messages, model, thinking level, active tools, goal, extension state — is a
fold over the root→leaf path.

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
| `:model-change` | `:provider` + `:model` (state fold) |
| `:thinking-change` | `:thinking` level (state fold) |
| `:tools-change` | `:tools` vector of active tool names (state fold) |
| `:compaction` | `:summary` + `:retained-tail` — self-contained checkpoint; context rebuild = [summary, …tail, …entries-after] |
| `:branch-summary` | summary of an abandoned branch |
| `:custom` | `:key`/`:data` extension state, INVISIBLE to the LLM |
| `:custom-message` | extension-injected content, visible to the LLM (`:key` lets a transform hook remove it later) |
| `:label` | bookmark (`:target-id` + `:label`) |
| `:session-info` | session name etc. |
| `:goal` | goal created/updated: `:goal-id :objective :status :token-budget :tokens-used [:done-when]` |
| `:load` | userspace source file loaded (`:path` + `:reason`) — replayed on boot |

Every entry carries `:id` (short random hex), `:parent-id` (nil for a root),
`:timestamp` (ISO-8601 UTC).

## Message plists

```lisp
(:role :user :content ((:type :text :text "...")))
(:role :assistant :api :anthropic-messages :provider :anthropic :model "..."
 :stop-reason :tool-use   ; :stop :length :tool-use :error :aborted
 :usage (:input i :output o :cache-read r :cache-write w :cost-usd rational)
 :content ((:type :thinking :thinking "..." :signature "...")
           (:type :text :text "...")
           (:type :tool-call :id "..." :name "bash" :arguments (:command "ls"))))
(:role :tool-result :tool-call-id "..." :tool-name "bash" :is-error nil
 :content ((:type :text :text "output")))
```

JSON arrays are CL **vectors**, JSON objects are plists — never mix.

## Why it matters to you

- Crash-safety: entries are appended (write-ahead) before being acted on.
  Kill the process at any moment; resume rebuilds everything from the file.
- Repairability: a corrupted runtime is fixed by editing/removing a source
  file named in a `:load` entry — never by surgery on opaque state.
- Your own state: use `:custom` entries (via `evo:set-custom-state`), and it
  survives restarts and compaction for free.
