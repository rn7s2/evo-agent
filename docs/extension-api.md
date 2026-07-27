# evo extension API

Everything outside the core loop is an extension — including the TUI and todo
list you are running with. Extensions (yours included) use exactly this API;
nothing bypasses it. Your code lives in the `EVO.USER` package and is rebuilt
from source files on every boot, so **files are the truth**: fix a broken
runtime by fixing the file, never by poking memory.

Every extension file starts with:

```lisp
(in-package :evo.user)
```

## Registering tools

```lisp
(evo:register-tool "word-count"
  :description "Count words in a file."
  :schema '(:object
            (:path :type :string :description "File to count")
            (:by-line :type :boolean :optional t :description "Per-line counts"))
  :execute (lambda (args)
             (let ((text (uiop:read-file-string (getf args :path))))
               (format nil "~d words" (length (uiop:split-string text))))))
```

- Schema DSL: `(:object (prop :type <t> :description "..." [:optional t]
  [:enum (...)] [:items <schema>] [:properties (...)]) ...)` with types
  `:string :integer :number :boolean :object :array`. Property keywords use
  `-`, the wire uses `_` (`:by-line` ⇄ `by_line`).
- `:execute` gets a plist of arguments (same `-` convention) and returns the
  model-visible content string, optionally `(values content details)`.
- Signal a condition for errors — the loop converts it to an error tool
  result; never return garbage silently.
- A newly registered tool is callable from the next request. Re-registering
  the same name replaces it (CL redefinition: applies to the NEXT call, not
  frames already running).

## Commands

```lisp
(evo:register-command "stats"
  (lambda (ctx)            ; ctx: (:agent <agent> :args "string after /stats" :tui ...)
    "42 files, 8 todos")   ; a returned string is shown to the user
  :description "Show project stats")
```

## Event hooks

```lisp
(evo:on :tool-call (lambda (call) ...))   ; call: (:name "bash" :arguments (...))
```

- `:tool-call` — THE interception point. Return `nil` to allow,
  `(:block t :reason "...")` to block, `(:arguments <new>)` to rewrite.
  Permission gates and sandboxes are all built here — so is the bundled
  plan mode (`src/core-ext/plan-mode.lisp`), which uses nothing you cannot.
- `:transform-context` — receives the message list before each request;
  return a new list (filter/rewrite). Output is never written back to the
  journal.
- `:turn-end` — after each assistant turn: `(:agent a :message m)`.
- `:session-start` — `(:agent a :resumed bool)`. Rebuild any in-memory state
  from `evo:custom-state` here; memory does NOT survive restart, the journal
  does.
- `:todo-changed` — the todo list was replaced.

## State

```lisp
(evo:set-custom-state "my-key" #(1 2 3)) ; :custom entry — invisible to the LLM
(evo:custom-state "my-key")              ; current value (fold over the path)
(evo:inject-context "text" :key "my-key") ; :custom-message — VISIBLE to the LLM
```

State values must stay inside the journal vocabulary: plists, keywords,
strings, numbers, `t`/`nil`, vectors. No raw symbols, no objects, no
closures.

## Configuration (init.lisp)

Config is the same userspace code, evaluated from `~/.evo/init.lisp` then
`<project>/.evo/init.lisp` before the extension directories load. Unlike
extension `:load` entries, init files are **not journaled**: they are
environment, re-evaluated fresh on every boot (and `/reload`), with the
model/provider/settings registries reset first — so re-running them is
idempotent and an override is just a later call.

```lisp
(evo:register-model "claude-sonnet-5"       ; evo has NO built-in models
  :provider :anthropic :api :anthropic-messages
  :context-window 200000 :max-output 64000 :thinking t)
(evo:register-provider :anthropic           ; stock endpoints pre-seeded;
  :base-url "http://127.0.0.1:8787")        ; re-register merges field-wise
(evo:set-setting :model "claude-sonnet-5")  ; required — no default model
(evo:setting :model)                        ; read a setting
```

`:api` names a registered provider API — `:anthropic-messages` and
`:openai-responses` ship bundled, and you can add your own (below). Tools,
commands, and hooks may be registered from init files too.

## Provider APIs (new wire protocols)

A provider API is one wire protocol. Subclass `provider-api`, implement the
generics, register it — then any model can name it via `:api`. This is the
same path the bundled adapters take; nothing about them is privileged.

```lisp
(defclass myco-api (evo:provider-api) ())

(defmethod evo:endpoint-path ((api myco-api)) "/v1/chat")
(defmethod evo:auth-headers ((api myco-api) config)
  (list (cons "authorization" (format nil "Bearer ~a" (getf config :api-key)))))
(defmethod evo:build-request ((api myco-api) &key model system messages tools
                                                  thinking-level cache-key)
  ...)                                ; -> JSON request body string
(defmethod evo:parse-stream ((api myco-api) char-stream &key on-event abort-flag)
  ...)                                ; -> result plist; see src/provider/api.lisp
(defmethod evo:thinking-param ((api myco-api) level) nil)

(evo:register-api :myco-chat (make-instance 'myco-api))
(evo:register-model "myco-large" :provider :myco :api :myco-chat
  :context-window 128000 :max-output 8192)
(evo:register-provider :myco :base-url "https://api.myco.com"
  :api-key-env "MYCO_API_KEY")
```

The adapter contract `parse-stream` must honor — the result plist, the
events emitted via `:on-event`, and the errors-are-data rule — is specified
in the header of `src/provider/api.lisp`. `evo:perform-request` has a
default method that streams SSE over dexador; override it only for a
non-SSE framing. `evo:map-sse-events` is available if you want the default
SSE framing but your own event dispatch.

Optionally make the API self-seeding, so an env key alone is enough config
and users need no `register-provider` call:

```lisp
(defmethod evo:default-provider-key ((api myco-api)) :myco)
(defmethod evo:default-base-url ((api myco-api)) "https://api.myco.com")
(defmethod evo:default-api-key-env ((api myco-api)) "MYCO_API_KEY")
```

These three default to `nil` (seed nothing), so implementing them is
genuinely optional. Re-registering the same key replaces in place, which
is what keeps a reloaded extension idempotent.

## Other API

```lisp
evo:*agent*                       ; the live agent
(evo:steer "text")                ; queue a steering message (next turn boundary)
(evo:set-active-tools agent '("read" "bash"))  ; gate the tool set (nil = all)
(evo:all-tools)                   ; every registered tool name
(evo:current-goal)                ; goal plist or nil
(evo:load-extension "/path/x.lisp") ; compile+load+journal a source file
```

## Ground rules

- Kernel packages (`EVO.KERNEL`, `EVO.JOURNAL`, `EVO.PROVIDER`, …) are
  **locked**. You can read their exported functions; redefining them
  requires an explicit `evo.port:unlock-package` — allowed, journaled by your
  own actions, and on your head.
- Extension files live in `~/.evo/extensions/` (global) and
  `<project>/.evo/extensions/` — loaded at boot alphabetically, each load
  journaled as a `:load` entry and replayed on session resume.
- Introspect the live image freely: `describe`, `apropos`, `macroexpand`.
