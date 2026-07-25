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
  Permission gates, plan mode, and sandboxes are all built here.
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
