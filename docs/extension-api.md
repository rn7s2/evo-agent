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

### Long-running tools and interruption

When the user presses Escape, the TUI thread calls `request-abort`, which sets
the agent's abort flag and runs every registered cleanup function. **Cleanups
run on the TUI thread, not on the worker thread executing your tool.** This is
a cross-thread data race for any resource tied to the spawning thread.

**Process handles are the worst case.** Both SBCL
(`sb-ext:process-kill`/`process-wait`/`process-alive-p`) and ECL
(`ext:terminate-process`/`ext:external-process-wait`/`ext:external-process-status`)
are unsafe to call from a different thread than the one that launched the
process. `process-wait` calls `waitpid(2)`, which is undefined behavior when
invoked concurrently from two threads on the same PID. The process struct's
status and exit-code slots are also unsynchronized — a reader on the worker
thread can see a torn state mid-write. This can crash the runtime.

**Closing a stream from another thread is also a data race** on the stream
reference, though the consequences are typically caught by `ignore-errors`
rather than crashing.

If your tool spawns a child process or holds any thread-local resource:

- **Do** poll `evo:*agent*`'s abort flag from the worker thread itself and
  kill/wait the process there.
- **Do not** register a `with-abort-cleanup` that calls `process-kill`,
  `process-wait`, or `close` on a resource you did not create on this thread.

The bundled `bash` tool (`src/kernel/builtin-tools.lisp`) follows this rule:
it polls the abort flag in its own loop and kills the child process on the
worker thread, never via a cross-thread cleanup.

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
(evo:register-model "deepseek-v4-pro"       ; evo has NO built-in models
  :provider :deepseek :api :anthropic-messages
  :context-window 1000000 :max-output 192000 :thinking t :effort t
  :vision nil)                              ; image input; t is the default,
                                            ; nil degrades images to text
(evo:register-provider :deepseek            ; :anthropic/:openai are pre-seeded,
  :base-url "https://api.deepseek.com/anthropic"   ; others you register;
  :api-key-env "DEEPSEEK_API_KEY")          ; re-registering merges field-wise
(evo:set-setting :model "deepseek-v4-pro")  ; required — no default model
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
(evo:steer "look" evo:*agent*     ; ... with images: :image content blocks from
           (list block))          ;     evo.media:attach-image-file / clipboard-image
(evo:set-active-tools agent '("read" "bash"))  ; gate the tool set (nil = all)
(evo:all-tools)                   ; every registered tool name
(evo:current-goal)                ; goal plist or nil
(evo:load-extension "/path/x.lisp") ; compile+load+journal a source file
```

## Images (evo.media)

```lisp
(evo.media:attach-image-file "/tmp/shot.png")  ; => (values BLOCK REASON)
(evo.media:clipboard-image)                    ; => (values BLOCK REASON)
(evo.media:pasted-image-paths text)            ; paths iff TEXT is only paths
evo.media:*max-image-bytes*                    ; cap before downscaling kicks in
evo.media:*clipboard-readers*                  ; ordered (NAME . FN) readers
evo.media:*downscalers*                        ; ordered (PASS PROGRAM ARGS-FN)
```

Errors are values here, not conditions: every entry point returns
`(values BLOCK REASON)` because the callers are keystroke handlers. To support
a platform evo does not ship a reader for, push onto `*clipboard-readers*` a
function of one argument (a scratch directory) that returns the pathname of an
image file it wrote there — or of a file the clipboard merely points at, which
is what a file-manager copy offers (`«class furl»` on macOS, `text/uri-list` on
X11/Wayland, `FileDropList` on Windows via the WSL bridge); only files inside
the scratch directory are deleted afterwards. Return `nil` for "not this
platform, or no image on the clipboard".

When every reader returns `nil`, `clipboard-image` explains which of the two
cases it was — an imageless clipboard, or a session with no way to read one —
via `evo.media::clipboard-gap`, a pure function of "what is this session" and
"which tools exist". Shipping a reader for a new platform means teaching that
function too, or the failure message will blame the clipboard.

## Ground rules

- Kernel packages (`EVO.KERNEL`, `EVO.JOURNAL`, `EVO.PROVIDER`, …) are
  **locked**. You can read their exported functions; redefining them
  requires an explicit `evo.port:unlock-package` — allowed, journaled by your
  own actions, and on your head.
- Extension files live in `~/.evo/extensions/` (global) and
  `<project>/.evo/extensions/` — loaded at boot alphabetically, each load
  journaled as a `:load` entry and replayed on session resume.
- Introspect the live image freely: `describe`, `apropos`, `macroexpand`.
