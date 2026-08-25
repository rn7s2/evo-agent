# Self-extension: giving yourself new abilities

If you lack a tool, write one, load it, keep going. The loop is:

1. **Write** a source file (use the `write` tool). It must start with
   `(in-package :evo.user)`. Keep one concern per file; files are the truth
   and get replayed on every boot.
2. **Load** it with `eval`: `(evo:load-extension "/path/to/file.lisp")`.
   Compilation errors come back as tool errors with the condition text — fix
   the file and load again. Redefinition applies from the NEXT call of a
   function, never to frames already running.
3. **Use** it: a tool registered with `evo:register-tool` is callable on
   your next turn.

The load is journaled (`:load` entry) and replayed on session resume, so
your extensions survive crashes and restarts as long as the file stays on
disk. Prefer `<project>/.evo/extensions/` for project-specific tools (they
also auto-load at boot) and one-off paths for experiments.

Bare `eval` is the fast path for anything you do not need to keep — compute a
number, check a predicate, look at a global. Nothing evaluated that way is
journaled, so a definition made in `eval` is gone at the next restart; the
file, and the `load-extension` that journals it, are what make it durable.

Boot-loaded files are loaded in file-name order, so name them
`NNN-name.lisp`: `000`–`099` for foundations others build on, `100`–`899`
for ordinary tools and hooks, `900`–`999` for wrappers that must load last.
Hooks run in registration order, so the rank orders those too.

## Example: a tool that fetches HTTP headers

```lisp
;; file: .evo/extensions/300-http-head.lisp
(in-package :evo.user)

(evo:register-tool "http_head"
  :description "Fetch the response headers for a URL (HEAD request)."
  :schema '(:object (:url :type :string :description "URL to probe"))
  :execute (lambda (args)
             (multiple-value-bind (body status headers)
                 (dex:head (getf args :url))
               (declare (ignore body))
               (format nil "HTTP ~a~%~{~a: ~a~%~}"
                       status
                       (loop for k being the hash-keys of headers
                               using (hash-value v)
                             append (list k v))))))
```

Then `eval` `(evo:load-extension ".evo/extensions/300-http-head.lisp")`, and
`http_head` exists.

## Goal predicates (`done-when`)

When you create a goal whose objective is mechanically checkable, formalize
it FIRST — before doing the work:

```lisp
;; file: .evo/extensions/300-goal-predicates.lisp
(in-package :evo.user)
(defun tests-pass-p ()
  (zerop (nth-value 2 (uiop:run-program "./test.sh"
                                        :ignore-error-status t))))
```

Load it, then `create_goal` with `done_when: "tests-pass-p"`. The kernel
runs the predicate when you claim completion and rejects the claim if it
fails — your completion claim becomes a checked assertion you wrote against
yourself, at a moment when you had no victory to declare.

## Debugging the live image

- `eval` is the debugger: it evaluates in the live image, not in a fresh
  Lisp a `bash` call would start, so `describe`, `apropos`, `macroexpand-1`
  and the value of any global answer for the runtime you are actually
  inside. Try a form there before you commit it to a file.
- The condition system is your error feedback: tool errors carry the
  condition's report string. Read it; it usually names the exact problem.
- Common Lisp pitfalls when generating code: keyword arguments need
  `&key`; `setf` on undefined places errors; strings are immutable-ish
  (use `concatenate`); prefer `uiop:` portability helpers for files and
  processes.
