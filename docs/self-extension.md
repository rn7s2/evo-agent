# Self-extension: giving yourself new abilities

If you lack a tool, write one, load it, keep going. The loop is:

1. **Write** a source file (use the `write` tool). It must start with
   `(in-package :evo.user)`. Keep one concern per file; files are the truth
   and get replayed on every boot.
2. **Load** it with the `load_extension` tool. Compilation errors come back
   as tool errors with the condition text — fix the file and load again.
   Redefinition applies from the NEXT call of a function, never to frames
   already running.
3. **Use** it: a tool registered with `evo:register-tool` is callable on
   your next turn.

The load is journaled (`:load` entry) and replayed on session resume, so
your extensions survive crashes and restarts as long as the file stays on
disk. Prefer `<project>/.evo/extensions/` for project-specific tools (they
also auto-load at boot) and one-off paths for experiments.

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

Then call `load_extension` with that path, and `http_head` exists.

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

- `describe`, `apropos`, `inspect` work on the running runtime — via a
  quick `bash` + a fresh Lisp? No: you ARE the image. Write a tiny tool
  that calls them if you need programmatic introspection.
- The condition system is your error feedback: tool errors carry the
  condition's report string. Read it; it usually names the exact problem.
- Common Lisp pitfalls when generating code: keyword arguments need
  `&key`; `setf` on undefined places errors; strings are immutable-ish
  (use `concatenate`); prefer `uiop:` portability helpers for files and
  processes.
