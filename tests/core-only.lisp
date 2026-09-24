;;;; core-only.lisp — run via: make test [LISP=sbcl|ecl] (before the unit suite)
;;;;
;;;; The dependency-direction guard.  "evo/core" is the agent without its
;;;; frontends (evo.asd); loading it alone in a fresh image proves no core file
;;;; reaches for the TUI or the CLI — a reference to either would not even
;;;; load, because their packages do not exist yet.

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload "evo/core" :silent t)

(let ((leaked (remove-if-not #'find-package '(:evo.tui :evo.cli))))
  (if leaked
      (format t "~&core-only: FAIL — loading evo/core also loaded ~{~a~^, ~}~%"
              leaked)
      (format t "~&core-only: ok — evo/core loads without a frontend~%"))
  (evo.port:exit-lisp (if leaked 1 0)))
