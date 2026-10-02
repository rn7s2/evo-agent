;;;; evo-only.lisp — run via: make test [LISP=sbcl|ecl] (before the unit suite)
;;;;
;;;; The second dependency-direction guard.  "evo" — the binary's system — must
;;;; load without evo-swarm: the swarm is its own program on top of evo
;;;; (evo-swarm.asd), and the evo binary carries none of it.  A reference from
;;;; evo to the swarm would not even load here, because EVO.SWARM does not
;;;; exist yet.
;;;;
;;;; Loading starts from a regular environment whatever started this run
;;;; (tests/env.lisp): what a caller's session exported must not decide what
;;;; this system loads.

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload "evo" :silent t)
(load (merge-pathnames "tests/env.lisp" (uiop:getcwd)))
(evo.test-env:clear-session-variables)

(let ((leaked (remove-if-not #'find-package '(:evo.swarm))))
  (if leaked
      (format t "~&evo-only: FAIL — loading evo also loaded ~{~a~^, ~}~%" leaked)
      (format t "~&evo-only: ok — evo loads without the swarm~%"))
  (evo.port:exit-lisp (if leaked 1 0)))
