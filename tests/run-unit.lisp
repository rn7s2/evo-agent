;;;; run-unit.lisp — run via: make test [LISP=sbcl|ecl]
;;;;
;;;; The suite starts from a regular environment whatever started it: the
;;;; session's own EVO_* variables are unset first (tests/env.lisp says why and
;;;; which ones are kept).

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :evo :silent t)
(load (merge-pathnames "tests/env.lisp" (uiop:getcwd)))
(evo.test-env:clear-session-variables)
;; Keep test sessions out of the real ~/.evo, in the OS temp directory.
;; UIOP:TEMPORARY-DIRECTORY is the portable answer: it honours TMPDIR/TMP where
;; set and is a real per-user temp directory on Windows — unlike the old "/tmp"
;; fallback, which only existed on Windows when C:\tmp happened to.
(evo.port:setenv "EVO_HOME"
                 (namestring (uiop:ensure-directory-pathname
                              (merge-pathnames "evo-unit-home/"
                                               (uiop:temporary-directory)))))
(load (merge-pathnames "tests/unit.lisp" (uiop:getcwd)))
(load (merge-pathnames "tests/responses.lisp" (uiop:getcwd)))
(evo.port:exit-lisp (evo.tests:run-all))
