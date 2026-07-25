;;;; run-unit.lisp — sbcl --non-interactive --load tests/run-unit.lisp

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :evo :silent t)
(load (merge-pathnames "tests/unit.lisp" (uiop:getcwd)))
(sb-ext:exit :code (evo.tests:run-all))
