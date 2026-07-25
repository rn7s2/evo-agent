;;;; run-unit.lisp — run via: make test [LISP=sbcl|ecl]

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :evo :silent t)
;; Keep test sessions out of the real ~/.evo.
(evo.port:setenv "EVO_HOME"
                 (namestring (uiop:ensure-directory-pathname
                              (format nil "~a/evo-unit-home" (or (uiop:getenv "TMPDIR") "/tmp")))))
(load (merge-pathnames "tests/unit.lisp" (uiop:getcwd)))
(evo.port:exit-lisp (evo.tests:run-all))
