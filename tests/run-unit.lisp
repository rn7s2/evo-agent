;;;; run-unit.lisp — sbcl --non-interactive --load tests/run-unit.lisp

(require :asdf)
(require :sb-posix)
;; Keep test sessions out of the real ~/.evo.
(sb-posix:setenv "EVO_HOME"
                 (namestring (uiop:ensure-directory-pathname
                              (format nil "~a/evo-unit-home" (or (uiop:getenv "TMPDIR") "/tmp"))))
                 1)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :evo :silent t)
(load (merge-pathnames "tests/unit.lisp" (uiop:getcwd)))
(sb-ext:exit :code (evo.tests:run-all))
