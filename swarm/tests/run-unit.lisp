;;;; swarm/tests/run-unit.lisp — run via: make test [LISP=sbcl|ecl]
;;;;
;;;; evo-swarm's unit suite, in its own image: it loads the swarm system,
;;;; which evo's suite (tests/run-unit.lisp) must never see.

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :evo-swarm :silent t)
(evo.port:setenv "EVO_HOME"
                 (namestring (uiop:ensure-directory-pathname
                              (merge-pathnames "evo-swarm-unit-home/"
                                               (uiop:temporary-directory)))))
(load (merge-pathnames "swarm/tests/unit.lisp" (uiop:getcwd)))
(evo.port:exit-lisp (evo.swarm.tests:run-all))
