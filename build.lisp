;;;; build.lisp — build the single evo executable (images are build
;;;; artifacts only).  Run via: make build
;;;;
;;;; The Makefile invokes sbcl with --dynamic-space-size 4096 and we save
;;;; with :save-runtime-options t, so the heap size is baked into the binary
;;;; and ALL argv goes to evo's own main — no launcher, no
;;;; --end-runtime-options.

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :evo :silent t)

(defun evo-toplevel ()
  (sb-ext:disable-debugger)
  (sb-ext:exit :code (evo.cli:main)))

(ensure-directories-exist "build/")
(sb-ext:save-lisp-and-die "build/evo"
                          :executable t
                          :save-runtime-options t
                          :toplevel #'evo-toplevel)
