;;;; build.lisp — build the evo executable (D2: images are build artifacts only).
;;;; Run: sbcl --non-interactive --load build.lisp

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :evo :silent t)

(defun evo-toplevel ()
  (sb-ext:disable-debugger)
  (sb-ext:exit :code (evo.cli:main)))

(ensure-directories-exist "build/")
(sb-ext:save-lisp-and-die "build/evo-core"
                          :executable t
                          :toplevel #'evo-toplevel)
