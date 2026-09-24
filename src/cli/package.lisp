;;;; package.lisp — EVO.CLI, the entry point that composes core and TUI.

(defpackage :evo.cli
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:main #:setup-agent #:toplevel))
