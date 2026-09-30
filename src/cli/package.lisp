;;;; package.lisp — EVO.CLI, the entry point that composes core and TUI.

(defpackage :evo.cli
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export #:main #:setup-agent #:toplevel
           #:supervise #:supervised-run-p #:usage-error #:resolve-journal
           ;; serve's pieces, for a second program built on this CLI
           ;; (evo-swarm serve): the same flag, the same token rule, the same
           ;; flags a restarted child keeps.
           #:parse-port #:*serve-default-port* #:check-serve-ready
           #:serve-restart-flags #:pin-bound-port #:exact-session-args
           #:set-model-opt #:cmd-catalog))
