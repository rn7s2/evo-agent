;;;; package.lisp — EVO.SWARM, the evo-swarm program.
;;;;
;;;; A separate system on top of evo (evo-swarm.asd): nothing in the evo
;;;; binary names it, and `make test` proves so (tests/evo-only.lisp).  One
;;;; coordinator agent in this process's TUI; a pool of worker lanes, each an
;;;; `evo serve` process driven only through serve's public HTTP API.  See
;;;; docs/swarm.md and design.md §18.

(defpackage :evo.swarm
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export ;; the program
           #:main #:toplevel
           ;; swarm.lisp — code for lanes, lane count, tool limits, prompt notes
           #:in-lanes #:baseline-forms
           #:set-lane-tools #:set-coordinator-tools
           #:set-worker-note #:set-coordinator-note
           #:worker-note #:coordinator-note
           ;; the live swarm, for eval in the coordinator
           #:*swarm* #:swarm-lanes #:swarm-id #:swarm-dir
           #:lane-n #:lane-state #:lane-task #:lane-cwd #:lane-worktree
           #:find-lane #:lane-eval #:lane-command #:tell-coordinator))
