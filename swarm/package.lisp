;;;; package.lisp — EVO.SWARM, the evo-swarm program.
;;;;
;;;; A separate system on top of evo (evo-swarm.asd): nothing in the evo
;;;; binary names it, and `make test` proves so (tests/evo-only.lisp).  One
;;;; coordinator agent in this process's TUI; a pool of worker lanes, each an
;;;; `evo-agent serve` process driven only through serve's public HTTP API.
;;;; See docs/swarm.md and design.md §18.

(defpackage :evo.swarm
  (:use :cl :evo.util :evo.journal :evo.provider :evo.kernel)
  (:export ;; the program
           #:main #:toplevel
           ;; swarm.lisp — code for lanes, lane count, tool limits, prompt notes
           #:in-lanes
           #:set-lane-tools #:set-coordinator-tools
           #:set-worker-note #:set-coordinator-note
           #:worker-note #:coordinator-note
           ;; the live swarm, for eval in the coordinator; what a lane is given
           #:baseline-forms
           #:*swarm* #:swarm-lanes #:swarm-id #:swarm-dir
           #:lane-n #:lane-state #:lane-task #:lane-cwd #:lane-worktree
           #:find-lane #:lane-eval #:lane-command #:tell-coordinator
           #:lane-report-origin #:lane-event-origin
           ;; the frontend seam (view.lisp): where notices, machine events and
           ;; the run itself go.  SWARM-VIEW is the swarm's own slot.
           #:view #:tui-view #:serve-view #:serve-view-server
           #:view-say #:view-repaint #:view-publish #:view-run
           #:swarm-view #:swarm-say #:swarm-repaint #:swarm-publish #:swarm-run
           ;; the swarm's commands, which both frontends get, and the TUI's
           ;; own status-line segment, which only it gets (tui.lisp)
           #:register-swarm-commands #:install-tui-observation
           ;; the read-only HTTP API (`evo-swarm serve`): lane goal cache,
           ;; GET /lanes, /lanes/N/transcript, /lanes/N/events (api.lisp,
           ;; routes.lisp)
           #:note-lane-goal #:note-lane-goal-status #:cached-lane-goal-status
           #:lane-state-event #:maybe-publish-lane-state
           #:lane-info #:swarm-summary #:swarm-lanes-response
           #:lane-transcript #:relay-lane-events #:copy-lane-events
           #:register-swarm-routes #:*swarm-identity*))
