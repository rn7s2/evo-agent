;;;; package.lisp — EVO.SWARM, the evo-swarm program.
;;;;
;;;; A separate system on top of evo (evo-swarm.asd): nothing in the evo
;;;; binary names it, and `make test` proves so (tests/evo-only.lisp).  One
;;;; coordinator agent in this process's TUI; a pool of worker lanes, each an
;;;; `evo-agent serve` process driven only through serve's protocol (/ops,
;;;; /snapshot, /stream) and mirrored back as the topic `lane:N`.
;;;; See docs/swarm.md, design.md §18 and CONTRACT.md §4.3, §6.

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
           #:*swarm* #:swarm-lanes #:swarm-id #:swarm-dir #:swarm-state
           #:lane-n #:lane-state #:lane-task #:lane-cwd #:lane-worktree
           #:lane-topic #:find-lane #:tell-coordinator
           ;; the topics a client reads the swarm from (mirror.lisp, topics.lisp)
           #:make-mirror #:mirror-topic #:mirror-lane-state #:mirror-apply
           #:mirror-load #:mirror-rebuild #:mirror-items-after #:mirror-last-items
           #:register-swarm-topics #:publish-swarm-state #:lanes-busy-p
           #:interrupt-lane-now
           ;; the frontend seam (view.lisp): where notices and the run itself go
           #:view #:tui-view #:serve-view #:serve-view-server
           #:view-say #:view-repaint #:view-run
           #:swarm-view #:swarm-say #:swarm-repaint #:swarm-run
           ;; the swarm's commands, which both frontends get, and the TUI's
           ;; own status-line segment, which only it gets (tui.lisp)
           #:register-swarm-commands #:install-tui-observation
           ;; lifecycle, for the coordinator's tools and the tests
           #:launch-lane #:bring-up-lane #:restart-lane #:stop-lane
           #:start-lanes #:stop-swarm #:swarm-record #:record-swarm
           #:lane-snapshot #:lane-op #:lane-get))
