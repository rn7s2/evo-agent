;;;; swarm.lisp — a sample evo-swarm configuration.  Reference only: copy it to
;;;; ~/.evo/swarm.lisp (every swarm) or <project>/.evo/swarm.lisp (this
;;;; project's swarms) and edit.  See docs/swarm.md.
;;;;
;;;; evo-swarm reads this in the coordinator, as the last step of the files
;;;; evo itself reads (init.lisp, extensions, post-init.lisp) — and again on
;;;; /reload.  Plain `evo` never reads it.  It configures the swarm: the
;;;; coordinator's own setup for the swarm, how many lanes, what the lanes run
;;;; beyond what they inherit, and how the coordinator shapes them.

;;; The coordinator.  Any init.lisp call works here and overrides your init
;;; files for the swarm only.  Lanes inherit the coordinator's providers,
;;; models, model and thinking level.
;; (evo:set-setting :model "claude-opus-5")
;; (evo:set-setting :thinking :high)

;;; How many lanes, when --workers is not given (default 6).
(evo:set-setting :swarm-workers 4)

;;; Code every lane evaluates — not here, in each lane — before it gets any
;;; work and whenever it restarts.  LANE is bound to the lane's number
;;; (1..LANES), LANES to the lane count; name only what you use, or () for
;;; neither.  Needed whenever the coordinator's models or tools come from an
;;; extension: with Claude OAuth, for one, the extension defines the API its
;;; models use, and a lane without it cannot register them.  Keys: use
;;; :api-key-env, never a literal :api-key — these forms run in the lane as
;;; written.
;; (evo.swarm:in-lanes (lane lanes)
;;   (load "~/.evo/extensions/020-claude-oauth-provider.lisp")
;;   (when (<= lane 2)
;;     (evo:set-setting :model "claude-sonnet-5")))

;;; Tool limits for the lanes: every lane, or the ones named.  The report tool
;;; is always kept.
;; (evo.swarm:set-lane-tools '("read" "write" "edit" "bash" "wait" "todo"))
;; (evo.swarm:set-lane-tools '("read" "bash") :lanes '(5 6))

;;; The lanes' prompt note: a FORMAT control taking the lane's number, its
;;; number again, the lane count, and a sentence on where it works.
;; (evo.swarm:set-worker-note
;;  "## Swarm lane ~d~%You are lane ~d of ~d. ~a Run the tests before you report done.~%")

;;; The coordinator itself: its tools and its prompt note (a FORMAT control
;;; taking the lane count).
;; (evo.swarm:set-coordinator-tools '("read" "bash" "lanes" "delegate" "steer_lane"
;;                                    "interrupt_lane" "interrupt_and_steer" "lane_command"
;;                                    "lane_eval" "lane_transcript" "lane_reports"
;;                                    "restart_lane" "lane_worktree"))
