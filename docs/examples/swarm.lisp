;;;; swarm.lisp — a sample evo-swarm configuration.  Reference only: copy it to
;;;; ~/.evo/swarm.lisp (every swarm) or <project>/.evo/swarm.lisp (this
;;;; project's swarms) and edit.  See docs/swarm.md.
;;;;
;;;; evo-swarm evaluates ~/.evo/swarm.lisp then <project>/.evo/swarm.lisp in
;;;; the coordinator's image, after init.lisp, the extensions and
;;;; post-init.lisp.  Like init.lisp it is Lisp, not data: what a lane becomes
;;;; is a program.  Each lane boots as `evo serve --no-userspace` — kernel and
;;;; core extensions only — and then evaluates, in order, the forms returned by
;;;; every worker-init GENERATOR: a function of the lane and the swarm.  The
;;;; default generator, :BASELINE, gives each lane the coordinator's models and
;;;; providers (keys by environment-variable name only), its model and
;;;; thinking level, the `report` tool and the lane's prompt note.

;;; How many lanes, when --workers is not given (default 6).
(evo:set-setting :swarm-workers 4)

;;; Add to every lane: here, an extension file of project tools.  Generators
;;; return FORMS, evaluated in the lane (package EVO.USER) — so anything a
;;; lane should have can be expressed, and nothing in the coordinator's image
;;; leaks into it by accident.
(evo.swarm:add-worker-init :project-tools
  (lambda (lane swarm)
    (declare (ignore lane swarm))
    (let ((tools (merge-pathnames ".evo/lane-tools.lisp" (uiop:getcwd))))
      (when (probe-file tools)
        `((evo:load-extension ,(namestring tools)))))))

;;; Different lanes, different setups: odd lanes also get a cheaper default
;;; model for routine work (it must be registered in init.lisp, and the
;;; baseline registers every model the coordinator knows).
;; (evo.swarm:add-worker-init :cheap-odd-lanes
;;   (lambda (lane swarm)
;;     (declare (ignore swarm))
;;     (when (oddp (evo.swarm:lane-n lane))
;;       '((evo:set-setting :model "claude-sonnet-5")))))

;;; An MCP server for every lane: install the extension's registration the
;;; same way it would be installed in init.lisp.
;; (evo.swarm:add-worker-init :mcp
;;   (lambda (lane swarm)
;;     (declare (ignore lane swarm))
;;     '((evo:load-extension "/home/me/.evo/extensions/500-mcp.lisp"))))

;;; Tool limits.  The coordinator has every tool unless limited; lanes too.
;;; The report tool is always kept for lanes.
;; (evo.swarm:set-lane-tools '("read" "write" "edit" "bash" "wait" "todo"))
;; (evo.swarm:set-lane-tools '("read" "bash") :lanes '(5 6))  ; read-only-ish lanes
;; (evo.swarm:set-coordinator-tools '("read" "bash" "lanes" "delegate" "steer_lane"
;;                                    "interrupt_lane" "interrupt_and_steer" "lane_command"
;;                                    "lane_eval" "lane_transcript" "lane_reports"
;;                                    "restart_lane" "lane_worktree"))

;;; Behavior lives in prompt notes.  Replace either wholesale; each is a FORMAT
;;; control: the coordinator's takes the lane count, a lane's takes its number,
;;; its number again, the lane count, and a sentence about where it works.
;; (evo.swarm:set-worker-note
;;  "## Swarm lane ~d~%You are lane ~d of ~d. ~a Report with the `report` tool
;; after every commit, and run the tests before you report done.~%")

;;; Or build lanes from scratch: drop the baseline and supply your own — but
;;; then providers, models and the report tool are yours to give them.
;; (evo.swarm:remove-worker-init :baseline)
;; (evo.swarm:add-worker-init :mine
;;   (lambda (lane swarm)
;;     (append (evo.swarm:default-worker-init lane swarm)
;;             '((evo:register-prompt-note "house-rules" "Never push to main.")))))
