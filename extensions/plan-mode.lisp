;;;; plan-mode.lisp — the plan/auto modes, as a userspace extension (§12).
;;;;
;;;; Modes are policy in userspace, not kernel features: /plan gates the
;;;; tool set to read-only (+ allowlisted bash), injects instructions via a
;;;; hidden :custom-message, and /auto restores everything — the injected
;;;; instructions are filtered back out of context by a transform-context
;;;; hook when the mode is off.
;;;;
;;;; State discipline (§10): no in-memory mode flag — the mode lives in
;;;; :custom journal state under "mode", so it survives restart untouched.

(in-package :evo.user)

(defparameter *plan-read-tools* '("read" "bash" "get_goal" "todo"))

(defparameter *plan-bash-allowlist*
  '("ls" "cat" "head" "tail" "grep" "rg" "find" "wc" "pwd" "git" "file" "du" "stat" "which" "tree")
  "First words of bash commands allowed while planning.")

(defparameter *plan-instructions*
  "PLAN MODE is on. Do not modify anything: no writing or editing files, no
state-changing shell commands. Explore the code, then produce a concrete
step-by-step plan and present it to the user. When the user is satisfied
they will switch you back to auto mode (/auto) to execute.")

(defun plan-mode-p ()
  (equal (evo:custom-state "mode") "plan"))

(evo:on :tool-call
        (lambda (call)
          (when (plan-mode-p)
            (let ((name (getf call :name)))
              (cond
                ((member name '("write" "edit" "load_extension" "create_goal" "update_goal")
                         :test #'equal)
                 (list :block t :reason "plan mode: read-only — present a plan, then /auto"))
                ((equal name "bash")
                 (let* ((command (string-trim " " (or (getf (getf call :arguments) :command) "")))
                        (first-word (subseq command 0 (or (position #\Space command)
                                                          (length command)))))
                   (unless (member first-word *plan-bash-allowlist* :test #'equal)
                     (list :block t
                           :reason (format nil "plan mode: bash is allowlisted (~{~a~^ ~}); '~a' is not"
                                           *plan-bash-allowlist* first-word))))))))))

(evo:on :transform-context
        (lambda (messages)
          ;; Plan instructions vanish from context whenever the mode is off.
          (if (plan-mode-p)
              messages
              (remove-if (lambda (m)
                           (equal (getf (getf m :meta) :key) "plan-mode"))
                         messages))))

(evo:register-command "plan"
  (lambda (ctx)
    (let ((agent (getf ctx :agent)))
      (evo:set-custom-state "mode" "plan" agent)
      (evo:set-active-tools agent *plan-read-tools*)
      (evo:inject-context *plan-instructions* :key "plan-mode" :agent agent)
      "◇ plan mode — read-only; /auto to execute"))
  :description "Read-only planning mode")

(evo:register-command "auto"
  (lambda (ctx)
    (let ((agent (getf ctx :agent)))
      (evo:set-custom-state "mode" "auto" agent)
      (evo:set-active-tools agent nil)  ; nil = full tool set
      "◆ auto mode — full permissions"))
  :description "Fully permissive mode (default)")
