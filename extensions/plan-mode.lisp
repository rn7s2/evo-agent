;;;; plan-mode.lisp — plan-mode enforcement, as a userspace extension.
;;;;
;;;; The TUI switches modes (shift+tab, /mode): it sets the "mode" custom
;;;; state, gates the tool set, and injects the plan instructions.  This
;;;; extension adds the enforcement hooks on top: a :tool-call gate that
;;;; blocks mutations (+ allowlists bash) while planning, and a
;;;; transform-context hook that filters the injected instructions back out
;;;; of context when the mode is off.
;;;;
;;;; State discipline: no in-memory mode flag — the mode lives in
;;;; :custom journal state under "mode", so it survives restart untouched.

(in-package :evo.user)

(defparameter *plan-bash-allowlist*
  '("ls" "cat" "head" "tail" "grep" "rg" "find" "wc" "pwd" "git" "file" "du" "stat" "which" "tree")
  "First words of bash commands allowed while planning.")

(defun plan-mode-p ()
  (equal (evo:custom-state "mode") "plan"))

(evo:on :tool-call
        (lambda (call)
          (when (plan-mode-p)
            (let ((name (getf call :name)))
              (cond
                ((member name '("write" "edit" "load_extension" "create_goal" "update_goal")
                         :test #'equal)
                 (list :block t :reason "plan mode: read-only — present a plan; the user switches back with shift+tab or /mode"))
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
