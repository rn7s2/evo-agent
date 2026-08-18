;;;; 100-permission-gate.lisp — example extension (seed corpus).
;;;;
;;;; Evo is permissive by design — there are no permission prompts.  This
;;;; example shows how the ONE interception point (:tool-call hooks)
;;;; builds a policy gate anyway: it blocks a denylist of catastrophic shell
;;;; patterns and rewrites nothing else.  Adapt the list; or invert it into
;;;; an allowlist for a hard sandbox.  Not loaded by default.

(in-package :evo.user)

(defparameter *gate-denylist*
  '("rm -rf /" "rm -rf ~" "rm -rf *" ":(){" "mkfs" "dd if=" "> /dev/sda"
    "chmod -R 777 /" "curl | sh" "wget | sh" "sudo rm")
  "Substrings that block a bash command outright.")

(evo:on :tool-call
        (lambda (call)
          (when (equal (getf call :name) "bash")
            (let ((command (or (getf (getf call :arguments) :command) "")))
              (loop for bad in *gate-denylist*
                    when (search bad command)
                      return (list :block t
                                   :reason (format nil "permission-gate: '~a' matches denied pattern '~a'"
                                                   command bad))))))
        :name :permission-gate)
