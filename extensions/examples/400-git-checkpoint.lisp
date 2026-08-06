;;;; 400-git-checkpoint.lisp — example extension (seed corpus).
;;;;
;;;; File-state undo via `git stash create`, keyed by turn: after every turn
;;;; a stash commit captures the working tree without touching the index or
;;;; the stash list; /undo restores the tree of an earlier checkpoint.
;;;; Checkpoint records ride :custom journal state (rebuilt from the fold,
;;;; never from memory).  This is evo's answer to built-in file undo.

(in-package :evo.user)

(defun git-checkpoint--sh (command)
  (string-trim '(#\Newline #\Space)
               (with-output-to-string (out)
                 (uiop:run-program (list "/bin/sh" "-c" command)
                                   :output out :ignore-error-status t))))

(defun git-checkpoint--record (sha)
  (let* ((existing (or (evo:custom-state "git-checkpoint") #()))
         (updated (concatenate 'vector existing
                               (vector (list :sha sha :timestamp (get-universal-time))))))
    ;; Keep the last 50 checkpoints.
    (when (> (length updated) 50)
      (setf updated (subseq updated (- (length updated) 50))))
    (evo:set-custom-state "git-checkpoint" updated)))

(evo:on :turn-end
        (lambda (payload)
          (declare (ignore payload))
          (when (equal (git-checkpoint--sh "git rev-parse --is-inside-work-tree 2>/dev/null")
                       "true")
            (let ((sha (git-checkpoint--sh "git stash create 2>/dev/null")))
              (when (plusp (length sha))
                (git-checkpoint--record sha))))))

(evo:register-command "undo"
  (lambda (ctx)
    (declare (ignore ctx))
    (let ((checkpoints (or (evo:custom-state "git-checkpoint") #())))
      (if (zerop (length checkpoints))
          "no checkpoints yet (they are created after each turn with file changes)"
          (let ((sha (getf (aref checkpoints (1- (length checkpoints))) :sha)))
            (git-checkpoint--sh (format nil "git checkout ~a -- . 2>&1" sha))
            (evo:set-custom-state "git-checkpoint"
                                  (subseq checkpoints 0 (1- (length checkpoints))))
            (format nil "⎌ restored working tree from checkpoint ~a" sha)))))
  :description "Restore files from the last turn checkpoint")
