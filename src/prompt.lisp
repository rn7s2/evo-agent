;;;; prompt.lisp — system prompt assembly (§12).
;;;;
;;;; Order: base -> tool one-liners -> guidelines -> own-docs paths -> lore
;;;; (post-MVP) -> project context files -> cwd.  Rebuilt on any tool-set
;;;; change (the loop rebuilds it every save point; it is cheap and pure).

(in-package :evo.kernel)

(defparameter *base-prompt*
  "You are evo, a goal-oriented software agent that runs in a terminal.
You accomplish tasks by calling tools: reading and writing files, running
shell commands, and managing your goal.  Work autonomously: when a task is
underway, keep going until it is done or you are truly blocked — do not stop
to ask permission for routine steps.")

(defparameter *guidelines*
  "Guidelines:
- Prefer tools over guessing: read files before editing them, verify with
  bash after changing things.
- Tool calls run sequentially; results come back as tool results.
- Keep file edits minimal and precise; use the edit tool for surgical
  changes, write for whole files.
- When a goal is active, every idle moment returns you to it.  Completion
  must be proven from current evidence (files, test output, runtime
  behavior), requirement by requirement — never from memory or intent.")

(defun context-files (&optional (cwd (uiop:getcwd)))
  "Walk / -> cwd collecting AGENTS.md / CLAUDE.md; nearest last."
  (let* ((dir (uiop:ensure-directory-pathname cwd))
         (dirs (loop for d = dir then (uiop:pathname-parent-directory-pathname d)
                     collect d
                     until (equal d (uiop:pathname-parent-directory-pathname d)))))
    (loop for d in (nreverse dirs)
          append (loop for name in '("AGENTS.md" "CLAUDE.md")
                       for path = (probe-file (merge-pathnames name d))
                       when path collect path))))

(defun build-system-prompt (tools &key (cwd (uiop:getcwd)))
  (with-output-to-string (out)
    (write-string *base-prompt* out)
    (format out "~2%## Tools~%")
    (dolist (tool tools)
      (format out "- ~a: ~a~%" (tool-name tool)
              (first (uiop:split-string (or (tool-description tool) "") :separator '(#\Newline)))))
    (format out "~%~a~%" *guidelines*)
    (let ((docs (probe-file (merge-pathnames "docs/" (evo-home)))))
      (when docs
        (format out "~%Your own documentation lives at: ~a~%" (namestring docs))))
    (dolist (path (context-files cwd))
      (let ((content (ignore-errors (read-file-string path))))
        (when (and content (plusp (length content)))
          (format out "~%## Context from ~a~%~a~%" (namestring path)
                  (truncate-string content 20000)))))
    (format out "~%Working directory: ~a~%Platform: ~a ~a~%"
            (namestring (uiop:ensure-directory-pathname cwd))
            (software-type) (software-version))))
