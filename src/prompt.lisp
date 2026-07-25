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

;;; Skills (§12): Agent Skills standard — SKILL.md + frontmatter, progressive
;;; disclosure: only name/description/path go into the prompt; the model
;;; reads the file on demand.

(defun parse-frontmatter (text)
  "Parse a leading '---' YAML-ish frontmatter block into a key->string alist."
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (when (and lines (string= (string-trim " " (first lines)) "---"))
      (loop for line in (rest lines)
            until (string= (string-trim " " line) "---")
            for colon = (position #\: line)
            when colon
              collect (cons (string-downcase (string-trim " " (subseq line 0 colon)))
                            (string-trim " " (subseq line (1+ colon))))))))

(defun skills-directories (&optional (cwd (uiop:getcwd)))
  (list (merge-pathnames "skills/" (evo-home))
        (merge-pathnames "skills/" (project-evo-dir cwd))))

(defun available-skills (&optional (cwd (uiop:getcwd)))
  "Plists (:name :description :path), project skills shadowing global ones."
  (let ((skills nil))
    (dolist (dir (skills-directories cwd) (nreverse skills))
      (dolist (skill-md (directory (merge-pathnames "*/SKILL.md" dir)))
        (let* ((text (ignore-errors (read-file-string skill-md)))
               (front (and text (parse-frontmatter text)))
               (name (or (cdr (assoc "name" front :test #'equal))
                         (car (last (pathname-directory skill-md))))))
          (setf skills (remove name skills :key (lambda (s) (pget s :name))
                                           :test #'equal))
          (push (list :name name
                      :description (or (cdr (assoc "description" front :test #'equal)) "")
                      :path (namestring skill-md))
                skills))))))

(defun find-skill (name &optional (cwd (uiop:getcwd)))
  (find name (available-skills cwd)
        :key (lambda (s) (pget s :name)) :test #'equal))

;;; Prompt templates (§12): .md files, filename = command, purely textual
;;; $1..$9 / $@ substitution.

(defun template-directories (&optional (cwd (uiop:getcwd)))
  (list (merge-pathnames "prompts/" (evo-home))
        (merge-pathnames "prompts/" (project-evo-dir cwd))))

(defun find-template (name &optional (cwd (uiop:getcwd)))
  (loop for dir in (reverse (template-directories cwd))
        for path = (probe-file (merge-pathnames (format nil "~a.md" name) dir))
        when path return path))

(defun expand-template (text args-string)
  (let ((words (remove "" (uiop:split-string args-string :separator '(#\Space))
                       :test #'equal))
        (result text))
    (flet ((sub (token value)
             (setf result (string-replace token value result :all t))))
      (loop for i from 9 downto 1     ; $9 before $1 so "$12" is not mangled
            do (sub (format nil "$~d" i)
                    (or (nth (1- i) words) "")))
      (sub "$@" args-string))
    result))

(defun build-system-prompt (tools &key (cwd (uiop:getcwd)) lore)
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
    (when lore
      ;; Lore (§9): injected every turn, immune to summarization.
      (format out "~%## Lore (durable user guidance — always applies)~%")
      (dolist (item lore)
        (format out "- ~a~%" item)))
    (dolist (path (context-files cwd))
      (let ((content (ignore-errors (read-file-string path))))
        (when (and content (plusp (length content)))
          (format out "~%## Context from ~a~%~a~%" (namestring path)
                  (truncate-string content 20000)))))
    (let ((skills (available-skills cwd)))
      (when skills
        (format out "~%<available_skills>~%")
        (dolist (skill skills)
          (format out "- ~a: ~a (read ~a before using)~%"
                  (pget skill :name) (pget skill :description) (pget skill :path)))
        (format out "</available_skills>~%")))
    (format out "~%Working directory: ~a~%Platform: ~a ~a~%"
            (namestring (uiop:ensure-directory-pathname cwd))
            (software-type) (software-version))))
