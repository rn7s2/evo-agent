;;;; prompt.lisp — system prompt assembly.
;;;;
;;;; Order: base -> tool one-liners -> guidelines -> own-docs paths -> lore
;;;; (post-MVP) -> project context files -> environment.  Rebuilt on any
;;;; tool-set change (the loop rebuilds it every save point; cheap and pure).

(in-package :evo.kernel)

;;; Language packs.
;;;
;;; Every word the MODEL reads is language-owned: the base prompt, the
;;; guidelines, the headings around them.  The kernel holds none of that
;;; prose — it holds the assembly order and this registry, and a pack
;;; supplies the text (English is a core extension, like todo or memory).
;;; The scope is deliberately narrow: what the model is told and what it
;;; answers in.  Evo's own interface — commands, help, status line, tool
;;; descriptions — is not translated and does not belong here.

(defvar *prompt-languages* nil
  "Alist of (CODE . PACK) in registration order.  PACK is a plist:
:code :name :native :response-language :sections.")

(defparameter *default-language* "en"
  "Code of the pack every other pack falls back to, section by section.")

(defparameter *prompt-sections*
  '(:base :guidelines :own-docs :environment :git-status :respond-in
    :tools-heading :lore-heading :context-heading)
  "The sections a pack may supply.  A pack that omits one gets the default
language's text for it, so a partial or outdated translation still yields a
complete prompt instead of a hole.")

(defun language-code (code)
  "Canonical form of a language code.  \"zh-CN\", \"zh-cn\" and :zh-cn all
name one pack."
  (string-downcase (string code)))

(defun register-prompt-language (code &key name native response-language sections)
  "Register (or replace) the prompt language CODE — a BCP-47-ish tag such as
\"en\" or \"zh-CN\".  NAME is its English name, NATIVE its endonym (what the
picker shows), RESPONSE-LANGUAGE the name the model is told to answer in, and
SECTIONS a plist of section key -> template text (see *PROMPT-SECTIONS*;
`{{TOKEN}}` placeholders are rendered exactly as in the built-in sections).
Re-registration replaces in place, so reloading an extension is idempotent."
  (let ((unknown (loop for (key nil) on sections by #'cddr
                       unless (member key *prompt-sections*) collect key)))
    (when unknown
      (error "register-prompt-language ~a: unknown section~p ~{~s~^ ~} (known: ~{~s~^ ~})"
             code (length unknown) unknown *prompt-sections*)))
  (let* ((key (language-code code))
         (pack (list :code key
                     :name (or name (string code))
                     :native (or native name (string code))
                     :response-language response-language
                     :sections sections))
         (existing (assoc key *prompt-languages* :test #'string=)))
    (if existing
        (setf (cdr existing) pack)
        (setf *prompt-languages* (append *prompt-languages* (list (cons key pack)))))
    pack))

(defun find-prompt-language (code)
  "The pack named by CODE, or NIL.  A free-text hint like \"Korean\" names no
pack — that is the distinction between switching the prompt and asking for a
reply in some language."
  (and code (cdr (assoc (language-code code) *prompt-languages* :test #'string=))))

(defun all-prompt-languages ()
  "Registered packs, registration order with the default language first."
  (let* ((packs (mapcar #'cdr *prompt-languages*))
         (default (find-prompt-language *default-language*)))
    (if default (cons default (remove default packs)) packs)))

(defun prompt-section (key &optional language)
  "Template text for section KEY in LANGUAGE (a pack, a code, or NIL for the
default), falling back to the default language and erroring only when no pack
supplies it at all — a silently empty prompt is the one outcome worth
crashing over."
  (let* ((pack (if (consp language) language (find-prompt-language language)))
         (fallback (find-prompt-language *default-language*)))
    (or (getf (getf pack :sections) key)
        (getf (getf fallback :sections) key)
        (error "No prompt language supplies section ~s (registered: ~{~a~^ ~}).~:[ Load the ~a language pack.~;~]"
               key (mapcar #'car *prompt-languages*) fallback *default-language*))))

;;; Which language is in force.  Precedence mirrors the model's: what the
;;; user picked this session (journaled, so it survives restart and
;;; compaction) beats the :language setting from init.lisp, which beats the
;;; default pack.

(defun language-request (&optional state)
  "The language the user asked for as a string, or NIL if they never said.
A code naming a registered pack switches the prompt; any other string is a
response-language hint on the default pack."
  (flet ((non-empty (x)
           (let ((s (and x (string x))))
             (and s (plusp (length s)) s))))
    (or (non-empty (and state (custom-state state "language")))
        (non-empty (setting :language)))))

(defun resolve-language (request)
  "(values PACK RESPONSE-LANGUAGE) for REQUEST.  RESPONSE-LANGUAGE is NIL
when the user asked for nothing: an unconfigured session gets no directive
about what to answer in, which is how it behaved before packs existed."
  (let ((pack (and request (find-prompt-language request))))
    (values (or pack (find-prompt-language *default-language*))
            (and request (or (getf pack :response-language) request)))))

(defun set-prompt-language (code &optional agent)
  "Switch the prompt/response language to CODE.  Without an AGENT this sets
the session default (what init.lisp does); with one the choice is journaled,
so it outlives a restart and a compaction.  CODE that names no pack is kept
as a response-language hint rather than refused."
  (let ((code (and code (string code))))
    (if agent
        (append-entry (agent-journal agent)
                      (list :type :custom :key "language" :data code))
        (set-setting :language code))
    code))

;;; Templating.  Every section evo owns is a template: `{{NAME}}` tokens are
;;; the injection points where facts about the running environment reach the
;;; model.  Only evo's own sections are rendered — never the lore, context
;;; files, or skill text, so a `{{...}}` sitting in a user's CLAUDE.md is
;;; passed through verbatim rather than expanded behind their back.

(defun render-template (template bindings)
  "Substitute {{NAME}} in TEMPLATE from BINDINGS, a (name . value) alist.
An unknown token is left standing, so a missing binding shows up in the
prompt instead of vanishing into an empty string."
  (let ((result template))
    (dolist (binding bindings result)
      (setf result (string-replace (format nil "{{~a}}" (car binding))
                                   (cdr binding) result :all t)))))

(defun today-string ()
  "Local calendar date.  Date only, never the clock: the prompt prefix is
cached upstream and a ticking timestamp in it would miss on every turn."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun git-dir (&optional (cwd (uiop:getcwd)))
  "The .git of CWD or its nearest ancestor, or NIL.  In a worktree or
submodule .git is a file holding `gitdir: <path>`; follow it."
  (let ((dot-git (loop for d = (uiop:ensure-directory-pathname cwd)
                         then (uiop:pathname-parent-directory-pathname d)
                       thereis (probe-file (merge-pathnames ".git" d))
                       until (equal d (uiop:pathname-parent-directory-pathname d)))))
    (cond ((null dot-git) nil)
          ((uiop:directory-pathname-p dot-git) dot-git)
          (t (let ((line (string-trim '(#\Space #\Newline #\Return)
                                      (or (ignore-errors (read-file-string dot-git)) ""))))
               (when (string-prefix-p "gitdir:" line)
                 (let ((target (string-trim " " (subseq line (length "gitdir:")))))
                   (probe-file
                    (uiop:ensure-directory-pathname
                     (if (uiop:absolute-pathname-p target)
                         target
                         (merge-pathnames target
                                          (uiop:pathname-directory-pathname dot-git))))))))))))

(defun git-branch (&optional (cwd (uiop:getcwd)))
  "Current branch name, read straight out of .git/HEAD — no subprocess, so
this stays cheap enough to run on every prompt rebuild.  NIL outside a repo;
a detached HEAD reports its short sha."
  (let* ((dir (git-dir cwd))
         (head (and dir (probe-file (merge-pathnames "HEAD" dir))))
         (line (and head (string-trim '(#\Space #\Newline #\Return)
                                      (or (ignore-errors (read-file-string head)) "")))))
    (cond ((null line) nil)
          ((string-prefix-p "ref: refs/heads/" line)
           (subseq line (length "ref: refs/heads/")))
          ((plusp (length line)) (subseq line 0 (min 8 (length line))))
          (t nil))))

;;; gitStatus.  The one part of the prompt that shells out, so it is taken
;;; once per process and cached: the snapshot describes the tree as the
;;; session found it, which is what the model is told, and which is why a
;;; rebuild at every save point costs nothing after the first.

(defun git-output (cwd &rest args)
  "Trimmed stdout of a git command in CWD, or NIL if it produced nothing."
  (let* ((raw (ignore-errors
                (uiop:run-program (cons "git" args)
                                  :directory (uiop:ensure-directory-pathname cwd)
                                  :output :string :error-output nil
                                  :ignore-error-status t)))
         (out (and raw (string-trim '(#\Space #\Newline #\Return) raw))))
    (and out (plusp (length out)) out)))

(defun git-default-branch (cwd)
  "The branch PRs normally target: origin's HEAD, else main, else master."
  (let ((origin-head (git-output cwd "symbolic-ref" "--short" "refs/remotes/origin/HEAD")))
    (cond (origin-head (let ((slash (position #\/ origin-head)))
                         (if slash (subseq origin-head (1+ slash)) origin-head)))
          ((git-output cwd "rev-parse" "--verify" "--quiet" "refs/heads/main") "main")
          ((git-output cwd "rev-parse" "--verify" "--quiet" "refs/heads/master") "master"))))

(defvar *git-status-cache* (make-hash-table :test #'equal)
  "cwd -> snapshot string (or NIL outside a repo).  Never invalidated: a
fresh process re-snapshots, which is exactly the intended lifetime.")

(defun git-status-snapshot (&optional (cwd (uiop:getcwd)))
  (let ((key (namestring (uiop:ensure-directory-pathname cwd))))
    (multiple-value-bind (cached present) (gethash key *git-status-cache*)
      (if present
          cached
          (setf (gethash key *git-status-cache*)
                (when (git-dir cwd)
                  (format nil (cat "Current branch: ~a~2%"
                                   "Main branch (you will usually use this for PRs): ~a~2%"
                                   "Status:~%~a~2%"
                                   "Recent commits:~%~a")
                          (or (git-branch cwd) "(detached)")
                          (or (git-default-branch cwd) "unknown")
                          (truncate-string (or (git-output cwd "status" "--short")
                                               "(clean)")
                                           2000)
                          (or (git-output cwd "log" "--oneline" "-5") "(none)"))))))))

(defun prompt-bindings (&key (cwd (uiop:getcwd)) model response-language)
  "The facts injected into the prompt templates.  Add a placeholder here and
it becomes available to every section evo owns."
  (list (cons "WORKING_DIRECTORY"
              (namestring (uiop:ensure-directory-pathname cwd)))
        (cons "IS_GIT_REPO" (if (git-dir cwd) "yes" "no"))
        (cons "GIT_BRANCH" (or (git-branch cwd) "n/a"))
        (cons "PLATFORM" (string (software-type)))
        ;; Named explicitly because the tool is called `bash` everywhere and
        ;; is not bash anywhere: a model that assumes /bin/sh on Windows
        ;; writes commands that cannot run, and it has no other way to know.
        (cons "SHELL" (evo.port:shell-name))
        (cons "OS_VERSION" (string (software-version)))
        (cons "TODAY_DATE" (today-string))
        (cons "MODEL" (or model "unknown"))
        (cons "GIT_STATUS" (or (git-status-snapshot cwd) ""))
        (cons "RESPONSE_LANGUAGE" (or response-language ""))
        (cons "EVO_DOCS" (namestring (merge-pathnames "docs/" (evo-home))))))

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

;;; Skills: Agent Skills standard — SKILL.md + frontmatter, progressive
;;; disclosure: only name/description/path go into the prompt; the model
;;; reads the file on demand.

(defparameter *yaml-blanks* '(#\Space #\Tab #\Return))
(defparameter *yaml-space* '(#\Space #\Tab #\Newline #\Return))

(defun frontmatter-lines (text)
  "Raw lines between the leading '---' fence and its closing '---', or NIL."
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (when (and lines (string= (string-trim *yaml-blanks* (first lines)) "---"))
      (loop for line in (rest lines)
            until (member (string-trim *yaml-blanks* line) '("---" "...") :test #'string=)
            collect (string-right-trim '(#\Return) line)))))

(defun yaml-indentation (line)
  "Column of the first non-blank character, or NIL when the line is blank."
  (position-if (lambda (c) (not (member c '(#\Space #\Tab)))) line))

(defun yaml-block-style (header)
  "For a block-scalar header (`|`, `>-`, `|2+`, ...) return :literal or :folded."
  (when (plusp (length header))
    (let ((style (case (char header 0) (#\| :literal) (#\> :folded) (t nil))))
      ;; Everything after the indicator is chomping/indent flags or a comment;
      ;; a stray word there means this was not a block header after all.
      (when (and style
                 (let ((rest (string-trim *yaml-blanks* (subseq header 1))))
                   (or (zerop (length rest))
                       (char= (char rest 0) #\#)
                       (every (lambda (c) (find c "+-0123456789")) rest))))
        style))))

(defun take-indented-block (lines indent)
  "Pop the lines belonging to a key at INDENT: blanks and deeper-indented text.
Returns (values block-lines remaining-lines)."
  (let ((block '()))
    (loop while lines
          for col = (yaml-indentation (first lines))
          while (or (null col) (> col indent))
          do (push (pop lines) block))
    (values (nreverse block) lines)))

(defun strip-block-indent (lines)
  "Drop the common leading indentation shared by the block's non-blank lines."
  (let ((base (loop for line in lines
                    for col = (yaml-indentation line)
                    when col minimize col)))
    (mapcar (lambda (line) (subseq line (min base (length line)))) lines)))

(defun join-block (lines style)
  "Literal blocks keep their line breaks; folded blocks join with spaces and
turn blank lines into breaks — YAML folding, minus the corner cases."
  (if (eq style :literal)
      (format nil "~{~a~^~%~}" lines)
      (with-output-to-string (out)
        (let ((started nil) (break-pending nil))
          (dolist (line lines)
            (if (null (yaml-indentation line))
                (when started (setf break-pending t))
                (progn (cond (break-pending (write-char #\Newline out)
                                            (setf break-pending nil))
                             (started (write-char #\Space out)))
                       (write-string (string-trim *yaml-blanks* line) out)
                       (setf started t))))))))

(defun unquote-scalar (s)
  "Strip matching YAML quotes and undo the escapes inside them."
  (let ((len (length s)))
    (cond ((and (>= len 2) (char= (char s 0) #\") (char= (char s (1- len)) #\"))
           (let ((body (subseq s 1 (1- len))))
             (with-output-to-string (out)
               (loop with i = 0
                     while (< i (length body))
                     for c = (char body i)
                     do (if (and (char= c #\\) (< (1+ i) (length body)))
                            (let ((next (char body (1+ i))))
                              (write-char (case next (#\n #\Newline) (#\t #\Tab) (t next)) out)
                              (incf i 2))
                            (progn (write-char c out) (incf i)))))))
          ((and (>= len 2) (char= (char s 0) #\') (char= (char s (1- len)) #\'))
           (string-replace "''" "'" (subseq s 1 (1- len)) :all t))
          (t s))))

(defun frontmatter-value (raw block)
  "Value for one key: RAW is the text after the colon, BLOCK its owned lines."
  (let ((style (yaml-block-style raw)))
    (if style
        (string-trim *yaml-space* (join-block (strip-block-indent block) style))
        ;; Plain or quoted, possibly wrapped across continuation lines; nested
        ;; mappings and lists fold into one string rather than leaking their
        ;; inner keys to the top level.
        (unquote-scalar
         (string-trim *yaml-space*
                      (join-block (if (plusp (length raw))
                                      (cons raw (strip-block-indent block))
                                      (strip-block-indent block))
                                  :folded))))))

(defun parse-frontmatter (text)
  "Parse a leading '---' YAML frontmatter block into a key->string alist.
Understands plain, quoted and block-scalar (`|` / `>` with chomping) values as
well as values wrapped across indented continuation lines."
  (let ((lines (frontmatter-lines text))
        (entries '()))
    (loop while lines
          for line = (pop lines)
          for indent = (yaml-indentation line)
          for colon = (and indent
                           (not (char= (char line indent) #\#))
                           (position #\: line))
          when colon
            do (let ((key (string-downcase (string-trim *yaml-blanks* (subseq line 0 colon))))
                     (raw (string-trim *yaml-blanks* (subseq line (1+ colon)))))
                 (multiple-value-bind (block rest) (take-indented-block lines indent)
                   (setf lines rest)
                   (push (cons key (frontmatter-value raw block)) entries)))
          else
            do (multiple-value-bind (block rest)
                   (if indent (take-indented-block lines indent) (values nil lines))
                 (declare (ignore block))
                 (setf lines rest)))
    (nreverse entries)))

(defun skills-directories (&optional (cwd (uiop:getcwd)))
  ;; Low-to-high precedence: project dirs shadow global dirs, and evo's own
  ;; directory shadows the generic .agents directory at the same scope.
  (list (merge-pathnames ".agents/skills/" (user-homedir-pathname))
        (merge-pathnames "skills/" (evo-home))
        (merge-pathnames ".agents/skills/" (uiop:ensure-directory-pathname cwd))
        (merge-pathnames "skills/" (project-evo-dir cwd))))

(defun available-skills (&optional (cwd (uiop:getcwd)))
  "Plists (:name :description :path). Later skill dirs shadow earlier ones."
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

;;; Prompt templates: .md files, filename = command, purely textual
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

;;; Prompt notes — extension-contributed system-prompt additions.  An
;;; extension that changes what the AGENT should DO (not just how output is
;;; shown) registers a note; the note rides in every system prompt until
;;; removed.  Named, so re-registration replaces rather than accumulates —
;;; an extension reloaded at session start stays idempotent — and so a
;;; feature toggled off can withdraw its guidance by name.  A note may be a
;;; function instead of a string: it is called with the active language pack
;;; while the prompt is being built, which is how an extension's own guidance
;;; follows /lang instead of staying in whatever language it was written in.

(defvar *prompt-notes* nil
  "Alist of (NAME . TEXT), appended to the system prompt after the
guidelines, in registration order.")

(defun register-prompt-note (name text)
  "Add or replace the system-prompt note NAME (a string) with TEXT, a
self-contained markdown snippet — or a function of the active language pack
returning one (NIL from it drops the note for that prompt).  NIL TEXT removes
the note.  Returns TEXT."
  (let ((entry (assoc name *prompt-notes* :test #'equal)))
    (cond ((null text)
           (setf *prompt-notes* (remove name *prompt-notes*
                                        :key #'car :test #'equal)))
          (entry (setf (cdr entry) text))
          (t (setf *prompt-notes*
                   (append *prompt-notes* (list (cons name text)))))))
  text)

(defun prompt-note-text (note pack)
  "Text NOTE contributes to a prompt in PACK's language.  A note that
signals is dropped with a warning rather than taking the turn down with it:
the agent losing one extension's guidance beats the agent losing its prompt."
  (let ((value (cdr note)))
    (if (functionp value)
        (handler-case (funcall value pack)
          (error (e)
            (warn "prompt note ~a failed: ~a" (car note) e)
            nil))
        value)))

(defun build-system-prompt (tools &key (cwd (uiop:getcwd)) lore model
                                       (language (language-request)))
  ;; NORMALIZE-NEWLINES so the prompt we send is the same text whatever the
  ;; line endings of the sources it was compiled from and of the files it
  ;; quotes (AGENTS.md, skills, context files) happen to be.
  (normalize-newlines
   (multiple-value-bind (pack response-language) (resolve-language language)
     (let ((bindings (prompt-bindings :cwd cwd :model model
                                      :response-language response-language)))
       (flet ((section (key &optional extra)
                (render-template (prompt-section key pack)
                                 (append extra bindings))))
         (with-output-to-string (out)
           (write-string (section :base) out)
           (format out "~2%~a~%" (section :tools-heading))
           (dolist (tool tools)
             (format out "- ~a: ~a~%" (tool-name tool)
                     (first (uiop:split-string (or (tool-description tool) "")
                                               :separator '(#\Newline)))))
           (format out "~%~a~%" (section :guidelines))
           ;; Extension-contributed guidance (REGISTER-PROMPT-NOTE), each
           ;; note given the chance to speak the active language.
           (dolist (note *prompt-notes*)
             (let ((text (prompt-note-text note pack)))
               (when text (format out "~%~a~%" text))))
           (let ((docs (probe-file (merge-pathnames "docs/" (evo-home)))))
             (when docs
               (format out "~%~a~%" (section :own-docs))
               ;; Name the files that are actually there, so the list can never
               ;; promise a doc the seed corpus did not install.
               (dolist (file (append (directory (merge-pathnames "*.md" docs))
                                     (directory (merge-pathnames "examples/*.lisp" docs))))
                 (format out "- ~a~%" (namestring file)))))
           (when lore
             ;; Lore: injected every turn, immune to summarization.  Each entry
             ;; carries its [id] so the user can ask to edit or remove it by id.
             (format out "~%~a~%" (section :lore-heading))
             (dolist (item lore)
               (if (and (listp item) (getf item :text))
                   (format out "- [~a] ~a~%" (getf item :id) (getf item :text))
                   (format out "- ~a~%" item))))
           (dolist (path (context-files cwd))
             (let ((content (ignore-errors (read-file-string path))))
               (when (and content (plusp (length content)))
                 (format out "~%~a~%~a~%"
                         (section :context-heading
                                  (list (cons "CONTEXT_PATH" (namestring path))))
                         (truncate-string content 20000)))))
           (let ((skills (available-skills cwd)))
             (when skills
               (format out "~%<available_skills>~%")
               (dolist (skill skills)
                 (format out "- ~a: ~a (read ~a before using)~%"
                         (pget skill :name) (pget skill :description) (pget skill :path)))
               (format out "</available_skills>~%")))
           (format out "~%~a~%" (section :environment))
           (when response-language
             (format out "~%~a~%" (section :respond-in)))
           (when (git-status-snapshot cwd)
             (format out "~%~a~%" (section :git-status)))))))))
