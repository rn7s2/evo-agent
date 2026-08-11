;;;; builtin-tools.lisp — the sequential tool set: read/write/edit/bash.

(in-package :evo.kernel)

(defparameter *read-max-chars* 40000)

(defun tool-read (args)
  (let* ((path (pget args :path))
         (offset (pget args :offset))
         (limit (pget args :limit)))
    (unless (and path (probe-file path))
      (error "File not found: ~a" path))
    (let* ((content (normalize-newlines (read-file-string path)))
           (lines (uiop:split-string content :separator '(#\Newline)))
           (start (max 0 (1- (or offset 1))))
           (end (min (length lines) (+ start (or limit (length lines)))))
           (selected (subseq lines start end)))
      (truncate-string
       (with-output-to-string (out)
         (loop for line in selected
               for n from (1+ start)
               do (format out "~6d~c~a~%" n #\Tab line)))
       *read-max-chars*
       (format nil "~%... [truncated at ~d chars; use offset/limit to read more]"
               *read-max-chars*)))))

(defun tool-write (args)
  (let* ((path (pget args :path))
         (content (or (pget args :content) ""))
         ;; Rewriting a file should not restyle every line of it: a CR-LF
         ;; file stays CR-LF (the agent writes LF, and git would otherwise
         ;; show the whole file as changed).  New files get what they are
         ;; given.
         (existing (and (probe-file path)
                        (ignore-errors (read-file-string path))))
         (crlf (and existing (crlf-p existing) (not (crlf-p content))))
         (written (if crlf (crlf-newlines content) content)))
    (write-file-string path written)
    (format nil "Wrote ~d chars to ~a~@[ ~a~]" (length written) path
            (and crlf "(kept the file's CR-LF line endings)"))))

(defun eol-respelled (old new content)
  "(values OLD NEW) written with the line endings CONTENT actually uses.

The agent sees LF — that is what the read tool shows it, and what a model
writes back — while the file on disk may be CR-LF, so a literal search for
OLD would miss every string that spans a line break.  We do not convert the
file: we respell the needle (and the replacement with it, so the edit keeps
the file's convention) and leave every other byte alone.  Files with mixed
endings therefore stay mixed."
  (cond ((search old content) (values old new))
        ((search (crlf-newlines old) content)
         (values (crlf-newlines old) (crlf-newlines new)))
        ((search (normalize-newlines old) content)
         (values (normalize-newlines old) (normalize-newlines new)))
        (t (values old new))))

(defun tool-edit (args)
  (let* ((path (pget args :path))
         (old (pget args :old-string))
         (new (pget args :new-string))
         (all (pget args :replace-all)))
    (unless (and path (probe-file path))
      (error "File not found: ~a" path))
    (unless (and (stringp old) (plusp (length old)))
      (error "old_string must be a non-empty string"))
    (let ((content (read-file-string path)))
      (multiple-value-setq (old new) (eol-respelled old new content))
      (let ((n (count-substring old content)))
        (cond ((zerop n)
               (error "old_string not found in ~a" path))
              ((and (> n 1) (not all))
               (error "old_string occurs ~d times in ~a; make it unique or set replace_all"
                      n path))
              (t
               (write-file-string path (string-replace old new content :all all))
               (format nil "Edited ~a (~d replacement~:p)" path (if all n 1))))))))

;; *bash-default-timeout* / *bash-max-timeout* live in jobs.lisp (loaded first):
;; the timeout is now a yield ceiling shared by bash and `wait`.

(defparameter *bash-max-inline-bytes* (* 1024 1024)
  "Bash output at or above this size (1 MiB) is left on disk instead of
returned inline: dumping megabytes into the context is wasteful and rarely
what's wanted.  The tool returns a head preview plus the file path so the
agent can read it selectively (read tool offset/limit, grep, sed, head/tail).")

(defparameter *bash-preview-chars* 4000
  "How many leading characters of a spilled-to-disk output to show inline.")

(defun file-byte-length (path)
  "Size of PATH in bytes, or NIL if it can't be measured."
  (ignore-errors
   (with-open-file (in path :element-type '(unsigned-byte 8)
                            :if-does-not-exist nil)
     (and in (file-length in)))))

(defun read-file-head-string (path max-chars)
  "First MAX-CHARS characters of PATH (UTF-8); NIL if unreadable (e.g. binary)."
  (ignore-errors
   (with-open-file (in path :direction :input :external-format :utf-8
                            :if-does-not-exist nil)
     (when in
       (let* ((buf (make-string max-chars))
              (n (read-sequence buf in)))
         (subseq buf 0 n))))))

(defun partial-output (out-file &optional (max-chars 10000))
  "What a killed command had printed so far — LF, whatever the platform's
console spells its line breaks with."
  (truncate-string (normalize-newlines
                    (or (ignore-errors (read-file-string out-file)) ""))
                   max-chars))

(defun bash-wait-loop (process out-file timeout agent)
  "Poll PROCESS until it exits (returns :DONE) or TIMEOUT seconds pass (returns
:TIMEOUT).  On user abort, kill the process and signal — the same, whether or
not there is an executing AGENT.  The worker that launched PROCESS stays its
sole owner: the TUI thread only sets the abort flag, and a cross-thread
kill/wait on the handle can race this poll and crash the runtime."
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        while (evo.port:process-alive-p process)
        do (when (and agent (agent-abort-flag agent))
             (evo.port:process-kill-tree process)
             (evo.port:process-wait process)
             (error "Command aborted by user. Partial output:~%~a"
                    (partial-output out-file)))
           (when (> (get-internal-real-time) deadline)
             (return-from bash-wait-loop :timeout))
           (sleep 0.05))
  (when (and agent (agent-abort-flag agent))
    (evo.port:process-wait process)
    (error "Command aborted by user. Partial output:~%~a"
           (partial-output out-file)))
  :done)

(defun tool-bash (args)
  (let* ((command (pget args :command))
         (timeout (min *bash-max-timeout*
                       (or (pget args :timeout) *bash-default-timeout*)))
         (out-file (uiop:with-temporary-file (:pathname p :keep t) p))
         (keep nil)          ; leave OUT-FILE on disk (large output, or detached)
         (agent *executing-agent*)
         (script nil))       ; scratch shell script, on platforms that need one
    (unless (and (stringp command) (plusp (length command)))
      (error "command must be a non-empty string"))
    (unwind-protect
         ;; The child inherits our environment and working directory.
         ;; NEW-SESSION detaches the child from evo's controlling terminal, so
         ;; a command that tries to prompt on /dev/tty (sudo, ssh, gpg) fails
         ;; at once instead of hanging until the timeout — the agent has no way
         ;; to answer such a prompt.  INPUT NIL (null device) covers the
         ;; separate stdin-EOF case.  EVO_PID names this (the worker) process,
         ;; since NEW-SESSION means $PPID points at the wrapper, not evo.
         (let ((process (multiple-value-bind (program shell-args scratch)
                            (evo.port:shell-invocation command)
                          (setf script scratch)
                          (evo.port:launch-child
                           program shell-args
                           :input nil :output out-file :error-output :output
                           :new-session t
                           :environment (list* (format nil "EVO_PID=~d"
                                                       (evo.port:getpid))
                                               (evo.port:environ))))))
           ;; At the yield ceiling the command is NOT killed: it keeps running
           ;; as a background job and bash hands back a handle, so the model
           ;; never has to guess a duration and sleep for it (see jobs.lisp).
           (when (eq (bash-wait-loop process out-file timeout agent) :timeout)
             (let ((id (detach-as-job :command command :process process
                                      :out-file out-file :script script)))
               (setf keep t script nil)  ; the job owns these files now
               (return-from tool-bash
                 (values (still-running-note id command
                                             (job-new-output (find-job id)))
                         (list :job-id id :running t)))))
           (let ((code (nth-value 1 (evo.port:process-wait process)))
                 (bytes (or (file-byte-length out-file) 0)))
             (if (>= bytes *bash-max-inline-bytes*)
                 ;; Too big to inline: keep the file and point the agent at it.
                 (progn
                   (setf keep t)
                   (values
                    (format nil
                            (cat "Output was large (~d bytes) — left on disk "
                                 "instead of returned inline.~%Saved to: ~a~%"
                                 "Read it selectively (don't cat the whole file): "
                                 "the read tool with offset/limit, or grep/sed/"
                                 "head/tail on that path.~%~%--- first ~d chars ---~%"
                                 "~a~%--- end of preview ---~@[~%(exit code ~d)~]")
                            bytes (namestring out-file) *bash-preview-chars*
                            (normalize-newlines
                             (or (read-file-head-string out-file *bash-preview-chars*)
                                 "(non-UTF-8 output; preview unavailable)"))
                            (and (/= code 0) code))
                    (list :exit-code code
                          :output-file (namestring out-file)
                          :output-bytes bytes)))
                 ;; A console program spells line breaks the platform's way
                 ;; (CR-LF on Windows); the agent reads LF everywhere.
                 (let ((output (normalize-newlines
                                (or (ignore-errors (read-file-string out-file)) ""))))
                   (values
                    (format nil "~a~@[~%(exit code ~d)~]"
                            (if (zerop (length output)) "(no output)" output)
                            (and (/= code 0) code))
                    (list :exit-code code))))))
      (progn
        (when script (ignore-errors (delete-file script)))
        (unless keep (ignore-errors (delete-file out-file)))))))

(defun register-builtin-tools ()
  (register-tool*
   :name "read"
   :description "Read a file from the filesystem. Returns numbered lines. Use offset/limit for large files."
   :schema '(:object
             (:path :type :string :description "Absolute or cwd-relative file path")
             (:offset :type :integer :optional t :description "1-based line to start from")
             (:limit :type :integer :optional t :description "Max lines to return"))
   :execute #'tool-read)
  (register-tool*
   :name "write"
   :description "Write a file (create or overwrite), creating parent directories as needed."
   :schema '(:object
             (:path :type :string :description "File path to write")
             (:content :type :string :description "Full file content"))
   :execute #'tool-write)
  (register-tool*
   :name "edit"
   :description "Replace an exact string in a file. old_string must match exactly and be unique unless replace_all is set."
   :schema '(:object
             (:path :type :string :description "File path to edit")
             (:old-string :type :string :description "Exact text to replace")
             (:new-string :type :string :description "Replacement text")
             (:replace-all :type :boolean :optional t :description "Replace every occurrence"))
   :execute #'tool-edit)
  (register-tool*
   :name "bash"
   ;; The shell is named, not assumed: on Windows this is PowerShell, and a
   ;; model told it is talking to /bin/sh writes commands that cannot run.
   :description (format nil "Run a shell command via ~a in the working directory. Returns combined stdout/stderr and exit code. Output at or above 1 MiB is not returned inline: it is written to a temp file and the tool returns that path plus a short head preview — read it back selectively (this read tool with offset/limit, or grep/sed/head/tail) rather than dumping it whole. A command still running at its timeout is NOT killed: it moves to the background and returns a job_id — call `wait` with that id to collect its output or kill it. Do not sleep-and-poll for a long command; just let it run and use `wait`."
                        (evo.port:shell-name))
   :schema '(:object
             (:command :type :string :description "Shell command to run")
             (:timeout :type :integer :optional t
              :description "Seconds to wait before the command moves to the background as a job (default 120, max 600)"))
   :execute #'tool-bash))

(register-builtin-tools)
