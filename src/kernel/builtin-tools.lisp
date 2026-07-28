;;;; builtin-tools.lisp — the sequential tool set: read/write/edit/bash.

(in-package :evo.kernel)

(defparameter *read-max-chars* 40000)

(defun tool-read (args)
  (let* ((path (pget args :path))
         (offset (pget args :offset))
         (limit (pget args :limit)))
    (unless (and path (probe-file path))
      (error "File not found: ~a" path))
    (let* ((content (read-file-string path))
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
  (let ((path (pget args :path))
        (content (or (pget args :content) "")))
    (write-file-string path content)
    (format nil "Wrote ~d chars to ~a" (length content) path)))

(defun tool-edit (args)
  (let* ((path (pget args :path))
         (old (pget args :old-string))
         (new (pget args :new-string))
         (all (pget args :replace-all)))
    (unless (and path (probe-file path))
      (error "File not found: ~a" path))
    (unless (and (stringp old) (plusp (length old)))
      (error "old_string must be a non-empty string"))
    (let* ((content (read-file-string path))
           (n (count-substring old content)))
      (cond ((zerop n)
             (error "old_string not found in ~a" path))
            ((and (> n 1) (not all))
             (error "old_string occurs ~d times in ~a; make it unique or set replace_all"
                    n path))
            (t
             (write-file-string path (string-replace old new content :all all))
             (format nil "Edited ~a (~d replacement~:p)" path (if all n 1)))))))

(defparameter *bash-default-timeout* 120)
(defparameter *bash-max-timeout* 600)

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

(defun tool-bash (args)
  (let* ((command (pget args :command))
         (timeout (min *bash-max-timeout*
                       (or (pget args :timeout) *bash-default-timeout*)))
         (out-file (uiop:with-temporary-file (:pathname p :keep t) p))
         (keep nil)          ; leave OUT-FILE on disk when output is large
         (agent *executing-agent*))
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
         (let ((process (evo.port:launch-child
                         "/bin/sh" (list "-c" command)
                         :input nil :output out-file :error-output :output
                         :new-session t
                         :environment (list* (format nil "EVO_PID=~d"
                                                     (evo.port:getpid))
                                             (evo.port:environ)))))
           (when agent
             ;; The worker that launched PROCESS remains its sole owner.  The
             ;; TUI thread only sets the abort flag; cross-thread kill/wait on
             ;; an SBCL process handle can race the polling worker and crash
             ;; the runtime.
             (loop with deadline = (+ (get-internal-real-time)
                                      (* timeout internal-time-units-per-second))
                   while (evo.port:process-alive-p process)
                   when (agent-abort-flag agent)
                     do (evo.port:process-kill-tree process)
                        (evo.port:process-wait process)
                        (error "Command aborted by user. Partial output:~%~a"
                               (truncate-string
                                (or (ignore-errors (read-file-string out-file)) "")
                                10000))
                   when (> (get-internal-real-time) deadline)
                     do (evo.port:process-kill-tree process)
                        (evo.port:process-wait process)
                        (error "Command timed out after ~ds. Partial output:~%~a"
                               timeout (truncate-string
                                        (or (ignore-errors (read-file-string out-file)) "")
                                        10000))
                   do (sleep 0.05))
             (when (agent-abort-flag agent)
               (evo.port:process-wait process)
               (error "Command aborted by user. Partial output:~%~a"
                      (truncate-string
                       (or (ignore-errors (read-file-string out-file)) "")
                       10000))))
           (unless agent
             (loop with deadline = (+ (get-internal-real-time)
                                      (* timeout internal-time-units-per-second))
                   while (evo.port:process-alive-p process)
                   when (> (get-internal-real-time) deadline)
                     do (evo.port:process-kill-tree process)
                        (evo.port:process-wait process)
                        (error "Command timed out after ~ds. Partial output:~%~a"
                               timeout (truncate-string
                                        (or (ignore-errors (read-file-string out-file)) "")
                                        10000))
                   do (sleep 0.05)))
           (let ((code (nth-value 1 (evo.port:process-wait process)))
                 (bytes (or (file-byte-length out-file) 0)))
             (if (>= bytes *bash-max-inline-bytes*)
                 ;; Too big to inline: keep the file and point the agent at it.
                 (progn
                   (setf keep t)
                   (values
                    (format nil
                            "Output was large (~d bytes) — left on disk ~
                             instead of returned inline.~%Saved to: ~a~%~
                             Read it selectively (don't cat the whole file): ~
                             the read tool with offset/limit, or grep/sed/~
                             head/tail on that path.~%~%--- first ~d chars ---~%~
                             ~a~%--- end of preview ---~@[~%(exit code ~d)~]"
                            bytes (namestring out-file) *bash-preview-chars*
                            (or (read-file-head-string out-file *bash-preview-chars*)
                                "(non-UTF-8 output; preview unavailable)")
                            (and (/= code 0) code))
                    (list :exit-code code
                          :output-file (namestring out-file)
                          :output-bytes bytes)))
                 (let ((output (or (ignore-errors (read-file-string out-file)) "")))
                   (values
                    (format nil "~a~@[~%(exit code ~d)~]"
                            (if (zerop (length output)) "(no output)" output)
                            (and (/= code 0) code))
                    (list :exit-code code))))))
      (unless keep (ignore-errors (delete-file out-file))))))

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
   :description "Run a shell command via /bin/sh -c in the working directory. Returns combined stdout/stderr and exit code. Output at or above 1 MiB is not returned inline: it is written to a temp file and the tool returns that path plus a short head preview — read it back selectively (this read tool with offset/limit, or grep/sed/head/tail) rather than dumping it whole."
   :schema '(:object
             (:command :type :string :description "Shell command to run")
             (:timeout :type :integer :optional t
              :description "Timeout in seconds (default 120, max 600)"))
   :execute #'tool-bash))

(register-builtin-tools)
