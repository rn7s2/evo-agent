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
               do (format out "~6d\t~a~%" n line)))
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

(defun tool-bash (args)
  (let* ((command (pget args :command))
         (timeout (min *bash-max-timeout*
                       (or (pget args :timeout) *bash-default-timeout*)))
         (out-file (uiop:with-temporary-file (:pathname p :keep t) p)))
    (unless (and (stringp command) (plusp (length command)))
      (error "command must be a non-empty string"))
    (unwind-protect
         ;; The child inherits our environment and working directory.
         (let ((process (evo.port:launch-child
                         "/bin/sh" (list "-c" command)
                         :input nil :output out-file :error-output :output)))
           (loop with deadline = (+ (get-internal-real-time)
                                    (* timeout internal-time-units-per-second))
                 while (evo.port:process-alive-p process)
                 when (> (get-internal-real-time) deadline)
                   do (evo.port:process-kill process)
                      (evo.port:process-wait process)
                      (error "Command timed out after ~ds. Partial output:~%~a"
                             timeout (truncate-string
                                      (or (ignore-errors (read-file-string out-file)) "")
                                      10000))
                 do (sleep 0.05))
           (let ((code (nth-value 1 (evo.port:process-wait process)))
                 (output (or (ignore-errors (read-file-string out-file)) "")))
             (values
              (format nil "~a~@[~%(exit code ~d)~]"
                      (if (zerop (length output)) "(no output)" output)
                      (and (/= code 0) code))
              (list :exit-code code))))
      (ignore-errors (delete-file out-file)))))

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
   :description "Run a shell command via /bin/sh -c in the working directory. Returns combined stdout/stderr and exit code."
   :schema '(:object
             (:command :type :string :description "Shell command to run")
             (:timeout :type :integer :optional t
              :description "Timeout in seconds (default 120, max 600)"))
   :execute #'tool-bash))

(register-builtin-tools)
