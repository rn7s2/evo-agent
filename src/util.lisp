;;;; util.lisp — small shared helpers: env, time, ids, plists, safe sexpr IO,
;;;; settings.

(in-package :evo.util)

(defun getenv (name)
  (sb-ext:posix-getenv name))

(defun iso8601-now ()
  "Current UTC time as an ISO-8601 string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour min sec)))

(defvar *id-random-state* (make-random-state t))

(defun reseed-ids ()
  "Re-seed id generation from OS entropy.  MUST run at process startup: a
save-lisp-and-die image bakes the load-time random state, so without this
every process would emit the identical id sequence — and a resumed session
would reuse ids already in its journal, corrupting the parent tree."
  (setf *id-random-state* (make-random-state t)))

(uiop:register-image-restore-hook 'reseed-ids nil)

(defun gen-id (&optional (nibbles 8))
  "Short random hex id."
  (string-downcase
   (format nil "~v,'0x" nibbles (random (expt 16 nibbles) *id-random-state*))))

;;; Plists

(defun pget (plist key &optional default)
  (getf plist key default))

(defun pput (plist key value)
  "Non-destructive plist update; returns a fresh plist."
  (let ((copy (copy-list plist)))
    (setf (getf copy key) value)
    copy))

(defun plist-merge (base override)
  "Shallow merge: keys in OVERRIDE win."
  (let ((result (copy-list base)))
    (loop for (k v) on override by #'cddr
          do (setf (getf result k) v))
    result))

;;; Strings

(defun string-join (separator strings)
  (with-output-to-string (out)
    (loop for s in strings
          for first = t then nil
          unless first do (write-string separator out)
          do (write-string s out))))

(defun string-prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun truncate-string (string max-chars &optional (marker "... [truncated]"))
  (if (<= (length string) max-chars)
      string
      (concatenate 'string (subseq string 0 max-chars) marker)))

;;; Files & directories

(defun ensure-directory (pathname)
  (ensure-directories-exist
   (if (pathname-name pathname)
       pathname
       (merge-pathnames "x" pathname)))
  pathname)

(defun read-file-string (path)
  (with-open-file (in path :direction :input :external-format :utf-8
                           :if-does-not-exist :error)
    (let ((s (make-string (file-length in))))
      (let ((n (read-sequence s in)))
        (subseq s 0 n)))))

(defun write-file-string (path string)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :external-format :utf-8
                            :if-exists :supersede :if-does-not-exist :create)
    (write-string string out))
  path)

(defun evo-home ()
  "Global evo directory (~/.evo/, overridable with EVO_HOME for tests)."
  (let ((env (getenv "EVO_HOME")))
    (if (and env (plusp (length env)))
        (uiop:ensure-directory-pathname env)
        (merge-pathnames ".evo/" (user-homedir-pathname)))))

(defun project-evo-dir (&optional (cwd (uiop:getcwd)))
  (merge-pathnames ".evo/" (uiop:ensure-directory-pathname cwd)))

(defun encode-cwd (cwd)
  "Encode a directory path into a flat file-system-safe name (pi style)."
  (let ((s (string-right-trim "/" (namestring (uiop:ensure-directory-pathname cwd)))))
    (substitute #\- #\/ s)))

;;; Safe sexpr IO (journal format rules, §4.3)
;;;
;;; Journals are data, not code: read with *read-eval* nil into a sandbox
;;; package, and reject anything outside the "sexpr-JSON" vocabulary —
;;; plists/lists, keywords, strings, integers, ratios, floats, t/nil, vectors.

(defpackage :evo.sexpr-sandbox
  (:use)
  ;; t/nil must read as the CL symbols; everything else stays sandboxed.
  (:import-from :cl #:t #:nil))

(define-condition malformed-sexpr (error)
  ((text :initarg :text :reader malformed-sexpr-text))
  (:report (lambda (c s) (format s "Malformed journal sexpr: ~a" (malformed-sexpr-text c)))))

(defun validate-journal-value (value)
  "Signal MALFORMED-SEXPR unless VALUE is within the restricted vocabulary."
  (typecase value
    (string t)
    (integer t)
    (ratio t)
    (float t)
    (null t)
    (keyword t)
    (symbol (if (eq value t)
                t
                (error 'malformed-sexpr
                       :text (format nil "non-keyword symbol ~s" value))))
    (cons (progn
            (unless (and (listp value) (null (cdr (last value))))
              (error 'malformed-sexpr :text "improper list"))
            (mapc #'validate-journal-value value)
            t))
    (vector (progn (map nil #'validate-journal-value value) t))
    (t (error 'malformed-sexpr
              :text (format nil "unsupported object of type ~s" (type-of value))))))

(defun read-sexpr (line)
  "Read one journal form from LINE, safely.  Returns the validated form."
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*package* (find-package :evo.sexpr-sandbox)))
      (let ((form (read-from-string line)))
        (validate-journal-value form)
        form))))

(defun read-sexpr-stream (stream)
  "Read the next journal form from STREAM, safely.  Returns :eof at end.
Reading is form-based, not line-based: writing keeps one form per line, but a
string value containing newlines legally spans lines."
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*package* (find-package :evo.sexpr-sandbox)))
      (let ((form (read stream nil :eof)))
        (unless (eq form :eof)
          (validate-journal-value form))
        form))))

(defun write-sexpr-line (form stream)
  "Write FORM as a single line to STREAM (readable, lowercase keywords).
*print-readably* stays NIL: with it, SBCL prints base-char strings in #A
array syntax — legal but hostile to the hand-editable-journal rule.  Within
the validated vocabulary, plain prin1 output re-reads exactly."
  (validate-journal-value form)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-pretty* nil)
          (*print-readably* nil))
      (prin1 form stream)))
  (terpri stream))

;;; Settings
;;;
;;; Sexpr plists (D3): global ~/.evo/settings.sexp merged with project
;;; .evo/settings.sexp (project wins, shallow merge).

(defvar *settings* nil)

(defun load-settings-file (path)
  (when (probe-file path)
    (handler-case (read-sexpr (read-file-string path))
      (error (e)
        (warn "Ignoring unreadable settings file ~a: ~a" path e)
        nil))))

(defun load-settings (&optional (cwd (uiop:getcwd)))
  (let ((global (load-settings-file (merge-pathnames "settings.sexp" (evo-home))))
        (project (load-settings-file (merge-pathnames "settings.sexp" (project-evo-dir cwd)))))
    (setf *settings* (plist-merge global project))))

(defun setting (key &optional default)
  (pget *settings* key default))
