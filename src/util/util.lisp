;;;; util.lisp — small shared helpers: env, time, ids, plists, safe sexpr IO,
;;;; settings.

(in-package :evo.util)

(defun getenv (name)
  (uiop:getenv name))

(defun url-host (url)
  (let* ((start (let ((p (search "://" url))) (if p (+ p 3) 0)))
         (end (or (position-if (lambda (c) (member c '(#\/ #\: #\?))) url
                               :start start)
                  (length url))))
    (subseq url start end)))

(defun proxy-bypass-p (host)
  "True when HOST must skip the proxy: loopback always, plus no_proxy /
NO_PROXY entries (comma-separated; an entry matches itself and its
subdomains; * matches everything)."
  (flet ((suffix-p (suffix s)
           (let ((n (- (length s) (length suffix))))
             (and (>= n 0) (string-equal suffix s :start2 n)))))
    (or (member host '("localhost" "127.0.0.1" "::1") :test #'string-equal)
        (let ((no-proxy (or (getenv "no_proxy") (getenv "NO_PROXY"))))
          (and no-proxy
               (loop for start = 0 then (1+ end)
                     for end = (or (position #\, no-proxy :start start)
                                   (length no-proxy))
                     for entry = (string-trim " " (subseq no-proxy start end))
                     thereis (and (plusp (length entry))
                                  (or (string= entry "*")
                                      (string-equal entry host)
                                      (suffix-p (if (char= (char entry 0) #\.)
                                                    entry
                                                    (concatenate 'string "." entry))
                                                host)))
                     until (= end (length no-proxy))))))))

(defun env-proxy (url)
  "Proxy for URL from the environment.  Passed explicitly on every
request: dexador's *default-proxy* only reads the UPPERCASE env vars, and
via a defvar evaluated at image build time — so in the shipped binary it
is stale on top of missing the lowercase Unix convention."
  (flet ((nonempty (name)
           (let ((v (getenv name)))
             (and v (plusp (length v)) v))))
    (let ((proxy (or (nonempty "HTTPS_PROXY") (nonempty "https_proxy")
                     (nonempty "HTTP_PROXY") (nonempty "http_proxy"))))
      (and proxy
           (not (proxy-bypass-p (url-host url)))
           proxy))))

(defvar *request-proxy* nil
  "The proxy for the request in flight, read by the WinHTTP shim below.")

(defvar *winhttp-proxy-shim* nil
  "True once ENSURE-WINHTTP-PROXY has wrapped WINHTTP:HTTP-OPEN.")

(defun ensure-winhttp-proxy ()
  "Teach dexador's Windows backend to use a proxy.  Idempotent, and a no-op
anywhere else.

dexador speaks WinHTTP on Windows, and that backend ignores :proxy outright
-- its own issue #66 closed with \"Proxy support: winhttp doesn't support
it\", and the argument is still in the DECLARE IGNORE.  Worse than ignoring
it: the library it calls passes WINHTTP_ACCESS_TYPE_NO_PROXY when given no
proxy, so every request is told explicitly to go direct, and no system-wide
setting (netsh winhttp, IE, http_proxy) can override that.

The library has since grown the argument WINHTTP:HTTP-OPEN takes -- with a
proxy it passes WINHTTP_ACCESS_TYPE_NAMED_PROXY -- so the fix is to hand it
the proxy evo has already resolved for this request.  Wrapped rather than
copied, because upstream (#202) is implementing this properly and the wrap
disappears when they do."
  (when (and (evo.port:windows-p) (not *winhttp-proxy-shim*))
    (setf *winhttp-proxy-shim* t)
    (let* ((package (find-package "WINHTTP"))
           (name (and package (find-symbol "HTTP-OPEN" package))))
      (when (and name (fboundp name))
        (let ((original (symbol-function name)))
          (setf (symbol-function name)
                (lambda (&optional user-agent proxy)
                  (let ((proxy (or proxy *request-proxy*)))
                    (or (and proxy
                             ;; An older winhttp takes no proxy argument.
                             (ignore-errors (funcall original user-agent proxy)))
                        (funcall original user-agent))))))))))

(defmacro with-proxy ((var url) &body body)
  "Bind VAR to the proxy for URL, and make it visible to the WinHTTP shim.
Every HTTP call evo makes goes through here: passing :proxy to dexador is
enough on Unix and is ignored on Windows (see ENSURE-WINHTTP-PROXY)."
  `(let* ((,var (env-proxy ,url))
          (*request-proxy* ,var))
     (ensure-winhttp-proxy)
     ,@body))

(defun iso8601-utc (universal-time)
  "UNIVERSAL-TIME as an ISO-8601 UTC string.  Fixed width, so string order
is time order — sorting session listings leans on that."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour min sec)))

(defun iso8601-now ()
  "Current UTC time as an ISO-8601 string."
  (iso8601-utc (get-universal-time)))

(defun %trim-nonempty (string)
  (let ((trimmed (and string
                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   string))))
    (and trimmed (plusp (length trimmed)) trimmed)))

(defun %zoneinfo-location-from-path (path)
  "Return the IANA location suffix from a zoneinfo PATH, if visible."
  (let* ((raw (and path (%trim-nonempty (namestring (pathname path)))))
         (pos (and raw (search "zoneinfo/" raw :test #'char=))))
    (when pos
      (let ((name (subseq raw (+ pos (length "zoneinfo/")))))
        (%trim-nonempty name)))))

(defun %iana-timezone-name-p (name)
  "True for IANA-looking TZ names; avoids scanning the full tzdata repo."
  (and (%trim-nonempty name)
       (not (find #\, name))
       (not (find #\: name))
       (or (equal name "UTC")
           (search "/" name :test #'char=))))

(defun %canonical-timezone-location-name (candidate)
  "Normalize TZ-style CANDIDATE into an IANA-looking timezone location name."
  (let* ((raw (%trim-nonempty candidate))
         (without-colon (and raw (if (char= (char raw 0) #\:)
                                     (subseq raw 1)
                                     raw))))
    (when (%trim-nonempty without-colon)
      (or (%zoneinfo-location-from-path without-colon)
          (%zoneinfo-location-from-path
           (ignore-errors (namestring (truename (pathname without-colon)))))
          (and (not (char= (char without-colon 0) #\/))
               (%iana-timezone-name-p without-colon)
               without-colon)))))

(defun local-timezone-name ()
  "Best-effort local IANA timezone name, e.g. \"Asia/Shanghai\".
Falls back to \"local\" when the OS does not expose a zoneinfo-style name.
This intentionally avoids LOCAL-TIME:REREAD-TIMEZONE-REPOSITORY, which scans
all tzdata files and can visibly freeze the TUI on first /resume."
  (or (%canonical-timezone-location-name (getenv "TZ"))
      (%canonical-timezone-location-name
       (ignore-errors (namestring (truename #P"/etc/localtime"))))
      "local"))

(defun %format-cl-local-timestamp (timestamp timezone-label)
  "Format TIMESTAMP in the host process local timezone via ANSI CL."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (local-time:timestamp-to-universal timestamp))
    (declare (ignore sec))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d ~a"
            year month day hour min timezone-label)))

(defun format-local-timestamp (timestamp &key (timezone-name (local-timezone-name)))
  "Format an ISO-8601 TIMESTAMP in local time and append the timezone name."
  (handler-case
      (%format-cl-local-timestamp
       (local-time:parse-timestring timestamp)
       (or (%trim-nonempty timezone-name) "local"))
    (error () timestamp)))

(defvar *id-random-state* (make-random-state t))

(defun reseed-ids ()
  "Re-seed id generation from OS entropy.  MUST run at process startup: a
save-lisp-and-die image bakes the load-time random state, so without this
every process would emit the identical id sequence — two agents started
separately would pick the same goal id and the same session id, and a resumed
session would reuse ids already in its journal, corrupting the parent tree.

Called explicitly from EVO.CLI:TOPLEVEL, not via
UIOP:REGISTER-IMAGE-RESTORE-HOOK: those hooks only fire from
UIOP:RESTORE-IMAGE, which the binary never reaches — SBCL is saved with our
own :toplevel and ECL with our own :epilogue-code."
  (setf *id-random-state* (make-random-state t)))

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

(defmacro cat (&rest parts)
  "Concatenate PARTS into one string — at compile time when they are all
literals, which is the point.

A long control string wants to be written as several source lines, and CL
offers exactly one way to do that inside a literal: end the line with the
~<newline> FORMAT directive.  That directive reads the character after the
tilde, so its meaning depends on the file's line endings — a CR-LF checkout
turns every one of them into \"error in FORMAT: Unknown directive
(character: Return)\", at compile time.  Writing the pieces as separate
literals keeps the line breaks outside the strings, where CR is only
whitespace, and the expansion is still a single constant so FORMAT's
compile-time checking and optimization apply as before."
  (if (every #'stringp parts)
      (apply #'concatenate 'string parts)
      `(concatenate 'string ,@parts)))

;;; Newlines.  Text reaches us from places that spell a line break their own
;;; way: a file written by a Windows editor, a console program's output, the
;;; clipboard, an HTTP body.  Nothing downstream should have to care, so
;;; foreign text is normalized where it enters and respelled, if the
;;; destination has an opinion, where it leaves.

(defun normalize-newlines (text)
  "TEXT with CR-LF and lone CR line breaks turned into LF.
Returns TEXT itself when there is nothing to change."
  (if (find #\Return text)
      (with-output-to-string (out)
        (loop with n = (length text)
              for i from 0 below n
              for char = (char text i)
              do (cond ((char/= char #\Return) (write-char char out))
                       ((and (< (1+ i) n) (char= (char text (1+ i)) #\Newline)))
                       (t (write-char #\Newline out)))))
      text))

(defun crlf-p (text)
  "Does TEXT use CR-LF line breaks?"
  (and (search (coerce '(#\Return #\Newline) 'string) text) t))

(defun crlf-newlines (text)
  "TEXT with every line break spelled CR-LF."
  (let ((lf (normalize-newlines text)))
    (if (find #\Newline lf)
        (with-output-to-string (out)
          (loop for char across lf
                do (when (char= char #\Newline) (write-char #\Return out))
                   (write-char char out)))
        lf)))

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

(defun count-substring (needle haystack)
  (loop with n = 0 with start = 0
        for pos = (search needle haystack :start2 start)
        while pos
        do (incf n) (setf start (1+ pos))
        finally (return n)))

(defun string-replace (needle replacement haystack &key all)
  (with-output-to-string (out)
    (loop with start = 0
          for pos = (search needle haystack :start2 start)
          while pos
          do (write-string haystack out :start start :end pos)
             (write-string replacement out)
             (setf start (+ pos (length needle)))
             (unless all (loop-finish))
          finally (write-string haystack out :start start))))

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

;;; Binary file IO.  Text IO decodes UTF-8 and would mangle image bytes, so
;;; anything that travels as bytes (images, clipboard grabs) uses these.

(defun read-file-octets (path)
  "Contents of PATH as a simple (unsigned-byte 8) vector."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8)
                           :if-does-not-exist :error)
    (let* ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8)))
           (n (read-sequence buffer in)))
      (if (= n (length buffer)) buffer (subseq buffer 0 n)))))

(defun write-file-octets (path octets)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede :if-does-not-exist :create)
    (write-sequence octets out))
  path)

;;; Base64.
;;;
;;; Hand-rolled rather than pulled in as a dependency: it is twenty lines of
;;; table lookup, and image payloads are the only user (the wire format for
;;; every provider's image block).  Standard alphabet, always padded.

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun octets->base64 (octets)
  "Base64-encode OCTETS (a byte vector) into a fresh string."
  (let* ((n (length octets))
         (out (make-string (* 4 (ceiling n 3))))
         (j 0))
    (loop for i from 0 below n by 3
          for b0 = (aref octets i)
          for b1 = (if (< (+ i 1) n) (aref octets (+ i 1)) 0)
          for b2 = (if (< (+ i 2) n) (aref octets (+ i 2)) 0)
          for word = (logior (ash b0 16) (ash b1 8) b2)
          do (setf (char out j) (char +base64-alphabet+ (ldb (byte 6 18) word))
                   (char out (+ j 1)) (char +base64-alphabet+ (ldb (byte 6 12) word))
                   (char out (+ j 2)) (if (< (+ i 1) n)
                                          (char +base64-alphabet+ (ldb (byte 6 6) word))
                                          #\=)
                   (char out (+ j 3)) (if (< (+ i 2) n)
                                          (char +base64-alphabet+ (ldb (byte 6 0) word))
                                          #\=))
             (incf j 4))
    out))

(defun base64->octets (string)
  "Decode STRING (standard alphabet; whitespace ignored) into a byte vector."
  (let ((out (make-array (* 3 (ceiling (length string) 4))
                         :element-type '(unsigned-byte 8) :fill-pointer 0))
        (acc 0) (bits 0))
    (loop for char across string
          for value = (position char +base64-alphabet+)
          do (cond (value
                    (setf acc (logior (ash acc 6) value))
                    (incf bits 6)
                    (when (>= bits 8)
                      (decf bits 8)
                      (vector-push (ldb (byte 8 bits) acc) out)))
                   ((or (char= char #\=) (member char '(#\Space #\Tab #\Newline #\Return)))
                    nil)
                   (t (error "base64->octets: invalid character ~s" char))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun evo-home ()
  "Global evo directory (~/.evo/, overridable with EVO_HOME for tests)."
  (let ((env (getenv "EVO_HOME")))
    (if (and env (plusp (length env)))
        (uiop:ensure-directory-pathname env)
        (merge-pathnames ".evo/" (user-homedir-pathname)))))

(defun project-evo-dir (&optional (cwd (uiop:getcwd)))
  (merge-pathnames ".evo/" (uiop:ensure-directory-pathname cwd)))

(defun encode-cwd (cwd)
  "Encode a directory path into a flat file-system-safe name (pi style).
Both separators and the drive colon go: a Windows cwd encodes to
`C-Users-me-project`, and a name with a colon in it is not creatable on
NTFS at all (it names an alternate data stream)."
  (let ((s (string-right-trim "/\\" (namestring (uiop:ensure-directory-pathname cwd)))))
    (map 'string (lambda (c) (if (member c '(#\/ #\\ #\:)) #\- c)) s)))

;;; Safe sexpr IO (journal format rules)
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
;;; A keyword→value plist mutated by config code (init.lisp) through
;;; set-setting; reset before each userspace boot, so project init
;;; overriding global init is just a later call.

(defvar *settings* nil)

(defun setting (key &optional default)
  (pget *settings* key default))

(defun set-setting (key value)
  (unless (keywordp key)
    (error "set-setting: key must be a keyword, got ~s" key))
  (setf (getf *settings* key) value)
  value)

(defun (setf setting) (value key &optional default)
  (declare (ignore default))
  (set-setting key value))

(defun reset-settings ()
  (setf *settings* nil))

(defun capture-settings ()
  "A copy of the settings plist, for restoring a runtime generation wholesale."
  (copy-list *settings*))

(defun restore-settings (settings)
  (setf *settings* (copy-list settings)))
