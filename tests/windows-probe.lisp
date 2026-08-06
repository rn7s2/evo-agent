;;;; windows-probe.lisp -- ask a Windows machine the questions this codebase
;;;; cannot answer from a Unix one.  Self-contained: plain SBCL, no evo, no
;;;; quicklisp, no sb-posix (which Windows SBCL does not have).  Every probe is
;;;; wrapped, so one failure cannot hide the rest, and every line is flushed as
;;;; it is written, so even a hard crash leaves on disk everything learned up
;;;; to that point.
;;;;
;;;; ROUND 3 -- the input half.  Settled so far, by measurement on Windows 11,
;;;; SBCL 2.6.7, console codepage 936:
;;;;
;;;;   * an "fd" is an OS handle.  In the console: *stdout* fd = 116 =
;;;;     GetStdHandle(STD_OUTPUT_HANDLE).  In the supervised child, the same
;;;;     stream is 12 -- handles are per process, so nothing may be hardcoded;
;;;;     taking the number out of SBCL's own stream is right in both.
;;;;   * the console external format is (UCS-2LE REPLACEMENT ...), not UTF-8.
;;;;     A stream built with SBCL's own format printed ASCII and the TUI's box
;;;;     drawing correctly; a stream forced to :utf-8 printed UTF-16 mojibake
;;;;     and then killed the process outright -- three runs, same place, no
;;;;     Lisp condition.  Both are now fixed in evo.port.
;;;;
;;;; What is left is the half that decides whether the TUI can read keys: the
;;;; console is written through a wide API, so is it read through one too?  evo
;;;; asks for raw (unsigned-byte 8) and parses ESC sequences out of the bytes.
;;;;
;;;; Run it in a console with plain SBCL (the round-2 answers say the child
;;;; behaves the same, so once is enough):
;;;;
;;;;    sbcl --script windows-probe.lisp
;;;;
;;;; It asks you to press a few keys and appends to
;;;; %USERPROFILE%\evo-windows-probe.txt -- send that file.  Nothing here
;;;; writes a mismatched encoding, so the console should stay readable.
;;;;
;;;; The file is deliberately kept in CR-LF (see .gitattributes): it doubles as
;;;; a standing check that CR-LF Lisp source compiles and reads.

(defparameter *probe-report*
  (merge-pathnames "evo-windows-probe.txt" (user-homedir-pathname)))

(defvar *probe-stream* nil)

(defun probe-say (control &rest args)
  "Write one report line and flush it: the next probe may be the fatal one."
  (let ((line (apply #'format nil control args)))
    (when *probe-stream*
      (write-line line *probe-stream*)
      (finish-output *probe-stream*))
    (ignore-errors
     (write-line line *error-output*)
     (finish-output *error-output*))))

(defmacro probe (label &body body)
  "Run BODY, reporting either its value or the condition it signalled."
  `(probe-say "~a: ~a" ,label
              (handler-case (progn ,@body)
                (error (e) (format nil "FAILED: ~a" e))
                (storage-condition (e) (format nil "FAILED: ~a" e)))))

(defun hex (n)
  (if (integerp n) (format nil "~d (#x~x)" n n) (format nil "~a" n)))

(defun env (name)
  (or (sb-ext:posix-getenv name) "(unset)"))

(defun stream-fd (stream)
  (if (typep stream 'sb-sys:fd-stream)
      (sb-sys:fd-stream-fd stream)
      :not-an-fd-stream))

(defun describe-codes (codes)
  "Numbers plus their printable spelling, which is how an ESC sequence is
recognised at a glance."
  (format nil "~a  [~a]" codes
          (map 'string (lambda (n) (if (< 31 n 127) (code-char n) #\.)) codes)))

#+win32
(progn
  (sb-alien:define-alien-routine ("GetStdHandle" %std-handle)
      sb-alien:system-area-pointer
    (which sb-alien:int))
  (sb-alien:define-alien-routine ("GetConsoleMode" %get-console-mode)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (mode (* sb-alien:unsigned-int)))
  (sb-alien:define-alien-routine ("SetConsoleMode" %set-console-mode)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (mode sb-alien:unsigned-int))

  (defun std-handle (which)
    (sb-sys:sap-int (%std-handle which)))

  (defun console-mode-of (handle)
    (sb-alien:with-alien ((mode sb-alien:unsigned-int))
      (if (zerop (%get-console-mode (sb-sys:int-sap handle) (sb-alien:addr mode)))
          nil
          mode)))

  (defun set-console-mode-of (handle mode)
    (not (zerop (%set-console-mode (sb-sys:int-sap handle) mode)))))

;;; Reading, with a deadline.  LISTEN before every read: a blocking read on a
;;; console that never delivers would hang the probe with nothing to show.

(defun read-codes-with-deadline (stream reader seconds limit)
  (let ((codes nil)
        (deadline (+ (get-universal-time) seconds)))
    (loop while (and (< (get-universal-time) deadline) (< (length codes) limit))
          do (if (listen stream)
                 (let ((item (funcall reader stream)))
                   (if (or (null item) (eq item :eof))
                       (return)
                       (push item codes)))
                 (sleep 0.05)))
    (nreverse codes)))

(defun probe-char-read (label seconds)
  (probe label
    (let ((codes (read-codes-with-deadline
                  sb-sys:*stdin*
                  (lambda (s) (let ((c (read-char-no-hang s nil :eof)))
                                (if (characterp c) (char-code c) c)))
                  seconds 12)))
      (if codes (describe-codes codes) "nothing arrived"))))

(defun probe-byte-read (label seconds)
  (probe label
    (let* ((stream (sb-sys:make-fd-stream (stream-fd sb-sys:*stdin*)
                                          :input t :buffering :none
                                          :element-type '(unsigned-byte 8)))
           (codes (read-codes-with-deadline
                   stream (lambda (s) (read-byte s nil :eof)) seconds 12)))
      (if codes (describe-codes codes) "nothing arrived"))))

(defun run-probe ()
  (with-open-file (out *probe-report* :direction :output
                                      :if-exists :append
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
    (let ((*probe-stream* out))
      (probe-say "============ evo windows probe, round 3 ============")
      (probe "when"
        (multiple-value-bind (sec min hour day month year) (get-decoded-time)
          (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                  year month day hour min sec)))
      (probe "implementation" (format nil "~a ~a" (lisp-implementation-type)
                                      (lisp-implementation-version)))
      (probe "role" (format nil "supervised child=~a" (env "EVO_SUPERVISED_CHILD")))
      (probe "*stdin* fd" (hex (stream-fd sb-sys:*stdin*)))
      (probe "*stdin* element-type" (stream-element-type sb-sys:*stdin*))
      #+win32
      (probe "console input mode at start" (hex (console-mode-of (std-handle -10))))

      ;; Bytes first, in both modes: SBCL's own character stream buffers, and
      ;; a buffered read on the same handle would swallow what the byte probe
      ;; is waiting for.  The byte answer is the one that decides the design.

      ;; 1. Cooked mode, raw bytes off the handle: what evo does today.
      (probe-say "")
      (probe-say ">>> STEP 1 of 4.  Type   abc   and press Enter.")
      (probe-byte-read "cooked mode, bytes via our own fd-stream" 25)

      ;; 2. Cooked mode, characters through SBCL's own stream: the control.
      (probe-say "")
      (probe-say ">>> STEP 2 of 4.  Type   abc   and press Enter again.")
      (probe-char-read "cooked mode, chars via SBCL's *stdin*" 25)

      ;; 3+4. The mode evo actually runs in: VT input, no line editing, no
      ;; echo -- so keys arrive as they are pressed and arrows arrive as ESC
      ;; sequences.  Nothing will be echoed; type blind.
      #+win32
      (probe "switch to ENABLE_VIRTUAL_TERMINAL_INPUT (evo's mode)"
        (set-console-mode-of (std-handle -10) #x200))

      (probe-say "")
      (probe-say ">>> STEP 3 of 4.  Press   a   then the UP ARROW.  No echo now,")
      (probe-say ">>> and no Enter needed.  ESC [ A would be 27 91 65.")
      (probe-byte-read "raw VT mode, bytes via our own fd-stream" 25)

      (probe-say "")
      (probe-say ">>> STEP 4 of 4.  Press   a   then the UP ARROW once more.")
      (probe-char-read "raw VT mode, chars via SBCL's *stdin*" 25)

      #+win32
      (probe "restore console input mode" (set-console-mode-of (std-handle -10) #x7))
      (probe-say "============ end of round 3 ============")))
  (ignore-errors
   (format *error-output* "~&probe report appended to: ~a~%" (namestring *probe-report*))
   (finish-output *error-output*))
  t)

(run-probe)
