;;;; windows-probe.lisp -- round 4.  Only run this if Enter still does nothing.
;;;;
;;;; Settled by rounds 1-3, all fixed: an "fd" is an OS handle here; the
;;;; console is written UCS-2LE (forcing :utf-8 garbles the screen and kills
;;;; the process); the console is read wide too, so a byte stream returns
;;;; UTF-16 (97 0 98 0 ...) while a character stream returns what was typed.
;;;;
;;;; Open question: in VT input mode Enter sends a bare CR, and no CR ever
;;;; appeared in round 3 -- SBCL's console stream translates newlines, so the
;;;; CR is held back waiting for an LF that never comes.  That is exactly the
;;;; symptom: Enter does nothing, ctrl+Enter (which sends LF) submits.
;;;;
;;;; The shipped fix asks for :newline :lf on the stream.  This round measures
;;;; whether SBCL honours that on a console, and if it does not, whether
;;;; ReadConsoleW called directly does -- so the next fix needs no third trip.
;;;;
;;;;    sbcl --script windows-probe.lisp
;;;;
;;;; Press Enter when asked.  Appends to %USERPROFILE%\evo-windows-probe.txt.

(defparameter *probe-report*
  (merge-pathnames "evo-windows-probe.txt" (user-homedir-pathname)))

(defvar *probe-stream* nil)

(defun probe-say (control &rest args)
  (let ((line (apply #'format nil control args)))
    (when *probe-stream*
      (write-line line *probe-stream*)
      (finish-output *probe-stream*))
    (ignore-errors (write-line line *error-output*) (finish-output *error-output*))))

(defmacro probe (label &body body)
  `(probe-say "~a: ~a" ,label
              (handler-case (progn ,@body)
                (error (e) (format nil "FAILED: ~a" e))
                (storage-condition (e) (format nil "FAILED: ~a" e)))))

(defun stream-fd (stream)
  (if (typep stream 'sb-sys:fd-stream) (sb-sys:fd-stream-fd stream) :not-an-fd-stream))

(defun untranslated-newlines (external-format)
  (let ((spec (if (listp external-format) (copy-list external-format)
                  (list external-format))))
    (loop until (eq (getf (cdr spec) :newline :absent) :absent)
          do (remf (cdr spec) :newline))
    (append spec (list :newline :lf))))

(defun codes (list)
  (format nil "~a  [~a]" list
          (map 'string (lambda (n) (if (< 31 n 127) (code-char n) #\.)) list)))

(defun read-chars-with-deadline (stream seconds limit)
  (let ((out nil) (deadline (+ (get-universal-time) seconds)))
    (loop while (and (< (get-universal-time) deadline) (< (length out) limit))
          do (if (listen stream)
                 (let ((c (read-char-no-hang stream nil nil)))
                   (if c (push (char-code c) out) (return)))
                 (sleep 0.05)))
    (nreverse out)))

#+win32
(progn
  (sb-alien:define-alien-routine ("GetStdHandle" %std-handle)
      sb-alien:system-area-pointer (which sb-alien:int))
  (sb-alien:define-alien-routine ("SetConsoleMode" %set-console-mode)
      sb-alien:int (handle sb-alien:system-area-pointer) (mode sb-alien:unsigned-int))
  (sb-alien:define-alien-routine ("GetConsoleMode" %get-console-mode)
      sb-alien:int (handle sb-alien:system-area-pointer) (mode (* sb-alien:unsigned-int)))
  (sb-alien:define-alien-routine ("ReadConsoleW" %read-console)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (buffer (* sb-alien:unsigned-short))
    (to-read sb-alien:unsigned-int)
    (did-read (* sb-alien:unsigned-int))
    (control sb-alien:system-area-pointer))
  (sb-alien:define-alien-routine ("GetNumberOfConsoleInputEvents" %input-events)
      sb-alien:int
    (handle sb-alien:system-area-pointer)
    (count (* sb-alien:unsigned-int)))

  (defun std-handle (which) (%std-handle which))

  (defun console-mode-of (handle)
    (sb-alien:with-alien ((mode sb-alien:unsigned-int))
      (if (zerop (%get-console-mode handle (sb-alien:addr mode))) nil mode)))

  (defun pending-events (handle)
    (sb-alien:with-alien ((count sb-alien:unsigned-int))
      (if (zerop (%input-events handle (sb-alien:addr count))) -1 count)))

  ;; ReadConsoleW straight at the handle: no stream, no external format, no
  ;; line discipline of SBCL's.  If Enter shows up here as 13 and nowhere
  ;; else, this is what the port layer has to call.
  (defun read-console-raw (handle seconds limit)
    (let ((out nil) (deadline (+ (get-universal-time) seconds)))
      (sb-alien:with-alien ((buffer (array sb-alien:unsigned-short 16))
                            (did-read sb-alien:unsigned-int))
        (loop while (and (< (get-universal-time) deadline) (< (length out) limit))
              do (if (plusp (pending-events handle))
                     (progn
                       (setf did-read 0)
                       (if (zerop (%read-console handle
                                                 (sb-alien:cast (sb-alien:addr buffer)
                                                                (* sb-alien:unsigned-short))
                                                 8 (sb-alien:addr did-read)
                                                 (sb-sys:int-sap 0)))
                           (return)
                           (dotimes (i did-read)
                             (push (sb-alien:deref buffer i) out))))
                     (sleep 0.05))))
      (nreverse out))))

(defun run-probe ()
  (with-open-file (out *probe-report* :direction :output :if-exists :append
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
    (let ((*probe-stream* out))
      (probe-say "============ evo windows probe, round 4 ============")
      (probe "*stdin* external-format" (stream-external-format sb-sys:*stdin*))
      (probe "asked for instead" (untranslated-newlines
                                  (stream-external-format sb-sys:*stdin*)))
      #+win32
      (probe "switch to VT input mode (evo's mode)"
        (not (zerop (%set-console-mode (std-handle -10) #x200))))

      (probe-say "")
      (probe-say ">>> STEP 1 of 2.  Press Enter twice.  ESC would be 27; the")
      (probe-say ">>> question is whether 13 arrives, and how soon.")
      (probe "stream with :newline :lf"
        (let ((stream (sb-sys:make-fd-stream
                       (stream-fd sb-sys:*stdin*) :input t :buffering :none
                       :external-format (untranslated-newlines
                                         (stream-external-format sb-sys:*stdin*)))))
          (let ((got (read-chars-with-deadline stream 20 8)))
            (if got (codes got) "nothing arrived -- the CR is still being held"))))

      (probe-say "")
      (probe-say ">>> STEP 2 of 2.  Press Enter twice again.")
      #+win32
      (probe "ReadConsoleW straight at the handle"
        (let ((got (read-console-raw (std-handle -10) 20 8)))
          (if got (codes got) "nothing arrived")))

      #+win32
      (probe "restore console input mode"
        (not (zerop (%set-console-mode (std-handle -10) #x7))))
      (probe-say "============ end of round 4 ============")))
  t)

(run-probe)
