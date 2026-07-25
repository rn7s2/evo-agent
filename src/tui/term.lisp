;;;; term.lisp — terminal control for the tui core extension.
;;;;
;;;; Raw mode + size via /bin/stty (no curses, no FFI: variadic ioctl is not
;;;; safely callable through implementation FFIs on arm64 Darwin, and stty is
;;;; exercised only at startup/exit/resize).  Output is ANSI escapes on a
;;;; dedicated fd-stream.  SIGWINCH sets a flag the main loop polls
;;;; (mandatory live resize).

(in-package :evo.tui)

(defvar *tty-out* nil)
(defvar *saved-stty* nil)
(defvar *resized* nil)
(defvar *rows* 24)
(defvar *cols* 80)

(defun stty (&rest args)
  (string-trim '(#\Newline #\Space)
               (with-output-to-string (out)
                 (uiop:run-program (cons "/bin/stty" args)
                                   :input :interactive :output out
                                   :ignore-error-status t))))

(defun refresh-size ()
  (let* ((size (ignore-errors (stty "size")))
         (parts (and size (uiop:split-string size :separator '(#\Space)))))
    (when (= 2 (length parts))
      (let ((r (parse-integer (first parts) :junk-allowed t))
            (c (parse-integer (second parts) :junk-allowed t)))
        (when (and r c (plusp r) (plusp c))
          (setf *rows* r *cols* c)))))
  (values *rows* *cols*))

(defun install-sigwinch ()
  (evo.port:install-signal-handler evo.port:+sigwinch+
                                   (lambda () (setf *resized* t))))

(defun wr (&rest strings)
  (dolist (s strings)
    (write-string s *tty-out*)))

(defun flush ()
  (force-output *tty-out*))

(defun esc (&rest parts)
  (format nil "~c[~{~a~}" #\Escape parts))

;; Frequently used sequences.
(defun cursor-up (n) (if (plusp n) (esc n "A") ""))
(defun cursor-right (n) (if (plusp n) (esc n "C") ""))
(defun clear-below () (esc "0J"))
(defun hide-cursor () (esc "?25l"))
(defun show-cursor () (esc "?25h"))
(defun sgr (&rest codes) (format nil "~c[~{~a~^;~}m" #\Escape codes))
(defun dim (s) (concatenate 'string (sgr 2) s (sgr 0)))
(defun bold (s) (concatenate 'string (sgr 1) s (sgr 0)))
(defun cyan (s) (concatenate 'string (sgr 36) s (sgr 0)))
(defun red (s) (concatenate 'string (sgr 31) s (sgr 0)))
(defun green (s) (concatenate 'string (sgr 32) s (sgr 0)))
(defun yellow (s) (concatenate 'string (sgr 33) s (sgr 0)))
(defun reverse-video (s) (concatenate 'string (sgr 7) s (sgr 0)))

(defun term-setup ()
  "Enter raw mode; enable bracketed paste, kitty key disambiguation and
xterm modifyOtherKeys (Shift+Enter detection); returns t on a tty."
  (setf *tty-out* (evo.port:make-fd-output-stream 1))
  (setf *saved-stty* (stty "-g"))
  (stty "raw" "-echo")
  (refresh-size)
  (install-sigwinch)
  (wr (esc "?2004h")                    ; bracketed paste
      (format nil "~c[>1u" #\Escape)    ; kitty: disambiguate escape codes
      (esc ">4;2m"))                    ; xterm modifyOtherKeys
  (flush)
  t)

(defun term-teardown ()
  (when *tty-out*
    (wr (esc ">4;0m")
        (format nil "~c[<u" #\Escape)
        (esc "?2004l")
        (show-cursor))
    (flush))
  (when *saved-stty*
    (stty *saved-stty*))
  (setf *saved-stty* nil))

(defun visible-length (s)
  "Length of S ignoring SGR escape sequences."
  (let ((n 0) (i 0) (len (length s)))
    (loop while (< i len)
          do (let ((c (char s i)))
               (cond ((and (char= c #\Escape) (< (1+ i) len)
                           (char= (char s (1+ i)) #\[))
                      (let ((end (position-if (lambda (ch) (char<= #\@ ch #\~))
                                              s :start (+ i 2))))
                        (setf i (if end (1+ end) len))))
                     (t (incf n) (incf i)))))
    n))

(defun truncate-visible (s max)
  "Truncate S to MAX visible columns, appending … when cut.  SGR-aware."
  (if (<= (visible-length s) max)
      s
      (let ((n 0) (i 0) (len (length s)) (cut nil))
        (loop while (and (< i len) (not cut))
              do (let ((c (char s i)))
                   (cond ((and (char= c #\Escape) (< (1+ i) len)
                               (char= (char s (1+ i)) #\[))
                          (let ((end (position-if (lambda (ch) (char<= #\@ ch #\~))
                                                  s :start (+ i 2))))
                            (setf i (if end (1+ end) len))))
                         (t (when (>= n (1- max)) (setf cut i))
                            (incf n) (incf i)))))
        (concatenate 'string (subseq s 0 (or cut len)) (sgr 0) "…"))))
