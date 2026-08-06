;;;; term.lisp — terminal control for the tui core extension.
;;;;
;;;; Raw mode + size come from evo.port (stty on Unix, SetConsoleMode on
;;;; Windows) — the platform lives there, the escape sequences live here.
;;;; Output is ANSI escapes on a dedicated fd-stream.  Live resize is
;;;; mandatory: SIGWINCH sets a flag the main loop polls, and where there is
;;;; no SIGWINCH (Windows) the main loop asks the terminal its size instead.

(in-package :evo.tui)

(defvar *tty-out* nil)
(defvar *saved-term-mode* nil)
(defvar *resized* nil)
(defvar *rows* 24)
(defvar *cols* 80)

(defvar *poll-for-resize* nil
  "T when this platform has no resize signal, so TICK must look for one.")

(defun refresh-size ()
  (multiple-value-bind (rows cols) (evo.port:terminal-size)
    (when (and rows cols)
      (setf *rows* rows *cols* cols)))
  (values *rows* *cols*))

(defun install-sigwinch ()
  "Ask for SIGWINCH; fall back to polling when the platform has no such
signal.  Either way *RESIZED* is what the main loop reads."
  (setf *poll-for-resize*
        (not (evo.port:install-signal-handler
              evo.port:+sigwinch+
              (lambda () (setf *resized* t))))))

(defun poll-terminal-resize ()
  "Notice a resize the hard way, where no signal announces it.  One console
call per tick, and only on the platforms that need it."
  (when *poll-for-resize*
    (multiple-value-bind (rows cols) (evo.port:terminal-size)
      (when (and rows cols (or (/= rows *rows*) (/= cols *cols*)))
        (setf *resized* t)))))

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

;;; Key reporting: ask for more than ASCII, and take back what you ask for.
;;;
;;; A modified key (ctrl+v, shift+enter, cmd+v) only reaches an application
;;; if the terminal is asked to report it, and there are two live requests
;;; for that: kitty's progressive enhancement (CSI > 1 u) and xterm's
;;; modifyOtherKeys (CSI > 4 ; 2 m).  Terminals ignore the one they do not
;;; implement, so evo asks for both and decodes both (see MODIFIED-KEY);
;;; the pair covers every modern emulator, and the legacy control byte
;;; covers the rest.
;;;
;;; Two rules keep that graceful.  Ask only where the answer can be
;;; understood: TERM=dumb (and CI capture, and an editor's "terminal" that
;;; is really a log pane) gets no request, because a terminal that echoes
;;; the request instead of honouring it litters the transcript.  And pop
;;; exactly what was pushed, so the shell evo exits into is not left in a
;;; mode its own line editor never asked for.

(defvar *key-enhancement* nil
  "T while the kitty/modifyOtherKeys requests of TERM-SETUP are in force.")

(defun key-enhancement-wanted-p (&optional (flag (uiop:getenv "EVO_KEY_ENHANCEMENT"))
                                           (term (or (uiop:getenv "TERM") "")))
  "Should evo ask this terminal for enhanced key reporting?
EVO_KEY_ENHANCEMENT=0 is the escape hatch for an emulator that claims a
protocol and then mangles it — the TUI stays fully usable without it, only
ctrl+v and shift+enter fall back to their legacy spellings."
  (let ((flag flag)
        (term (or term "")))
    (cond ((and flag (member flag '("0" "off" "no" "false") :test #'string-equal)) nil)
          ((and flag (member flag '("1" "on" "yes" "true") :test #'string-equal)) t)
          ((zerop (length term)) nil)
          ((string-equal term "dumb") nil)
          (t t))))

(defun term-setup ()
  "Enter raw mode; enable bracketed paste, kitty key disambiguation and
xterm modifyOtherKeys (ctrl+v, cmd+v, Shift+Enter detection); returns t on
a tty."
  (setf *tty-out* (evo.port:make-stdout-stream))
  (setf *saved-term-mode* (evo.port:terminal-raw-mode))
  (refresh-size)
  (install-sigwinch)
  (wr (esc "?2004h"))                   ; bracketed paste
  (setf *key-enhancement* (key-enhancement-wanted-p))
  (when *key-enhancement*
    (wr (format nil "~c[>1u" #\Escape)  ; kitty: disambiguate escape codes
        (esc ">4;2m")))                 ; xterm modifyOtherKeys
  (flush)
  t)

(defun term-teardown ()
  (when *tty-out*
    (when *key-enhancement*
      (wr (esc ">4;0m")
          (format nil "~c[<u" #\Escape))
      (setf *key-enhancement* nil))
    (wr (esc "?2004l")
        (show-cursor))
    (flush))
  (when *saved-term-mode*
    (evo.port:restore-terminal-mode *saved-term-mode*))
  (setf *saved-term-mode* nil))

(defun char-display-width (c)
  "Terminal columns C occupies (wcwidth approximation).  The region's
cursor math assumes every painted line is exactly one terminal row, so
this must not under-count: a wide char counted as 1 makes the line wrap
and every later repaint then strands a copy of it in scrollback.  Tabs
are 4 (the painter expands them to 4 spaces), other control characters 0
(the painter drops them)."
  (let ((cp (char-code c)))
    (cond
      ((char= c #\Tab) 4)
      ((or (< cp 32) (= cp 127)) 0)
      ((< cp #x0300) 1)                       ; fast path: latin-1 and friends
      ((<= #x0300 cp #x036F) 0)               ; combining marks
      ((or (<= #x200B cp #x200F) (= cp #x2060)
           (<= #x20D0 cp #x20FF) (<= #xFE00 cp #xFE0F))
       0)                                     ; zero-width, variation selectors
      ((or (<= #x1100 cp #x115F)              ; Hangul jamo
           (<= #x2E80 cp #x303E)              ; CJK radicals .. punctuation
           (<= #x3041 cp #x33FF)              ; kana .. CJK compatibility
           (<= #x3400 cp #x4DBF)              ; CJK extension A
           (<= #x4E00 cp #x9FFF)              ; CJK unified ideographs
           (<= #xA000 cp #xA4CF)              ; Yi
           (<= #xAC00 cp #xD7A3)              ; Hangul syllables
           (<= #xF900 cp #xFAFF)              ; CJK compatibility ideographs
           (<= #xFE30 cp #xFE4F)              ; CJK compatibility forms
           (<= #xFF00 cp #xFF60)              ; fullwidth forms
           (<= #xFFE0 cp #xFFE6)              ; fullwidth signs
           (<= #x1F300 cp #x1FAFF)            ; emoji
           (<= #x20000 cp #x3FFFD))           ; CJK extensions B+
       2)
      (t 1))))

(defun visible-length (s)
  "Display columns S occupies, ignoring SGR escape sequences (wide
characters count 2, zero-width characters 0)."
  (let ((n 0) (i 0) (len (length s)))
    (loop while (< i len)
          do (let ((c (char s i)))
               (cond ((and (char= c #\Escape) (< (1+ i) len)
                           (char= (char s (1+ i)) #\[))
                      (let ((end (position-if (lambda (ch) (char<= #\@ ch #\~))
                                              s :start (+ i 2))))
                        (setf i (if end (1+ end) len))))
                     (t (incf n (char-display-width c)) (incf i)))))
    n))

(defun truncate-visible (s max)
  "Truncate S to at most MAX display columns, appending … when cut.
SGR-aware and wide-character-aware: the result never paints past MAX
columns, so a truncated region line cannot wrap the terminal."
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
                         (t (let ((w (char-display-width c)))
                              (if (> (+ n w) (1- max))
                                  (setf cut i)
                                  (progn (incf n w) (incf i))))))))
        (concatenate 'string (subseq s 0 (or cut len)) (sgr 0) "…"))))

(defun wrap-visible (s width)
  "Soft-wrap S into a list of rows, each at most WIDTH display columns.
SGR-aware and wide-character-aware: escape sequences don't count toward
WIDTH and are never split; a wide char that wouldn't fit starts a new row.
Always returns at least one row.  Wrapping is by cell, so a prefix's wrap
points don't move when more text is appended — the streaming preview stays
stable as it grows.  Callers re-wrap every repaint, so it tracks resizes."
  (let ((width (max 1 width))
        (rows nil)
        (row (make-string-output-stream))
        (col 0) (i 0) (len (length s)))
    (flet ((emit-row () (push (get-output-stream-string row) rows) (setf col 0)))
      (loop while (< i len)
            do (let ((c (char s i)))
                 (cond
                   ((and (char= c #\Escape) (< (1+ i) len)
                         (char= (char s (1+ i)) #\[))
                    (let* ((end (position-if (lambda (ch) (char<= #\@ ch #\~))
                                             s :start (+ i 2)))
                           (stop (if end (1+ end) len)))
                      (write-string (subseq s i stop) row)
                      (setf i stop)))
                   (t (let ((w (char-display-width c)))
                        (when (and (plusp col) (> (+ col w) width))
                          (emit-row))
                        (write-char c row)
                        (incf col w)
                        (incf i))))))
      (emit-row))
    (nreverse rows)))
