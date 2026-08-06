;;;; input.lisp — incremental byte-stream -> key-event parser.
;;;;
;;;; Fed by the tui poll loop; unconsumed bytes (partial escape sequences,
;;;; unterminated bracketed pastes, split UTF-8) stay in the buffer until the
;;;; next tick.  A lone ESC is only emitted after the caller reports quiet
;;;; ticks (see PARSE-KEYS :flush-escape).
;;;;
;;;; Events: (:char c) (:paste string) :enter :shift-enter :newline
;;;; :backspace :delete :up :down :left :right :home :end :word-left
;;;; :word-right :escape (:ctrl char) (:super char) :delete-word :shift-tab

(in-package :evo.tui)

(defun now-ms ()
  "The poll loop's clock (milliseconds, monotonic within a session)."
  (round (* 1000 (/ (get-internal-real-time) internal-time-units-per-second))))

(defvar *input-trace* :unchecked
  "Path from EVO_INPUT_TRACE, or NIL.  Resolved once, on first use.")

(defun input-trace (label datum)
  "Append one line to EVO_INPUT_TRACE when that names a file.

The TUI owns the terminal, so on a machine one cannot attach to, the only
way to learn what a key actually produced is to write it down: the bytes as
they arrived, the events the parser made of them, and the events that
survived paste coalescing.  Off unless the variable is set."
  (when (eq *input-trace* :unchecked)
    (let ((path (uiop:getenv "EVO_INPUT_TRACE")))
      (setf *input-trace* (and path (plusp (length path)) path))))
  (when *input-trace*
    (ignore-errors
     (with-open-file (out *input-trace* :direction :output :if-exists :append
                                        :if-does-not-exist :create
                                        :external-format :utf-8)
       (format out "~6d ~a ~s~%" (now-ms) label datum)))))

(defstruct (input-state (:conc-name in-))
  (buffer (make-array 0 :element-type '(unsigned-byte 8)
                        :adjustable t :fill-pointer t)))

(defun in-push-bytes (state bytes)
  (loop for b across bytes do (vector-push-extend b (in-buffer state))))

(defun utf8-length (lead)
  (cond ((< lead #x80) 1)
        ((= (logand lead #xE0) #xC0) 2)
        ((= (logand lead #xF0) #xE0) 3)
        ((= (logand lead #xF8) #xF0) 4)
        (t 1)))

(defun decode-utf8 (bytes)
  (handler-case (flexi-streams:octets-to-string
                 (coerce bytes '(vector (unsigned-byte 8)))
                 :external-format :utf-8)
    (error () (string (code-char #xFFFD)))))   ; U+FFFD replacement character

(defparameter *paste-end* #(27 91 50 48 49 126)) ; ESC [ 2 0 1 ~

(defun find-subseq (needle haystack start)
  (search needle haystack :start2 start))

;;; ---------------------------------------------------------------------
;;; Pasting.
;;;
;;; Text arrives from the clipboard in one of two shapes, and an editor has
;;; to understand both:
;;;
;;;   1. Bracketed paste.  The terminal wraps the payload in ESC[200~ ...
;;;      ESC[201~ and hands it over as data (parsed above, emitted as a
;;;      single :PASTE event).
;;;
;;;   2. Nothing at all.  The clipboard is simply typed at us, byte for
;;;      byte, as fast as the pty will carry it — because the terminal has
;;;      no bracketed-paste mode, because a multiplexer or ssh hop ate the
;;;      request, or because the paste came from `tmux send-keys`/an
;;;      automation script.  Every line break in it is the same byte the
;;;      Enter key sends, so a naive reader submits the first line as a
;;;      prompt and then races the rest of the clipboard into the model one
;;;      line at a time.  COALESCE-PASTE-BURST below recovers the paste.
;;;
;;; Both shapes end at the same door: NORMALIZE-PASTE, then the editor.
;;;
;;; NORMALIZE-PASTE is where the line breaks are settled.  Terminals do not
;;; agree on how a break inside a paste is spelled: many send CR (that is
;;; what Enter produces, and xterm.js — VS Code, Cursor, and everything
;;; built on it — rewrites every newline in the clipboard to CR before
;;; sending it), Windows clipboards send CRLF, some send bare LF.  Raw mode
;;; turns off the tty's own CR->LF translation, so all three reach us
;;; verbatim and all three mean "new line".  Dropping CR instead of
;;; translating it welds every line of a paste into one — "return 1" +
;;; "end" arrives as "return 1end", losing the lines and the whitespace
;;; that separated them.
;;;
;;; The rest is sanitising: a paste is text, not keystrokes.  ANSI escape
;;; sequences are removed whole (a stray ESC would be painted straight back
;;; at the terminal by the next repaint — SANITIZE-LINE keeps ESC, since
;;; that is how styling reaches the screen — and the bytes after it would
;;; be swallowed as a control sequence), and so are the other C0 controls.
;;; TAB and LF survive: pasted code is full of tabs and the painter gives
;;; them a fixed width.

(defun normalize-paste (text)
  "A paste payload -> the text the user copied.  CRLF and lone CR become
LF, ANSI escape sequences and other control characters are dropped, TAB and
LF survive."
  (let ((out (make-string-output-stream))
        (len (length text))
        (i 0))
    (flet ((peek (k) (and (< (+ i k) len) (char text (+ i k)))))
      (loop while (< i len)
            do (let ((c (char text i)))
                 (cond
                   ;; ESC [ ... final-byte: an escape sequence, dropped whole.
                   ((and (char= c #\Escape) (eql (peek 1) #\[))
                    (let ((end (position-if (lambda (ch) (char<= #\@ ch #\~))
                                            text :start (+ i 2))))
                      (setf i (if end (1+ end) len))))
                   ((char= c #\Return)
                    (write-char #\Newline out)
                    (when (eql (peek 1) #\Newline) (incf i)) ; CRLF is one break
                    (incf i))
                   ((or (char= c #\Newline) (char= c #\Tab))
                    (write-char c out) (incf i))
                   ((or (< (char-code c) 32) (= (char-code c) 127)) (incf i))
                   (t (write-char c out) (incf i))))))
    (get-output-stream-string out)))

;;; Paste bursts: a paste from a terminal that does not bracket them.
;;;
;;; The tell is arrival rate.  The poll loop drains everything the tty has
;;; every tick (~20ms), so one batch of key events is one 20ms window of
;;; input: a human fills it with at most one or two characters, while a
;;; paste fills it with as many as the pty will carry.  A batch that is
;;; nothing but text and line breaks and holds at least
;;; *PASTE-BURST-MIN-CHARS* characters was therefore not typed — it was
;;; pasted, and it is folded into a :PASTE event so it takes the same path
;;; as a bracketed one (placeholder collapse, image-path attachment, no
;;; accidental submits).
;;;
;;; The batch is the clock, so there are no timers here and no need for
;;; codex's held-first-char/retro-grab dance: evo sees the whole window at
;;; once instead of one key at a time.
;;;
;;; Line breaks: a break *inside* the burst is part of the pasted text.  A
;;; break at the very end is ambiguous — it is either the last line's
;;; newline or the user's own Enter — so it is held for one tick: if more
;;; pasted text follows it was interior after all, and otherwise it is
;;; released as a real Enter.  That keeps `tmux send-keys "/help\r"` and
;;; every scripted driver working (text, then submit) while a pasted block
;;; of prose still lands in the editor as one prompt.
;;;
;;; The tail of a paste is often a short batch, so a batch of one or two
;;; characters still joins the burst — but only while the input never
;;; stopped (*PASTE-BURST-GAP-MS*, a couple of polls).  A human's next
;;; keystroke is a tenth of a second away at best, so typing that follows a
;;; paste is typing again immediately.
;;;
;;; A false positive costs nothing: the same characters are inserted at the
;;; same place, one tick later.

(defparameter *paste-burst-min-chars* 3
  "Characters in a single poll batch that make it a paste, not typing.")

(defparameter *paste-burst-gap-ms* 50
  "How long a burst stays open for a short trailing batch: long enough for
a late poll, far short of the ~100ms a human needs to press the next key.")

(defun paste-burst-wanted-p (&optional (flag (uiop:getenv "EVO_PASTE_BURST")))
  "Should a fast batch of plain keys be read as a paste?  Yes, unless
EVO_PASTE_BURST says otherwise: the detection is what makes pasting work at
all on a terminal without bracketed paste, and a false positive only inserts
the same characters one tick later."
  (not (and flag (member flag '("0" "off" "no" "false") :test #'string-equal))))

(defstruct (paste-burst (:conc-name pb-))
  (text (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
  (enabled (paste-burst-wanted-p))
  (active nil)          ; the previous batch was paste-like
  (last-ms 0)           ; when that batch arrived
  (held-enter nil))     ; a trailing line break, waiting to be classified

(defun pb-push (pb char)
  (vector-push-extend char (pb-text pb)))

(defun pb-take (pb)
  "The buffered burst as a string, clearing it; NIL when empty."
  (let ((text (pb-text pb)))
    (when (plusp (fill-pointer text))
      (prog1 (subseq text 0)
        (setf (fill-pointer text) 0)))))

(defun char-event-p (event)
  (and (consp event) (eq (first event) :char)))

(defun text-event-p (event)
  (or (char-event-p event) (eq event :enter)))

(defun batch-paste-like-p (events)
  "Is this batch of events a paste rather than typing?"
  (and events
       (every #'text-event-p events)
       (>= (count-if #'char-event-p events) *paste-burst-min-chars*)))

(defun paste-burst-flush (pb)
  "End any burst in flight; returns the events it owes the caller."
  (let ((text (pb-take pb))
        (events nil))
    (when text (push (list :paste text) events))
    (when (shiftf (pb-held-enter pb) nil) (push :enter events))
    (setf (pb-active pb) nil)
    (nreverse events)))

(defun coalesce-paste-burst (pb events &optional (now (now-ms)))
  "One poll batch of key EVENTS, with unbracketed pastes folded into
:PASTE events.  Typing passes through untouched."
  (let* ((paste-like (batch-paste-like-p events))
         ;; A burst also swallows a short batch that arrives before the
         ;; input has had time to stop — the tail of the same paste.
         (bursting (or paste-like
                       (and (pb-active pb) events
                            (every #'text-event-p events)
                            (<= (- now (pb-last-ms pb)) *paste-burst-gap-ms*))))
         (out nil))
    (labels ((emit (event) (push event out))
             (flush () (mapc #'emit (paste-burst-flush pb))))
      (cond
        (bursting
         ;; Text after a held break means the break was inside the paste.
         (when (shiftf (pb-held-enter pb) nil) (pb-push pb #\Newline))
         (loop for rest on events
               for event = (car rest)
               do (cond ((char-event-p event) (pb-push pb (second event)))
                        ((cdr rest) (pb-push pb #\Newline))   ; interior break
                        (t (setf (pb-held-enter pb) t)))))    ; trailing break
        (t
         (flush)                        ; whatever was in flight has ended
         (mapc #'emit events)))
      (setf (pb-active pb) paste-like
            (pb-last-ms pb) now)
      (nreverse out))))

;;; Modified keys: one decoder, three spellings.
;;;
;;; A key with a modifier reaches us in whichever encoding the terminal
;;; chose, and TERM-SETUP asks for two of them at once (kitty's CSI-u and
;;; xterm's modifyOtherKeys), so both must be understood in full.  They
;;; carry the same pair — a unicode code point and a modifier bitmask —
;;; only the frame differs:
;;;
;;;   legacy            ctrl+v -> #x16
;;;   kitty CSI-u       ctrl+v -> ESC [ 118 ; 5 u
;;;   modifyOtherKeys   ctrl+v -> ESC [ 27 ; 5 ; 118 ~
;;;
;;; Decoding one spelling and dropping the other is worse than not asking
;;; for it: the key does nothing at all, silently, on exactly the terminals
;;; that honour the request.  That bug ate ctrl+v (image paste) wherever
;;; modifyOtherKeys was live, so this decoder is deliberately total — every
;;; (code, modifier) pair either maps to an event or is explicitly dropped.

(defun modifier-bits (mod)
  "Kitty/xterm modifier parameter -> bitmask (1 shift, 2 alt, 4 ctrl, 8 super)."
  (max 0 (1- (or mod 1))))

(defun modified-key (code mod)
  "Event for unicode CODE pressed with modifier parameter MOD, or NIL when
the combination has no meaning here.  Shared by the CSI-u and
modifyOtherKeys paths."
  (let* ((bits (modifier-bits mod))
         (shift (logtest bits 1))
         (alt (logtest bits 2))
         (ctrl (logtest bits 4))
         (super (logtest bits 8)))
    (cond
      ;; Named keys keep their meaning under any modifier.
      ;; (ctrl+enter keeps sending, as it did before this decoder existed.)
      ((= code 13) (cond (alt :newline) (shift :shift-enter) (t :enter)))
      ((= code 27) :escape)
      ((= code 9) (if shift :shift-tab (list :char #\Tab)))
      ((member code '(8 127)) :backspace)
      ;; cmd+key, when a terminal reports it instead of eating it: only
      ;; cmd+v means anything to us (the macOS paste gesture), and mapping
      ;; the rest onto ctrl would be dangerous — cmd+c is not ctrl+c.
      (super (when (member code '(86 118)) (list :super #\v)))
      ;; Ctrl+letter, in either case, matching the legacy C0-byte path.
      (ctrl (cond ((<= 97 code 122) (list :ctrl (code-char code)))
                  ((<= 65 code 90) (list :ctrl (code-char (+ code 32))))
                  (t nil)))
      ;; A printable key with no modifier that changes it: plain text.
      ;; (Alt is dropped, as in the legacy path.)
      ((and (not alt) (<= 32 code 126)) (list :char (code-char code)))
      (t nil))))

(defun csi-key (params final)
  "Map a CSI sequence (PARAMS: list of integers, FINAL: char) to an event."
  (case final
    (#\A :up) (#\B :down) (#\C (if (member 5 (cdr params)) :word-right
                                   (if (member 3 (cdr params)) :word-right :right)))
    (#\D (if (member 5 (cdr params)) :word-left
             (if (member 3 (cdr params)) :word-left :left)))
    (#\H :home) (#\F :end)
    (#\Z :shift-tab)                    ; CSI Z: back-tab
    (#\u ;; kitty / CSI-u: code;modifiers u  (mod = 1 + shift:1 alt:2 ctrl:4)
     (modified-key (or (first params) 0) (or (second params) 1)))
    (#\~ (let ((n (first params)))
           (case n
             ((1 7) :home) ((4 8) :end) (3 :delete)
             (27 ;; modifyOtherKeys: 27;mod;code~ — same pair, other order
              (modified-key (or (third params) 0) (or (second params) 1)))
             (t nil))))
    (t nil)))

(defun parse-keys (state &key flush-escape)
  "Parse buffered bytes into events.  FLUSH-ESCAPE: treat a trailing lone ESC
as the Escape key (caller saw quiet ticks).  Returns the list of events."
  (let ((buf (in-buffer state))
        (events nil)
        (i 0))
    (labels ((emit (e) (when e (push e events)))
             (bytes-left () (- (fill-pointer buf) i))
             (at (k) (and (< (+ i k) (fill-pointer buf)) (aref buf (+ i k)))))
      (loop
        (when (zerop (bytes-left)) (return))
        (let ((b (aref buf i)))
          (cond
            ;; --- escape sequences ---
            ((= b 27)
             (let ((b2 (at 1)))
               (cond
                 ((null b2)
                  (if flush-escape (progn (emit :escape) (incf i)) (return)))
                 ((= b2 91)             ; CSI
                  ;; Bracketed paste start?
                  (if (and (eql (at 2) 50) (eql (at 3) 48) (eql (at 4) 48) (eql (at 5) 126))
                      (let ((end (find-subseq *paste-end* buf (+ i 6))))
                        (if end
                            (progn
                              ;; Raw payload: HANDLE-PASTE is the one door
                              ;; where pasted text is normalized, so that a
                              ;; burst and a bracketed paste cannot drift.
                              (emit (list :paste (decode-utf8 (subseq buf (+ i 6) end))))
                              (setf i (+ end (length *paste-end*))))
                            (return)))  ; wait for the rest of the paste
                      ;; Generic CSI: params then final byte in @..~
                      (let ((j (+ i 2)))
                        (loop while (and (< j (fill-pointer buf))
                                         (not (<= 64 (aref buf j) 126)))
                              do (incf j))
                        (if (>= j (fill-pointer buf))
                            (if (> (- j i) 32)
                                (setf i j) ; junk guard: discard runaway CSI
                                (return))  ; incomplete, wait
                            (let* ((param-str (decode-utf8 (subseq buf (+ i 2) j)))
                                   (params (mapcar (lambda (p) (parse-integer p :junk-allowed t))
                                                   (uiop:split-string param-str :separator '(#\;))))
                                   (final (code-char (aref buf j))))
                              (emit (csi-key (remove nil params) final))
                              (setf i (1+ j)))))))
                 ((= b2 79)             ; SS3
                  (let ((b3 (at 2)))
                    (if (null b3)
                        (return)
                        (progn (emit (case (code-char b3)
                                       (#\A :up) (#\B :down) (#\C :right) (#\D :left)
                                       (#\H :home) (#\F :end) (t nil)))
                               (incf i 3)))))
                 ((= b2 13) (emit :newline) (incf i 2))     ; Alt+Enter fallback
                 ((= b2 98) (emit :word-left) (incf i 2))   ; Alt+b
                 ((= b2 102) (emit :word-right) (incf i 2)) ; Alt+f
                 ((= b2 127) (emit :delete-word) (incf i 2)); Alt+Backspace
                 ((= b2 27)                                 ; ESC ESC
                  (emit :escape) (emit :escape) (incf i 2))
                 ;; Alt+Ctrl+key, legacy spelling: ESC then the control
                 ;; byte (ctrl+alt+v = ESC SYN).  The alt is dropped and
                 ;; the ctrl kept, exactly as MODIFIED-KEY does for the
                 ;; same chord in CSI-u or modifyOtherKeys — otherwise
                 ;; ctrl+alt+v, the shortcut for terminals that eat plain
                 ;; ctrl+v, would work in two encodings out of three.
                 ((and (< b2 27) (not (member b2 '(8 9 10 13))))
                  (emit (list :ctrl (code-char (+ 96 b2)))) (incf i 2))
                 (t (incf i 2)))))      ; unknown alt-key: drop
            ;; --- plain bytes ---
            ((or (= b 13) (= b 10)) (emit :enter) (incf i))
            ((or (= b 127) (= b 8)) (emit :backspace) (incf i))
            ((= b 9) (emit (list :char #\Tab)) (incf i))
            ((< b 27)
             (emit (list :ctrl (code-char (+ 96 b))))
             (incf i))
            ((< b 32) (incf i))
            (t                          ; UTF-8 text
             (let ((len (utf8-length b)))
               (if (< (bytes-left) len)
                   (return)             ; split multibyte char, wait
                   (progn
                     (emit (list :char (char (decode-utf8 (subseq buf i (+ i len))) 0)))
                     (incf i len))))))))
      ;; Keep the unconsumed tail.
      (let ((rest (subseq buf i)))
        (setf (fill-pointer buf) 0)
        (loop for b across rest do (vector-push-extend b buf)))
      (nreverse events))))
