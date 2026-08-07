;;;; windows-console-live.lisp — verify EVO.PORT's console *output* moving
;;;; parts against a REAL Windows console: size (GetConsoleScreenBufferInfo),
;;;; raw-mode set/restore (SetConsoleMode), and glyph rendering.
;;;;
;;;;   sbcl --non-interactive --load tests/windows-console-live.lisp
;;;;
;;;; Size and raw-mode use CONOUT$/CONIN$ directly, so they work even when this
;;;; process's std handles are redirected pipes.  The glyph round-trip, by
;;;; contrast, drives evo's ACTUAL output stream — EVO.PORT:MAKE-STDOUT-STREAM
;;;; built and then handed to TERMINAL-RAW-MODE, exactly as src/tui/term.lisp
;;;; wires it — and reads the glyphs back out of the screen buffer.  It makes NO
;;;; assumption about the console's codepage or the stream's encoding: it writes
;;;; characters and asks the console which code points landed.  That is the fix
;;;; for the blind spot the earlier version had — it hand-encoded with
;;;; STD-EXTERNAL-FORMAT and wrote the bytes with the *narrow* WriteFile after
;;;; forcing codepage 65001, which only agreed with itself on a console that
;;;; already started at 65001 (the tool harness) and produced mojibake on a real
;;;; console whose codepage is the machine's OEM one (936 here).  evo never
;;;; takes that path: its FD-STREAM writes through the *wide* console API, which
;;;; is codepage-independent.  The wide path needs a real console on stdout
;;;; (evo's stream cannot reach the screen buffer through a redirected pipe), so
;;;; when stdout is redirected the glyph check is skipped, not failed.

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :evo :silent t)

(in-package :evo.port)

(sb-alien:define-alien-routine ("CreateFileW" %create-file-w) sb-alien:system-area-pointer
  (name (* sb-alien:unsigned-short)) (access sb-alien:unsigned-int)
  (share sb-alien:unsigned-int) (sa sb-alien:system-area-pointer)
  (disp sb-alien:unsigned-int) (flags sb-alien:unsigned-int)
  (templ sb-alien:system-area-pointer))
(sb-alien:define-alien-routine ("SetConsoleCursorPosition" %set-cursor)
    sb-alien:int (h sb-alien:system-area-pointer) (coord sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("ReadConsoleOutputCharacterW" %read-output-chars)
    sb-alien:int (h sb-alien:system-area-pointer) (buf (* sb-alien:unsigned-short))
    (len sb-alien:unsigned-int) (coord sb-alien:unsigned-int) (got (* sb-alien:unsigned-int)))
(sb-alien:define-alien-routine ("GetConsoleOutputCP" %get-output-cp) sb-alien:unsigned-int)

(defun open-con (name)
  (sb-alien:with-alien ((wname (sb-alien:array sb-alien:unsigned-short 12)))
    (loop for i from 0 for ch across name do (setf (sb-alien:deref wname i) (char-code ch)))
    (setf (sb-alien:deref wname (length name)) 0)
    (%create-file-w (sb-alien:cast (sb-alien:addr (sb-alien:deref wname 0))
                                   (* sb-alien:unsigned-short))
                    #xC0000000 3 (sb-sys:int-sap 0) 3 0 (sb-sys:int-sap 0))))

(defvar *pass* 0)
(defvar *fail* 0)
(defun ok (name form)
  (if form (progn (incf *pass*) (format t "  ok   ~a~%" name))
      (progn (incf *fail*) (format t "  FAIL ~a~%" name)))
  (finish-output))

;;; --- glyph round-trip through evo's REAL output stream -------------------

(defun read-cells (handle row col n)
  "The N code points the screen buffer holds starting at (ROW,COL).  Unread
cells stay #\\Nul (the buffer is zeroed first), so a short read is visible
rather than mistaken for a space.

ReadConsoleOutputCharacterW here fills one fewer cell than asked for — it
returns got = nLength-1 and never writes the last requested slot (measured on
Windows 11, SBCL 2.6.7; consistent across sentinels, rows and cursor moves).
So ask for one extra cell and keep the first N; a console that instead filled
all N would just leave the ignored extra cell unused."
  (let ((want (1+ n)))
    (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-short 64))
                          (got sb-alien:unsigned-int))
      (dotimes (i 64) (setf (sb-alien:deref buf i) 0))
      (setf got 0)
      (%read-output-chars handle
                          (sb-alien:cast (sb-alien:addr (sb-alien:deref buf 0))
                                         (* sb-alien:unsigned-short))
                          want (logior (logand col #xFFFF) (ash row 16)) (sb-alien:addr got))
      (map 'string (lambda (i) (code-char (sb-alien:deref buf i)))
           (loop for i below n collect i)))))

(defun glyph-roundtrip (handle stream row glyphs read-n)
  "Write GLYPHS through STREAM at (ROW,0) and return the READ-N code points the
screen buffer holds there."
  (finish-output stream)                 ; drain STREAM's buffer before we move the cursor
  (%set-cursor handle (ash row 16))
  (write-string glyphs stream)
  (finish-output stream)
  (read-cells handle row 0 read-n))

(defun run-glyph-checks ()
  (let ((h (%std-handle +std-output-handle+)))
    (unless (console-mode h)
      (format t "  -- glyph round-trip skipped: stdout is redirected; run in a real console --~%")
      (finish-output)
      (return-from run-glyph-checks))
    ;; term.lisp's order: build the stream (captures STD-EXTERNAL-FORMAT from the
    ;; console's *current* codepage) THEN raw mode (which flips it to 65001).
    (let ((orig-cp (%get-output-cp))
          (stream (make-stdout-stream))
          (token nil))
      (setf token (terminal-raw-mode))
      (unwind-protect
          (progn
            (format t "  writing through evo's real stdout (~a); console CP now ~a (was ~a)~%"
                    (type-of stream) (%get-output-cp) orig-cp)
            (finish-output)
            ;; Single-width glyphs land one per cell: an exact cell-for-cell round
            ;; trip.  U+2500 box, U+276F ornament, U+25CB ring, U+00E9 e-acute.
            (let* ((glyphs (map 'string #'code-char '(#x7C #x2500 #x276F #x25CB #x00E9 #x7C)))
                   (back (glyph-roundtrip h stream 0 glyphs (length glyphs))))
              (format t "  single-width row: ~{U+~4,'0X~^ ~}~%" (map 'list #'char-code back))
              (ok "evo's stream renders box-drawing/ornament/accent glyphs (any codepage)"
                  (string= back glyphs)))
            ;; A CJK ideograph is double-width (two cells); the console keeps the
            ;; code point in the lead cell.  Read a span and assert both
            ;; ideographs are present (their exact column depends on the console's
            ;; double-width accounting).
            (let* ((cjk (map 'string #'code-char '(#x4E2D #x6587))) ; 中 文
                   (back (glyph-roundtrip h stream 2 cjk 6))
                   (codes (map 'list #'char-code back)))
              (format t "  CJK row cells: ~{U+~4,'0X~^ ~}~%" codes)
              (ok "evo's stream renders double-width CJK ideographs (any codepage)"
                  (and (member #x4E2D codes) (member #x6587 codes) t))))
        (when token (restore-terminal-mode token))
        (ignore-errors (%set-console-output-cp orig-cp))))))

(let ((conout (open-con "CONOUT$"))
      (conin (open-con "CONIN$")))
  (format t "~&== EVO.PORT console output/size/raw-mode against a real console ==~%")

  ;; --- SIZE (terminal-size's engine) -------------------------------------
  (multiple-value-bind (rows cols) (console-size conout)
    (format t "  console-size -> rows=~a cols=~a~%" rows cols)
    (ok "console-size returns plausible rows" (and rows (<= 1 rows 5000)))
    (ok "console-size returns plausible cols" (and cols (<= 1 cols 5000))))

  ;; --- MODE + raw-mode round trip ----------------------------------------
  (let ((in-mode (console-mode conin))
        (out-mode (console-mode conout)))
    (ok "console-mode reads the input handle"  (integerp in-mode))
    (ok "console-mode reads the output handle" (integerp out-mode))
    ;; Enable VT input on CONIN$ (what TERMINAL-RAW-MODE does), then restore.
    (when (integerp in-mode)
      (set-console-mode conin (logior (logandc2 in-mode
                                                (logior +enable-processed-input+
                                                        +enable-line-input+
                                                        +enable-echo-input+))
                                      +enable-virtual-terminal-input+))
      (let ((raw (console-mode conin)))
        (ok "raw mode sets ENABLE_VIRTUAL_TERMINAL_INPUT"
            (plusp (logand raw +enable-virtual-terminal-input+)))
        (ok "raw mode clears ENABLE_LINE_INPUT"
            (zerop (logand raw +enable-line-input+)))
        (ok "raw mode clears ENABLE_ECHO_INPUT"
            (zerop (logand raw +enable-echo-input+))))
      (set-console-mode conin in-mode)
      (ok "restore puts the input mode back exactly" (eql (console-mode conin) in-mode)))
    (when (integerp out-mode)
      (set-console-mode conout (logior out-mode +enable-virtual-terminal-processing+))
      (ok "raw mode sets ENABLE_VIRTUAL_TERMINAL_PROCESSING"
          (plusp (logand (console-mode conout) +enable-virtual-terminal-processing+)))
      (set-console-mode conout out-mode)
      (ok "restore puts the output mode back exactly" (eql (console-mode conout) out-mode))))

  ;; --- OUTPUT: evo's real stream, whatever the console's codepage ---------
  (format t "  std-external-format = ~s~%" (std-external-format))
  (finish-output)
  (run-glyph-checks)

  (format t "~%~d passed, ~d failed~%" *pass* *fail*)
  (sb-ext:exit :code (if (zerop *fail*) 0 1)))
