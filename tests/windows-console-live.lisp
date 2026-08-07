;;;; windows-console-live.lisp — verify EVO.PORT's console *output* moving
;;;; parts against a REAL Windows console: size (GetConsoleScreenBufferInfo),
;;;; raw-mode set/restore (SetConsoleMode), and UTF-8 glyph rendering.
;;;;
;;;;   sbcl --non-interactive --load tests/windows-console-live.lisp
;;;;
;;;; Uses CONOUT$/CONIN$ directly, so it works even when this process's own
;;;; std handles are redirected pipes (as under a tool harness).  The output
;;;; check writes the exact bytes evo's stdout stream would and reads the
;;;; glyphs back out of the screen buffer with ReadConsoleOutputCharacterW.

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
(sb-alien:define-alien-routine ("WriteFile" %write-file) sb-alien:int
  (h sb-alien:system-area-pointer) (buf (* sb-alien:unsigned-char))
  (n sb-alien:unsigned-int) (written (* sb-alien:unsigned-int))
  (overlapped sb-alien:system-area-pointer))

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

  ;; --- OUTPUT encoding: write evo's exact bytes, read the glyphs back -----
  ;; UTF-8 both ways is what TERMINAL-RAW-MODE establishes; write the box
  ;; drawing and CJK the TUI paints and confirm the console stored the right
  ;; code points, not mojibake.
  (ignore-errors (%set-console-output-cp +utf8-codepage+))
  (ignore-errors (%set-console-cp +utf8-codepage+))
  (format t "  std-external-format = ~s~%" (std-external-format))
  (finish-output)
  (flet ((write-glyphs (row glyphs)
           (let ((octets (sb-ext:string-to-octets glyphs
                                                   :external-format (std-external-format))))
             (%set-cursor conout (logior 0 (ash row 16)))
             (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-char 64))
                                   (written sb-alien:unsigned-int))
               (dotimes (i (length octets)) (setf (sb-alien:deref buf i) (aref octets i)))
               ;; WriteFile is what the fd-stream ultimately calls; the console
               ;; decodes these UTF-8 bytes by its (65001) output code page.
               (%write-file conout (sb-alien:cast (sb-alien:addr (sb-alien:deref buf 0))
                                                  (* sb-alien:unsigned-char))
                            (length octets) (sb-alien:addr written) (sb-sys:int-sap 0)))))
         (read-cells (row n)
           (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-short 32))
                                 (got sb-alien:unsigned-int))
             (%read-output-chars conout
                                 (sb-alien:cast (sb-alien:addr (sb-alien:deref buf 0))
                                                (* sb-alien:unsigned-short))
                                 n (logior 0 (ash row 16)) (sb-alien:addr got))
             (map 'string (lambda (i) (code-char (sb-alien:deref buf i)))
                  (loop for i below n collect i)))))
    ;; Single-width glyphs land one per cell: an exact cell-for-cell round trip.
    ;; U+2500 box, U+276F ornament, U+25CB ring, U+00E9 e-acute.
    (let ((glyphs (map 'string #'code-char '(#x7C #x2500 #x276F #x25CB #x00E9 #x7C))))
      (write-glyphs 0 glyphs)
      (let ((back (read-cells 0 (length glyphs))))
        (format t "  single-width row: ~{U+~4,'0X~^ ~}~%" (map 'list #'char-code back))
        (ok "console stored the box-drawing/ornament/accent glyphs evo wrote"
            (string= back glyphs))))
    ;; A CJK ideograph is double-width (two cells); the console keeps the code
    ;; point in the lead cell.  Proves the wide glyph the TUI paints is decoded,
    ;; not mojibake'd — the failure mode docs/windows.md warns about.  Read a
    ;; span of cells and assert both ideographs are present (their exact column
    ;; depends on the console's double-width cell accounting).
    (let ((cjk (map 'string #'code-char '(#x4E2D #x6587)))) ; 中 文
      (write-glyphs 2 cjk)
      (let* ((span (read-cells 2 6))
             (codes (map 'list #'char-code span)))
        (format t "  CJK row cells: ~{U+~4,'0X~^ ~}~%" codes)
        (ok "console stored the double-width CJK ideographs evo wrote"
            (and (member #x4E2D codes) (member #x6587 codes) t)))))
  (format t "~%~d passed, ~d failed~%" *pass* *fail*)
  (sb-ext:exit :code (if (zerop *fail*) 0 1)))
