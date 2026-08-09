;;;; markdown.lisp — agent output styling: line-oriented markdown -> ANSI.
;;;;
;;;; Agent text reaches scrollback one line at a time while streaming, so
;;;; rendering is line-oriented with a single piece of cross-line state:
;;;; whether we are inside a ``` fenced code block (the md struct).  Code
;;;; block content renders verbatim.  Inline styles toggle independent SGR
;;;; attributes (bold 1/22, italic 3/23, code as color 33/39) so nesting
;;;; needs no stack, and a marker is only honoured when its closing marker
;;;; exists later in the same line — unmatched markers render literally,
;;;; which also keeps the live preview of a half-streamed line stable as
;;;; it grows.

(in-package :evo.tui)

(defstruct (md (:conc-name md-))
  (in-code nil)
  ;; While inside a bare $$ / \[ … \] display-math block, this holds the
  ;; accumulated LaTeX (a string); NIL otherwise.  Mirrors IN-CODE: one
  ;; slot of cross-line state, so nesting needs no stack.
  (in-math nil))

;;; Prose-styler seam.  The inline renderer emits some text verbatim — the
;;; words BETWEEN markdown markers, outside `code` spans and link URLs.  An
;;; extension can restyle exactly those runs (bionic reading bolds each word's
;;; leading letters, say) by installing a *PROSE-STYLER*.  It is a peer of the
;;; math seam: off by default, so with nothing installed the renderer is
;;; byte-for-byte what it was, and a signalling styler falls back to source.

(defvar *prose-styler* nil
  "Function (TEXT) -> styled-text applied to each run of plain prose the
inline renderer would otherwise emit verbatim — the words between markers,
never inside a `code` span or a link URL, and never inside already-bold text
(a heading, or **strong**), which a bold-toggling styler must not disturb.
TEXT carries no ANSI; the result may add some.  NIL is identity — with no
styler installed the markdown renderer behaves exactly as before, so this
seam has zero impact until an extension opts in.  The bundled reader is
extensions/350-bionic-reader.lisp.")

(defvar *prose-styling-suppressed* nil
  "Bound T while the renderer emits text that is ALREADY fully bold (a
heading): a prose styler that toggles bold — bionic reading does — must not
fire there, or it would only un-bold the letters it did not fixate.")

(defun register-prose-styler (fn)
  "Install FN as *PROSE-STYLER* (NIL removes it) and return it.  The supported
way for an extension to restyle plain prose words, e.g. bionic reading."
  (setf *prose-styler* fn))

(defun style-prose (text)
  "Route a plain-prose run through *PROSE-STYLER*, falling back to TEXT on a
NIL result or a signalling styler.  Byte-for-byte identity when no styler is
installed or styling is suppressed (already-bold context)."
  (if (and *prose-styler* (not *prose-styling-suppressed*))
      (or (ignore-errors (funcall *prose-styler* text)) text)
      text))

(defun md-fence-p (line)
  "A ``` fence line (up to 3 spaces of indent, per CommonMark — deeper
indented backticks inside a code block must not toggle the fence)."
  (let ((i (or (position-if-not (lambda (c) (char= c #\Space)) line)
               (length line))))
    (and (<= i 3)
         (<= (+ i 3) (length line))
         (string= "```" line :start2 i :end2 (+ i 3)))))

;;; Inline pass.

(defun md-code-close (text start ticks)
  "Index of the closing run of exactly TICKS backticks at or after START."
  (let ((i start) (len (length text)))
    (loop while (< i len)
          do (if (char= (char text i) #\`)
                 (let ((run (or (position-if-not (lambda (c) (char= c #\`))
                                                 text :start i)
                                len)))
                   (when (= (- run i) ticks) (return i))
                   (setf i run))
                 (incf i)))))

(defun md-strong-close (text start marker2)
  "Index of a closing double MARKER2 at or after START, preceded by
non-space."
  (loop for j = (search marker2 text :start2 start)
          then (search marker2 text :start2 (1+ j))
        while j
        when (not (char= (char text (1- j)) #\Space))
          return j))

(defun md-emph-close (text start marker)
  "Index of a closing single MARKER at or after START: preceded by
non-space, not doubled, and for _ not running into a word."
  (loop for j from start below (length text)
        when (and (char= (char text j) marker)
                  (not (char= (char text (1- j)) #\Space))
                  (or (= j (1- (length text)))
                      (char/= (char text (1+ j)) marker))
                  (or (char/= marker #\_)
                      (= j (1- (length text)))
                      (not (alphanumericp (char text (1+ j))))))
          return j))

(defun md-inline (text)
  "Inline markdown -> ANSI: **strong**, *emph* (also _), `code`, and
[text](url) links.  Unmatched markers stay literal.  Plain prose runs — what
this would otherwise emit verbatim, outside code spans, link URLs and bold —
are collected and passed to STYLE-PROSE (identity by default), the seam a
reader like bionic reading installs."
  (let ((len (length text))
        (bold-close nil)     ; index of the pending ** closer
        (emph-close nil)     ; index of the pending * / _ closer
        (buf (make-string-output-stream)))  ; pending plain-prose run
    (with-output-to-string (out)
      ;; Drain the buffered prose before emitting anything that is not prose
      ;; (an SGR toggle, a code span, link markup): the styler only ever sees a
      ;; contiguous run in one style state, and BOLD-CLOSE tells it whether we
      ;; are inside **strong** (already bold — leave it alone).  When no styler
      ;; is installed STYLE-PROSE is identity, so the bytes are unchanged.
      (flet ((drain ()
               (let ((run (get-output-stream-string buf)))
                 (when (plusp (length run))
                   (write-string (if bold-close run (style-prose run)) out)))))
        (loop with i = 0
              while (< i len)
              do (let ((c (char text i)))
                   (cond
                     ;; pending closers first: ** before * at the same index
                     ((and bold-close (= i bold-close))
                      (drain)
                      (write-string (sgr 22) out)
                      (setf bold-close nil)
                      (incf i 2))
                     ((and emph-close (= i emph-close))
                      (drain)
                      (write-string (sgr 23) out)
                      (setf emph-close nil)
                      (incf i))
                     ;; `code` span: content is protected from other styling
                     ((char= c #\`)
                      (let* ((run (or (position-if-not (lambda (ch) (char= ch #\`))
                                                       text :start i)
                                      len))
                             (ticks (- run i))
                             (close (md-code-close text run ticks)))
                        (cond
                          (close
                           (drain)
                           (write-string (sgr-role :code) out)  ; theme-aware
                           (write-string (subseq text run close) out)
                           (write-string (sgr 39) out)
                           (setf i (+ close ticks)))
                          (t (write-string (subseq text i run) buf)
                             (setf i run)))))
                     ;; **strong** / __strong__
                     ((and (member c '(#\* #\_))
                           (null bold-close)
                           (< (+ i 2) len)
                           (char= (char text (1+ i)) c)
                           (not (char= (char text (+ i 2)) #\Space))
                           (char/= (char text (+ i 2)) c))
                      (let ((j (md-strong-close text (+ i 3)
                                                (make-string 2 :initial-element c))))
                        (cond (j (drain)
                                 (write-string (sgr 1) out)
                                 (setf bold-close j)
                                 (incf i 2))
                              (t (write-char c buf) (incf i)))))
                     ;; *emph* / _emph_
                     ((and (member c '(#\* #\_))
                           (null emph-close)
                           (< (1+ i) len)
                           (not (char= (char text (1+ i)) #\Space))
                           (char/= (char text (1+ i)) c)
                           (or (char/= c #\_)   ; _ opens only at a word edge
                               (zerop i)
                               (not (alphanumericp (char text (1- i))))))
                      (let ((j (md-emph-close text (+ i 2) c)))
                        (cond (j (drain)
                                 (write-string (sgr 3) out)
                                 (setf emph-close j)
                                 (incf i))
                              (t (write-char c buf) (incf i)))))
                     ;; [text](url)
                     ((char= c #\[)
                      (let* ((mid (search "](" text :start2 (1+ i)))
                             (end (and mid (position #\) text :start (+ mid 2)))))
                        (cond
                          (end
                           (drain)
                           (let ((label (subseq text (1+ i) mid))
                                 (url (subseq text (+ mid 2) end)))
                             (write-string (sgr 4) out)
                             (write-string (if bold-close label (style-prose label))
                                           out)
                             (write-string (sgr 24) out)
                             (unless (equal label url)
                               (write-string (dim (format nil " (~a)" url)) out))
                             (setf i (1+ end))))
                          (t (write-char c buf) (incf i)))))
                     (t (write-char c buf) (incf i)))))
        ;; drain the trailing prose, then reset a style left open by a closer
        ;; swallowed inside a code span.
        (drain)
        (when (or bold-close emph-close)
          (write-string (sgr 0) out))))))

(defun md-inline-math (text)
  "MD-INLINE, but with math spans ($…$, \\(…\\), and single-line $$…$$ /
\\[…\\]) carved out first and handed to RENDER-MATH-SPAN; the prose between
them still gets full inline styling.  When math is off, or the line holds no
math, this is exactly MD-INLINE — so the old rendering is untouched."
  (if (not *math-enabled*)
      (md-inline text)
      (let ((segs (md-split-math text)))
        (if (or (null segs)
                (and (null (rest segs)) (eq (first (first segs)) :text)))
            (md-inline text)
            ;; Render each segment to an item, then let MATH-ASSEMBLE-LINE lay
            ;; the line out per :MATH-INLINE-MODE.  Prose keeps full inline
            ;; styling; a math span becomes an (:image bytes height) item, or
            ;; falls back to styled source text when no image was produced.
            (math-assemble-line
             (mapcar (lambda (seg)
                       (if (eq (first seg) :text)
                           (list :text (md-inline (second seg)))
                           (multiple-value-bind (bytes image-p total ascent cols advance)
                               (render-math-span (second seg) (third seg))
                             (if image-p
                                 (list :image bytes total ascent cols advance)
                                 (list :text bytes)))))
                     segs))))))

;;; Block pass.

(defun md-hrule-p (trimmed)
  (let ((bare (remove #\Space trimmed)))
    (and (>= (length bare) 3)
         (member (char bare 0) '(#\- #\* #\_))
         (every (lambda (c) (char= c (char bare 0))) bare))))

(defun md-ordered-end (trimmed)
  "End index of an ordered-list prefix (\"12. \" / \"3) \"), or nil."
  (let ((d (or (position-if-not #'digit-char-p trimmed) (length trimmed))))
    (and (<= 1 d 3)
         (< (1+ d) (length trimmed))
         (member (char trimmed d) '(#\. #\)))
         (char= (char trimmed (1+ d)) #\Space)
         (1+ d))))

(defun md-block-line (line)
  (let* ((indent (or (position-if-not (lambda (c) (char= c #\Space)) line)
                     (length line)))
         (pad (subseq line 0 indent))
         (rest (subseq line indent)))
    (cond
      ((zerop (length rest)) line)
      ;; # heading — marks stay (dim) so the level remains readable
      ((and (zerop indent) (char= (char rest 0) #\#))
       (let ((hashes (or (position-if-not (lambda (c) (char= c #\#)) rest)
                         (length rest))))
         (if (and (<= hashes 6)
                  (or (= hashes (length rest))
                      (char= (char rest hashes) #\Space)))
             (concatenate 'string (dim (subseq rest 0 hashes))
                          (bold (let ((*prose-styling-suppressed* t))
                                  (md-inline-math (subseq rest hashes)))))
             (md-inline-math line))))
      ((md-hrule-p rest)
       (dim (make-string (max 10 (1- *cols*)) :initial-element #\─)))
      ;; > blockquote — dim, one ▌ per nesting level
      ((char= (char rest 0) #\>)
       (let ((depth 0) (j 0))
         (loop while (and (< j (length rest))
                          (member (char rest j) '(#\> #\Space)))
               do (when (char= (char rest j) #\>) (incf depth))
                  (incf j))
         (dim (concatenate 'string pad
                           (make-string depth :initial-element #\▌)
                           " " (subseq rest j)))))
      ;; - bullet list
      ((and (>= (length rest) 2)
            (member (char rest 0) '(#\- #\* #\+))
            (char= (char rest 1) #\Space))
       (concatenate 'string pad (cyan "•") (md-inline-math (subseq rest 1))))
      ;; 1. ordered list
      ((md-ordered-end rest)
       (let ((end (md-ordered-end rest)))
         (concatenate 'string pad (cyan (subseq rest 0 end))
                      (md-inline-math (subseq rest end)))))
      (t (concatenate 'string pad (md-inline-math rest))))))

;;; Entry points.

(defun md-render-line (line md)
  "Render one complete markdown LINE for scrollback, advancing the fence and
display-math state in MD.  Returns NIL to SUPPRESS a line: the interior
lines of a multi-line $$…$$ block produce nothing, and the whole formula is
emitted as one unit on its closing line.  Callers must skip a NIL result."
  (cond
    ;; Code fences win over everything (a $$ inside a code block is literal).
    ((md-fence-p line)
     (setf (md-in-code md) (not (md-in-code md)))
     (dim line))
    ((md-in-code md) line)
    ;; Inside a bare $$ / \[ display block: accumulate until the closer,
    ;; then render the whole formula at once.
    ((md-in-math md)
     (cond
       ((math-close-display-line-p line)
        (let ((latex (md-in-math md)))
          (setf (md-in-math md) nil)
          (multiple-value-bind (bytes image-p total)
              (render-math-span (string-right-trim '(#\Newline) latex) t)
            (if image-p (math-display-block bytes total) bytes))))
       (t (setf (md-in-math md)
                (concatenate 'string (md-in-math md) line (string #\Newline)))
          nil)))
    ;; A bare $$ / \[ opens a display block (only when math is on; otherwise
    ;; it is ordinary text and must render verbatim as before).
    ((and *math-enabled* (math-open-display-line-p line))
     (setf (md-in-math md) "")
     nil)
    (t (md-block-line line))))

(defun md-render-preview (line md)
  "Render a still-streaming LINE for the managed region without advancing the
real fence/math state.  Math renders as its own source here — never an image
(the region strips control bytes and counts columns).  Always a string."
  (let ((*math-live-preview* t)
        ;; A copy whose IN-MATH is cleared: the preview line shows its own
        ;; source rather than being swallowed by an open display block.
        (probe (copy-md md)))
    (setf (md-in-math probe) nil)
    (or (md-render-line line probe) "")))

(defun md-render-text (text)
  "Render complete multi-line markdown TEXT with a fresh fence state,
dropping suppressed (NIL) lines so a multi-line formula joins cleanly."
  (let ((md (make-md)))
    (string-join (string #\Newline)
                 (remove nil
                         (mapcar (lambda (line) (md-render-line line md))
                                 (uiop:split-string text :separator '(#\Newline)))))))
