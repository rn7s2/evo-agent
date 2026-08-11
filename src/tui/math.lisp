;;;; math.lisp — LaTeX math spans in agent output: the placement seam.
;;;;
;;;; The markdown renderer turns agent text into ANSI one line at a time.
;;;; This file adds a renderer-agnostic seam for math: it FINDS the math
;;;; spans in a line ($…$, $$…$$, \(…\), \[…\]) and, for each, asks a
;;;; pluggable *MATH-RENDERER* to produce whatever bytes should stand in
;;;; for the formula — an inline-image escape from the bundled LaTeX
;;;; extension, typically.  The renderer is the only heavy, optional,
;;;; toolchain-bound part, and it lives in an extension; the grammar and
;;;; placement live here so both the streaming and the transcript-repaint
;;;; paths get it for free (both go through MD-RENDER-LINE / -TEXT).
;;;;
;;;; Two rules keep it safe:
;;;;
;;;;  - Off by default.  *MATH-ENABLED* nil means MD-SPLIT-MATH is never
;;;;    consulted and the markdown renderer behaves exactly as before —
;;;;    zero impact until an extension (or a setting) opts in.
;;;;  - Never emit an image into the managed region.  Region lines are
;;;;    sanitized and width-counted; an image escape there would be stripped
;;;;    and would desync the cursor math.  The live streaming preview binds
;;;;    *MATH-LIVE-PREVIEW* so math renders as its own source there, and the
;;;;    image only lands when a finished line reaches scrollback.

(in-package :evo.tui)

(defvar *math-enabled* nil
  "Master switch.  When NIL the markdown renderer ignores math entirely and
renders byte-for-byte as it did before this file existed — zero impact until
an extension (or a setting) opts in.")

(defvar *math-renderer* nil
  "Function (LATEX DISPLAY-P) -> escape-string, or NIL to fall back to
source.  DISPLAY-P is T for $$…$$ / \\[…\\] block math, NIL for inline.  The
string is spliced verbatim into scrollback, so it is where an image protocol
(iTerm2 inline-image, sixel) is emitted.  Errors and NIL both fall back to
the LaTeX source.")

(defvar *math-live-preview* nil
  "Bound T while rendering the still-streaming line for the managed region:
math renders as source there, never as an image (the region strips control
bytes and counts columns).")

(defun register-math-renderer (fn)
  "Install FN as *MATH-RENDERER* and turn math rendering on (or off when FN
is NIL).  The supported way for an extension to claim math."
  (setf *math-renderer* fn *math-enabled* (and fn t))
  fn)

;;; Grammar.  A small hand scanner rather than a regex: it must skip over
;;; `code spans` (a $ inside backticks is not math) and honour \$ escapes,
;;; and stay stable under a growing prefix so the streaming preview does not
;;; jump — an opener is only honoured once its closer is present.

(defun %math-skip-code-span (text i len)
  "If a backtick run starts at I, return the index just past its closing run
of equal length (or past the opening run when unclosed); else NIL."
  (when (and (< i len) (char= (char text i) #\`))
    (let* ((run (or (position-if-not (lambda (c) (char= c #\`)) text :start i)
                    len))
           (ticks (- run i))
           (j run))
      (loop while (< j len)
            do (if (char= (char text j) #\`)
                   (let ((r (or (position-if-not (lambda (c) (char= c #\`))
                                                 text :start j)
                                len)))
                     (when (= (- r j) ticks)
                       (return-from %math-skip-code-span r))
                     (setf j r))
                   (incf j)))
      run)))

(defun %math-close (text start token &key require-tight)
  "Index of a closing TOKEN at or after START that is not backslash-escaped.
When REQUIRE-TIGHT the character just before it must be non-space (inline
math is `$…x$`, not `$… $`).  NIL when there is none."
  (loop for j = (search token text :start2 start)
          then (search token text :start2 (1+ j))
        while j
        when (and (not (and (plusp j) (char= (char text (1- j)) #\\)))
                  (or (not require-tight)
                      (and (plusp j) (not (char= (char text (1- j)) #\Space)))))
          return j))

(defmacro %math-try (open-len close close-len display)
  "Emit the math segment TEXT[i+OPEN-LEN .. CLOSE] and advance, or step past
the opener when there is no non-empty closer.  Used inside MD-SPLIT-MATH."
  `(cond
     ((and ,close (> ,close (+ i ,open-len)))
      (flush-text i)
      (push (list :math (subseq text (+ i ,open-len) ,close) ,display) segments)
      (setf i (+ ,close ,close-len) text-start i))
     (t (incf i ,open-len))))

(defun md-split-math (text)
  "Split TEXT into an ordered list of segments: (:TEXT string) for prose and
(:MATH latex display-p) for a formula.  Recognises $$…$$ and \\[…\\]
(display) and $…$ and \\(…\\) (inline); a $ inside a code span or written
\\$ stays prose.  With no math the result is one :TEXT segment."
  (let ((len (length text)) (segments nil) (text-start 0) (i 0))
    (flet ((flush-text (upto)
             (when (> upto text-start)
               (push (list :text (subseq text text-start upto)) segments))))
      (loop while (< i len)
            for c = (char text i)
            for skip = (%math-skip-code-span text i len)
            do (cond
                 (skip (setf i skip))
                 ;; \[ … \]  display
                 ((and (char= c #\\) (< (1+ i) len) (char= (char text (1+ i)) #\[))
                  (%math-try 2 (%math-close text (+ i 2) "\\]") 2 t))
                 ;; \( … \)  inline
                 ((and (char= c #\\) (< (1+ i) len) (char= (char text (1+ i)) #\())
                  (%math-try 2 (%math-close text (+ i 2) "\\)") 2 nil))
                 ;; any other backslash escape (\$, \\, …): skip the pair
                 ((and (char= c #\\) (< (1+ i) len)) (incf i 2))
                 ;; $$ … $$  display
                 ((and (char= c #\$) (< (1+ i) len) (char= (char text (1+ i)) #\$))
                  (%math-try 2 (%math-close text (+ i 2) "$$") 2 t))
                 ;; $ … $  inline, tight (no space just inside either end)
                 ((and (char= c #\$) (< (1+ i) len)
                       (not (char= (char text (1+ i)) #\Space))
                       (not (char= (char text (1+ i)) #\$)))
                  (%math-try 1 (%math-close text (+ i 1) "$" :require-tight t) 1 nil))
                 (t (incf i))))
      (flush-text len))
    (nreverse segments)))

(defun math-display-block (escape total)
  "Place a standalone display-math image on its own lines: reserve its TOTAL
rows, draw it at the top (the escape pins the cursor with C=1), and leave the
cursor on the block's foot so the following line lands below the image rather
than overwriting it."
  (if (<= (or total 1) 1)
      escape
      (with-output-to-string (out)
        (dotimes (i (1- total)) (write-char #\Newline out))   ; reserve the rows
        (write-string (cursor-up (1- total)) out)             ; back to the top
        (write-string escape out)                             ; draw (C=1: no move)
        (write-string (cursor-down (1- total)) out))))        ; to the foot

(defun math-source (latex display-p)
  "Literal source fallback for a formula: what shows with no renderer, on a
render failure, or in the live preview."
  (if display-p
      (concatenate 'string "$$" latex "$$")
      (concatenate 'string "$" latex "$")))

(defun render-math-span (latex display-p)
  "(values BYTES IMAGE-P TOTAL-ROWS ASCENT-ROWS COLS ADVANCE): the renderer's
escape when one is installed and we are not in the region preview (IMAGE-P t,
TOTAL-ROWS its cell height, ASCENT-ROWS how many of those rows sit above the
formula's baseline — or NIL to let the layout pick from :MATH-INLINE-VALIGN,
COLS its width in cells for wrap budgeting and manual stepping), else the
LaTeX source (IMAGE-P nil).  ADVANCE is :SELF when the escape itself leaves
the cursor stepped past the image (the terminal advances x, exactly), or NIL
when the layout must step by COLS (an estimate).  Any renderer error degrades
to source — a bad formula must never take down the render thread."
  (multiple-value-bind (esc total ascent cols advance)
      (if (and *math-renderer* (not *math-live-preview*))
          (ignore-errors (funcall *math-renderer* latex display-p))
          (values nil nil nil nil nil))
    (if esc
        (values esc t (or total 1) ascent cols advance)
        (values (math-source latex display-p) nil nil nil nil nil))))

;;; Inline line assembly.  A line may mix prose and one or more inline
;;; formula images.  Terminal images occupy whole cells and flow the cursor
;;; DOWNWARD, so naively splicing several onto one line makes them stair-step
;;; 'lower and lower'.  Three strategies, chosen by :MATH-INLINE-MODE:
;;;
;;;   :aligned  (default) keep the prose as real, selectable text on one line
;;;             and place the images with cursor moves at a chosen vertical
;;;             alignment (:MATH-INLINE-VALIGN :bottom (default) | :center |
;;;             :top).  This depends on how the terminal advances the cursor
;;;             after an inline image — :MATH-INLINE-ADVANCE overrides that
;;;             count when a terminal differs from the default (height - 1).
;;;   :break    (safe everywhere) each image ends its physical line, so there
;;;             is at most one per line and no stair-step — the fallback for a
;;;             terminal where :aligned's cursor math does not hold.
;;;   :raw      splice images inline with no correction (exhibits the
;;;             stair-step; for comparison/debugging).

(defun math-inline-mode () (setting :math-inline-mode :aligned))
(defun math-inline-valign () (setting :math-inline-valign :bottom))
(defun math-inline-advance (h)
  "Rows the cursor drops after an inline image of height H — from the
terminal's behaviour, overridable via :MATH-INLINE-ADVANCE."
  (let ((override (setting :math-inline-advance nil)))
    (max 0 (if override override (1- h)))))

(defun %valign-ascent (total)
  "Fallback ascent (rows above the baseline) from :MATH-INLINE-VALIGN, for a
renderer that reports only a height: :bottom puts the baseline at the image's
bottom row, :top at its first row, :center in the middle."
  (case (math-inline-valign)
    (:top 0)
    (:center (floor (1- total) 2))
    (t (1- total))))                    ; :bottom (default)

(defun %item-total (it)
  (if (eq (first it) :image) (or (third it) 1) 1))

(defun %item-ascent (it)
  "Rows of IT above the text baseline row: prose sits ON the baseline (0); an
image uses its reported ascent, clamped, or the :MATH-INLINE-VALIGN fallback."
  (if (eq (first it) :image)
      (let ((total (or (third it) 1)))
        (max 0 (min (1- total) (or (fourth it) (%valign-ascent total)))))
      0))

(defun %item-descent (it)
  "Rows of IT below the text baseline row."
  (- (%item-total it) 1 (%item-ascent it)))

(defun %item-cols (it)
  "Width of an image IT in cells (for stepping the cursor past it), or NIL."
  (and (eq (first it) :image) (fifth it)))

(defun %item-self-advance-p (it)
  "T when IT's escape leaves the cursor stepped past the image on its own
(the terminal computes the exact column count; ours is an estimate)."
  (and (eq (first it) :image) (eq (sixth it) :self)))

(defun %item-budget-cols (it)
  "Columns to budget for IT when wrapping.  For an image, the renderer's
estimate plus slack: the terminal lays images out from a fractional CSS cell
width while the estimate divides by an integer one, so a wide image can cost
a few more columns than estimated — wrap early, never late."
  (if (eq (first it) :image)
      (let ((cols (or (%item-cols it) 1)))
        (+ cols 1 (ceiling cols 8)))
      (visible-length (second it))))

(defun math-line-raw (items)
  "Splice every segment inline with no cursor correction."
  (apply #'concatenate 'string (mapcar #'second items)))

(defun math-line-break (items)
  "One image per physical line: newline after each placed image (except a
trailing one), so nothing stair-steps."
  (with-output-to-string (out)
    (loop for (it . rest) on items
          do (write-string (second it) out)
             (when (and (eq (first it) :image) rest)
               (write-char #\Newline out)))))

(defun math-partition-items (items width)
  "Split ITEMS into sub-lines each fitting WIDTH display columns, so a line
mixing prose and formula images wraps HERE, by items and cells, instead of
being hard-wrapped mid-image by the terminal (which would wreck the block's
reserved-row geometry).  Prose may break at any cell boundary (SGR sequences
and wide characters are kept whole); an image that would overflow starts a
new sub-line.  Returns a list of item lists — always at least one."
  (let ((sublines nil) (cur nil) (col 0))
    (labels ((break-line ()
               (when cur (push (nreverse cur) sublines))
               (setf cur nil col 0)))
      (dolist (it items)
        (ecase (first it)
          (:image
           (let ((need (%item-budget-cols it)))
             (when (and (plusp col) (> (+ col need) width))
               (break-line))
             (push it cur)
             (incf col need)))
          (:text
           (let* ((s (second it)) (len (length s)) (i 0) (seg 0))
             (loop while (< i len)
                   do (let ((c (char s i)))
                        (cond
                          ;; keep SGR sequences whole (and free)
                          ((and (char= c #\Escape) (< (1+ i) len)
                                (char= (char s (1+ i)) #\[))
                           (let ((end (position-if
                                       (lambda (ch) (char<= #\@ ch #\~))
                                       s :start (+ i 2))))
                             (setf i (if end (1+ end) len))))
                          (t (let ((w (char-display-width c)))
                               (when (and (plusp col) (> (+ col w) width))
                                 (when (> i seg)
                                   (push (list :text (subseq s seg i)) cur))
                                 (setf seg i)
                                 (break-line))
                               (incf col w)
                               (incf i))))))
             (when (> len seg)
               (push (list :text (subseq s seg)) cur))))))
      (break-line))
    (or (nreverse sublines) (list nil))))

(defun math-subline-aligned (items)
  "Lay out one physical line: prose as text with each formula's BASELINE on
the text baseline row.  Prose is written normally (the terminal advances the
cursor for it); an image is drawn at its top row with the cursor returned to
the baseline afterwards.  Two stepping modes per image: :SELF advance trusts
the escape to move the cursor past the image (the terminal's own, exact,
column count — the cursor lands on the image's bottom row, so only a vertical
correction is needed); otherwise a save/restore plus a COLS-estimate step.
The block reserves ASCENT rows above the baseline and DESCENT below so the
moves have room."
  (let* ((above (reduce #'max items :initial-value 0 :key #'%item-ascent))
         (below (reduce #'max items :initial-value 0 :key #'%item-descent))
         (h (+ above 1 below)))            ; block height; text baseline row = ABOVE
    (with-output-to-string (out)
      ;; Reserve the block (scroll room) and sit on the baseline row.
      (when (> h 1)
        (dotimes (i (1- h)) (write-char #\Newline out))
        (write-string (cursor-up below) out))     ; from the foot up to the baseline
      (dolist (it items)
        (ecase (first it)
          (:text (write-string (second it) out))  ; cursor advances with the text
          (:image
           (let ((ascent (%item-ascent it))
                 (descent (%item-descent it))
                 (cols (%item-cols it)))
             (cond
               ((%item-self-advance-p it)
                (write-string (cursor-up ascent) out)   ; rise to the top row
                (write-string (second it) out)          ; draw; cursor -> bottom row,
                (write-string (cursor-up descent) out)) ; x stepped; back to baseline
               (t
                (write-string (save-cursor) out)     ; remember the baseline position
                (write-string (cursor-up ascent) out); rise to the image's top row
                (write-string (second it) out)       ; draw it (C=1: no move)
                (write-string (restore-cursor) out)  ; back exactly to the baseline
                (when (and cols (plusp cols))
                  (write-string (cursor-right cols) out)))))))) ; step past it
      ;; Leave the cursor at the block's foot for the next line.
      (when (> h 1) (write-string (cursor-down below) out)))))

(defun math-line-aligned (items)
  "MATH-SUBLINE-ALIGNED over width-aware sub-lines: the logical line wraps by
items and cells first, then each sub-line gets its own baseline block.  A
sub-line's height is the max ascent/descent of what is ON it, so a tall
formula opens only its own line, exactly like browser inline-math layout."
  (with-output-to-string (out)
    (loop for (subline . rest) on (math-partition-items items (max 20 *cols*))
          do (write-string (math-subline-aligned subline) out)
             (when rest (write-char #\Newline out)))))

(defun math-assemble-line (items)
  "Combine rendered ITEMS — (:text string) and (:image bytes total ascent) —
into one scrollback line, according to :MATH-INLINE-MODE.  With no images this
is a plain concatenation, so nothing changes for ordinary prose."
  (if (notany (lambda (it) (eq (first it) :image)) items)
      (math-line-raw items)
      (ecase (math-inline-mode)
        (:raw (math-line-raw items))
        (:break (math-line-break items))
        (:aligned (math-line-aligned items)))))

;;; Multi-line display math: a line that is exactly $$ (or \[) opens a block
;;; that runs to the matching $$ / \]; the lines between accumulate and the
;;; whole thing renders as one image at the close.

(defun %math-trim (line)
  (string-trim '(#\Space #\Tab) line))

(defun math-open-display-line-p (line)
  "T if LINE, trimmed, is a bare display-math opener with no closer on it —
$$ or \\[ standing alone."
  (let ((s (%math-trim line)))
    (or (string= s "$$") (string= s "\\["))))

(defun math-close-display-line-p (line)
  "T if LINE, trimmed, is a bare display-math closer — $$ or \\]."
  (let ((s (%math-trim line)))
    (or (string= s "$$") (string= s "\\]"))))

(defun math-starts-with-opener-p (line)
  "T if LINE, trimmed, OPENS a multi-line display block: it starts with $$ or
\\[ carrying content, and has NO matching closer later on the same line.  A
self-contained single-line formula ($$…$$ / \\[…\\]) is NOT an opener — it is
rendered inline by MD-SPLIT-MATH — so this must reject it, or the single-line
form would swallow the prose that follows until the next stray closer."
  (let ((s (%math-trim line)))
    (cond
      ((and (eql 0 (search "$$" s)) (not (string= s "$$")))
       (null (%math-close s 2 "$$")))
      ((and (eql 0 (search "\\[" s)) (not (string= s "\\[")))
       (null (%math-close s 2 "\\]")))
      (t nil))))

(defun math-ends-with-closer-p (line)
  "T if LINE, trimmed, ends with $$ or \\] but is NOT exactly that — i.e. the
closer is on the same line as the last content."
  (let ((s (%math-trim line)))
    (or (and (>= (length s) 3)
             (string= s "$$" :start1 (- (length s) 2))
             (not (string= s "$$")))
        (and (>= (length s) 3)
             (string= s "\\]" :start1 (- (length s) 2))
             (not (string= s "\\]"))))))

(defun %math-strip-trailing (s suffix)
  "Remove SUFFIX from the end of S; NIL if S does not end with it."
  (let ((pos (- (length s) (length suffix))))
    (and (>= pos 0) (string= s suffix :start1 pos) (subseq s 0 pos))))

(defun %math-strip-closer (s)
  "Strip the trailing $$ or \\] from S; NIL if S ends with neither."
  (or (%math-strip-trailing s "$$")
      (%math-strip-trailing s "\\]")))
