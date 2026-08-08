;;;; 300-latex-math.lisp — real LaTeX formula rendering for the evo TUI.
;;;;
;;;; The TUI core (src/tui/math.lisp) knows how to FIND math spans in agent
;;;; output — $…$, $$…$$, \(…\), \[…\] — and where to PLACE whatever a
;;;; renderer returns, falling back to the raw LaTeX source when there is no
;;;; renderer.  This extension is the renderer: it rasterizes each formula to
;;;; a PNG with the LaTeX toolchain and emits it inline in scrollback via the
;;;; KITTY graphics protocol, so a formula shows as a real image — the way
;;;; KaTeX/MathJax render it — not an ASCII approximation.
;;;;
;;;; Why kitty and not iTerm2/sixel: VS Code's terminal supports the kitty
;;;; graphics protocol on ALL THREE platforms (macOS, Linux, Windows w/ ConPTY
;;;; >= v2), whereas iTerm2's inline-image protocol and sixel are macOS/Linux
;;;; only there.  One protocol, everywhere.
;;;;
;;;; Why NATIVE pixel size and not cell-scaled: xterm.js (VS Code) blurs any
;;;; image it rescales into the cell grid — with both iTerm2 `height=N` and
;;;; kitty `r=N`.  The ONLY crisp path is to send the PNG at its own pixel size
;;;; and let it occupy however many cells that comes to.  So sizing is by DPI,
;;;; and every formula naturally takes the height it needs.
;;;;
;;;; PIXEL SPACE (learned from the addon's own source, @xterm/addon-image as
;;;; bundled in VS Code): the terminal maps image pixels to CSS pixels, NOT
;;;; device pixels — an image occupies ceil(width / cssCellWidth) columns and
;;;; ceil((height + Y) / cssCellHeight) rows, where a cell is ~9x18 CSS px at
;;;; default zoom (CSI 16 t reports it).  On retina that means 1 image px
;;;; covers 2 device px; the image layer canvas itself is CSS-resolution, so
;;;; that softness is the addon's ceiling, not something DPI can beat.  All
;;;; calibration here is therefore in CSS px: :math-cell-px is the CSS row
;;;; height and :math-cell-w-px the CSS column width.  Getting this wrong is
;;;; catastrophic, not cosmetic: under-reserved rows/cols make images spill
;;;; over later text, images then overwrite each other's cells, and the
;;;; addon's tile accounting deletes them — formulas degrade into gray
;;;; placeholder boxes.
;;;;
;;;; SUPERSAMPLING: dvipng at the target DPI can thin/drop hairlines.  We render
;;;; at :math-dpi × (an integer ≥ :math-supersample) and downsample by that
;;;; whole factor with ImageMagick (a clean integer-ratio Lanczos shrink), which
;;;; antialiases far better at the same on-screen size.  If magick is absent we
;;;; render straight at :math-dpi.
;;;;
;;;; BASELINE: dvipng --depth --height reports the pixels above (height) and
;;;; below (depth) the formula's baseline.  We carry that through so the core
;;;; can sit each formula ON the text baseline (a kitty sub-cell Y offset makes
;;;; it pixel-exact), instead of hanging every image from a fixed row.
;;;;
;;;; Background is always transparent and the glyph colour follows the TUI
;;;; light/dark theme (the shared :theme setting, toggled by /theme).
;;;;
;;;; Settings (override in init.lisp, e.g. (evo:set-setting :math-dpi 120)):
;;;;   :math              t     master on/off
;;;;   :math-dpi          110   on-screen size (CSS px); tune so $x$ ≈ prose
;;;;   :math-supersample  3     render this many× larger, then downsample (AA)
;;;;   :math-cell-px      18    terminal row height in CSS px (CSI 16 t)
;;;;   :math-cell-w-px    9     terminal column width in CSS px (CSI 16 t)
;;;;   :math-baseline-frac 0.8  where the text baseline sits within a row (0..1)
;;;;   :math-snap-px      2     sink/lift a formula <= this many px when that
;;;;                            saves a whole (mostly empty) terminal row
;;;;   :math-x-advance    :terminal   inline stepping: :terminal = the escape
;;;;                            itself advances the cursor by the terminal's
;;;;                            own exact column count (no C=1; xterm.js moves
;;;;                            x by ceil(width/fractional-cell-width), which
;;;;                            an integer :math-cell-w-px cannot reproduce for
;;;;                            wide images); :manual = pin with C=1 and step
;;;;                            by the estimate (for terminals that move the
;;;;                            cursor differently after an image)
;;;;   :math-pixel-align  t     kitty sub-cell Y offset for pixel-exact baseline
;;;;   :math-foreground   nil   xcolor name; nil = follow :theme
;;;;   :math-border       "1pt" whitespace around the formula
;;;;   :math-max-bytes    786432 largest PNG to emit (safety cap)
;;;;
;;;; Commands:  /math status | on | off | clear-cache   ·   /theme dark | light

(in-package :evo.user)

;;; ---------------------------------------------------------------------------
;;; Settings
;;; ---------------------------------------------------------------------------

(defun math-setting (key default) (evo:setting key default))

(defun math-on-p () (and (math-setting :math t) t))
(defun math-dpi () (max 10 (math-setting :math-dpi 110)))
(defun math-border () (math-setting :math-border "1pt"))
(defun math-max-bytes () (math-setting :math-max-bytes (* 768 1024)))

;; Supersample factor as a whole number ≥ 1: the reference :math-supersample
;; rounded UP to the next integer, so the downsample is a clean 1/N shrink.
(defun math-supersample () (max 1 (ceiling (math-setting :math-supersample 3))))

;; Terminal geometry for placement, in CSS pixels — the space the terminal
;; itself lays images out in (see PIXEL SPACE above).  :math-cell-px is the
;; row height and :math-cell-w-px the column width (defaulting to half the
;; height, the usual monospace shape); :math-baseline-frac is where the text
;; baseline sits within a row (≈0.8 = near the bottom, above the descenders).
(defun math-cell-px () (max 1 (math-setting :math-cell-px 18)))
(defun math-cell-w-px ()
  (max 1 (math-setting :math-cell-w-px (round (math-cell-px) 2))))
(defun math-baseline-frac () (max 0.0 (min 1.0 (math-setting :math-baseline-frac 0.8))))
(defun math-pixel-align () (math-setting :math-pixel-align t))
(defun math-snap-px () (max 0 (math-setting :math-snap-px 2)))
(defun math-x-advance () (math-setting :math-x-advance :terminal))

;; Colour follows the TUI light/dark theme unless :math-foreground overrides.
(defun math-theme () (evo:setting :theme :dark))
(defun math-fg ()
  (or (math-setting :math-foreground nil)
      (if (eq (math-theme) :light) "black" "white")))

;;; ---------------------------------------------------------------------------
;;; Toolchain discovery — PATH plus the usual TeX/Homebrew locations, since the
;;; GUI-launched evo may not inherit a login shell's PATH.
;;; ---------------------------------------------------------------------------

(defparameter *math-extra-dirs*
  '("~/bin" "/Library/TeX/texbin" "/usr/local/bin" "/opt/homebrew/bin"
    "/usr/bin" "/bin" "/usr/local/texlive/bin"))

(defvar *math-exe-cache* (make-hash-table :test #'equal))

(defun %expand-dir (dir)
  (if (and (plusp (length dir)) (char= (char dir 0) #\~))
      (merge-pathnames (subseq dir (if (> (length dir) 1) 2 1))
                       (user-homedir-pathname))
      (pathname (concatenate 'string dir "/"))))

(defun %candidate-dirs ()
  (append *math-extra-dirs*
          (let ((path (uiop:getenv "PATH")))
            (and path (uiop:split-string path :separator ":")))))

(defun find-exe (name)
  "Path to executable NAME, searched across *MATH-EXTRA-DIRS* and PATH; NIL when
not found.  Cached.  Returns the NAME-PRESERVING path (not the symlink
truename): TeX programs are multi-call binaries that pick their format from
argv[0], so `latex` must be invoked as `latex`, never as the miktex-pdftex /
pdftex it links to."
  (multiple-value-bind (hit present) (gethash name *math-exe-cache*)
    (if present
        hit
        (setf (gethash name *math-exe-cache*)
              (loop for d in (%candidate-dirs)
                    for p = (merge-pathnames name (%expand-dir d))
                    when (ignore-errors (and (probe-file p) t))
                      return (uiop:native-namestring p))))))

(defun magick-exe () (or (find-exe "magick") (find-exe "convert")))

(defun latex-toolchain-ready-p ()
  "T iff LaTeX + dvipng are present (magick is optional — it only sharpens)."
  (and (find-exe "latex") (find-exe "dvipng") t))

(defun math-usable-p () (and (math-on-p) (latex-toolchain-ready-p)))

;;; ---------------------------------------------------------------------------
;;; Cache — a stable content hash keys an on-disk PNG (+ a tiny metrics file).
;;; ---------------------------------------------------------------------------

(defvar *math-escape-memo* (make-hash-table :test #'equal))

(defun math-cache-dir ()
  (uiop:ensure-directory-pathname
   (merge-pathnames "cache/math/" (evo.util:evo-home))))

(defun math-hash (string)
  "64-bit FNV-1a of STRING as 16 hex chars — a dependency-free, stable cache
key (folds each char's low and high byte)."
  (let ((h 14695981039346656037))
    (loop for c across string
          for cc = (char-code c)
          do (setf h (logand (* (logxor h (logand cc #xff)) 1099511628211)
                             #xffffffffffffffff))
             (setf h (logand (* (logxor h (logand (ash cc -8) #xff)) 1099511628211)
                             #xffffffffffffffff)))
    (format nil "~(~16,'0x~)" h)))

;;; ---------------------------------------------------------------------------
;;; Rasterization: LaTeX -> DVI -> (supersampled) PNG, with baseline metrics
;;; ---------------------------------------------------------------------------

(defun %run (argv &key directory)
  (multiple-value-bind (out err code)
      (uiop:run-program argv :output :string :error-output :string
                             :ignore-error-status t
                             :directory (and directory
                                             (uiop:ensure-directory-pathname directory)))
    (declare (ignore err))
    (values code out)))

(defun latex-document (latex display-p)
  "A tight standalone document around LATEX, glyphs set to the theme colour via
xcolor, on a transparent background.  CAT folds the literal lines at
macroexpansion, so no ~<newline> FORMAT continuation appears (TEST-LINE-ENDINGS)."
  (format nil (cat "\\documentclass[preview,border=~a]{standalone}~%"
                   "\\usepackage{amsmath,amssymb,amsfonts}~%"
                   "\\usepackage{xcolor}~%"
                   "\\begin{document}~%\\color{~a}~%~a~%\\end{document}~%")
          (math-border) (math-fg)
          (if display-p
              (format nil "$\\displaystyle ~a$" latex)
              (format nil "$~a$" latex))))

(defun parse-depth-height (s)
  "(values HEIGHT DEPTH) in px from dvipng's `depth=D height=H` report, or NILs."
  (flet ((num (key)
           (let ((p (search key s)))
             (and p (parse-integer s :start (+ p (length key)) :junk-allowed t)))))
    (values (num "height=") (num "depth="))))

(defun %dvi->png (dvi png dpi)
  "Render DVI to a transparent PNG at DPI; (values HEIGHT-PX DEPTH-PX) from the
--depth/--height report (pixels above / below the baseline), or NIL on failure."
  (multiple-value-bind (code out)
      (%run (list (find-exe "dvipng") "-q" "--depth" "--height"
                  "-D" (princ-to-string dpi) "-T" "tight" "-bg" "Transparent"
                  "-o" (uiop:native-namestring png) (uiop:native-namestring dvi)))
    (when (and (zerop code) (probe-file png))
      (multiple-value-bind (h d) (parse-depth-height out)
        (and h (values h (or d 0)))))))

(defun png-dimensions (path)
  "(values WIDTH HEIGHT) in pixels from a PNG's IHDR, or NIL."
  (let ((o (ignore-errors (evo.util:read-file-octets path))))
    (when (and o (>= (length o) 24))
      (flet ((be32 (i) (+ (ash (aref o i) 24) (ash (aref o (+ i 1)) 16)
                          (ash (aref o (+ i 2)) 8) (aref o (+ i 3)))))
        (values (be32 16) (be32 20))))))

(defun %downsample (png factor)
  "Shrink PNG by 1/FACTOR with a Lanczos filter (supersample AA).  Writes to a
temp file and swaps it in only on success, so a failed/absent magick leaves the
full-resolution PNG intact rather than silently doing nothing in place.  No-op
when FACTOR is 1 or magick is missing.  Returns T iff it actually shrank."
  (when (and (> factor 1) (magick-exe))
    (let ((tmp (merge-pathnames (format nil "~a-ds.png" (pathname-name png)) png)))
      (multiple-value-bind (code)
          (%run (list (magick-exe) (uiop:native-namestring png)
                      "-filter" "Lanczos"
                      "-resize" (format nil "~,4f%" (/ 100.0 factor))
                      (uiop:native-namestring tmp)))
        (when (and (zerop code) (probe-file tmp))
          (ignore-errors (rename-file tmp png))
          (return-from %downsample t))
        (ignore-errors (delete-file tmp)))))
  nil)

(defun %write-metrics (path above below width) ; display-resolution pixels
  (with-open-file (o path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (format o "~d ~d ~d~%" above below width)))

(defun %read-metrics (path)
  (ignore-errors (with-open-file (i path) (list (read i) (read i) (read i)))))

(defun render-latex-png (latex display-p)
  "Rasterize LATEX to a native-size, supersample-antialiased transparent PNG in
the cache.  Returns (values PNG-PATH ABOVE-PX BELOW-PX WIDTH-PX) — pixels above /
below the baseline and the image width, at display size — or NIL on failure.  A
cache hit skips the toolchain entirely."
  (unless (latex-toolchain-ready-p) (return-from render-latex-png nil))
  (let* ((d0 (math-dpi))
         ;; Render straight at the target DPI.  Supersampling needs an external
         ;; downscaler (magick), which silently no-ops in GUI-launched terminals
         ;; that lack its delegate env — leaving 3x-oversized PNGs that overlap
         ;; and blow past xterm's image limit (display math showed as gray
         ;; placeholder boxes).  dvipng antialiases fine at this DPI.
         (ss 1)
         (key (format nil "~a|~a|~a|~a|~a|~a|k3" latex display-p d0 ss
                      (math-fg) (math-border)))
         (h (math-hash key))
         (dir (math-cache-dir))
         (png (merge-pathnames (format nil "~a.png" h) dir))
         (met (merge-pathnames (format nil "~a.txt" h) dir)))
    (let ((m (and (probe-file png) (%read-metrics met))))
      (when m (return-from render-latex-png
                (values png (first m) (second m) (third m)))))
    (ensure-directories-exist dir)
    (let ((tex (merge-pathnames (format nil "~a.tex" h) dir))
          (dvi (merge-pathnames (format nil "~a.dvi" h) dir)))
      (with-open-file (out tex :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
        (write-string (latex-document latex display-p) out))
      (unwind-protect
           (progn
             (%run (list (find-exe "latex") "-interaction=nonstopmode" "-halt-on-error"
                         (format nil "-output-directory=~a" (uiop:native-namestring dir))
                         (uiop:native-namestring tex))
                   :directory dir)
             (unless (probe-file dvi) (return-from render-latex-png nil))
             (multiple-value-bind (rh rd) (%dvi->png dvi png (* d0 ss))
               (unless rh (return-from render-latex-png nil))
               (%downsample png ss)
               ;; Derive the baseline split from the ACTUAL final pixels, not
               ;; from an assumed 1/ss shrink — so the layout always matches
               ;; what is on screen even if the downsample did not run (metrics
               ;; would otherwise say 1 row while a 3x image is displayed).
               (multiple-value-bind (aw ah) (png-dimensions png)
                 (let* ((tot (max 1 (+ rh rd)))
                        (ah (or ah (round tot ss)))
                        (aw (or aw (round tot ss)))
                        (above (max 0 (round (* rh ah) tot)))
                        (below (max 0 (- ah above))))
                   (%write-metrics met above below aw)
                   (and (probe-file png) (values png above below aw))))))
        (dolist (ext '("tex" "dvi" "aux" "log"))
          (ignore-errors
           (delete-file (merge-pathnames (format nil "~a.~a" h ext) dir))))))))

;;; ---------------------------------------------------------------------------
;;; Placement geometry + kitty encoder
;;; ---------------------------------------------------------------------------

(defun %math-place-at (height-px depth-px cell base-y shift)
  "Placement with the image shifted SHIFT px down from true baseline:
(values TOTAL-ROWS ASCENT-ROWS Y-OFF)."
  (let* ((top (+ (- base-y height-px) shift))       ; image top, px rel. trow top
         (bottom (+ base-y depth-px shift))         ; image bottom, px rel. trow top
         (top-row (floor top cell))                 ; rows rel. trow (neg = above)
         (bottom-row (floor (max top (1- bottom)) cell))
         (ascent (max 0 (- top-row)))
         (total (max 1 (+ (- bottom-row top-row) 1)))
         (y-off (mod top cell)))
    (values total ascent y-off)))

(defun math-place (height-px depth-px)
  "From the pixels above (HEIGHT-PX) and below (DEPTH-PX) the baseline, and the
cell height + baseline fraction, return (values TOTAL-ROWS ASCENT-ROWS Y-OFF):
how many rows the image spans, how many lie above the text baseline row, and the
sub-cell pixel offset of the image top within its top row (0 ≤ Y-OFF < cell).
A formula poking at most :MATH-SNAP-PX past a row boundary is nudged off the
true baseline by that amount instead of billing a whole extra terminal row —
imperceptible, and it keeps line spacing tight."
  (let* ((cell (math-cell-px))
         (snap (math-snap-px))
         (base-y (round (* (math-baseline-frac) cell)))
         (over-top (- height-px base-y))            ; px poking above the row
         (over-bot (mod (+ base-y depth-px) cell))  ; px into the last row
         (shifts (list 0)))
    ;; candidate nudges, preferring none: sink to absorb a tiny top overflow,
    ;; lift to vacate a barely-entered bottom row.
    (when (and (plusp over-top) (<= over-top snap))
      (setf shifts (append shifts (list over-top))))
    (when (and (plusp over-bot) (<= over-bot snap))
      (setf shifts (append shifts (list (- over-bot)))))
    (let ((best 0) (best-rows nil))
      (dolist (s shifts)
        (let ((rows (%math-place-at height-px depth-px cell base-y s)))
          (when (or (null best-rows) (< rows best-rows))
            (setf best s best-rows rows))))
      (%math-place-at height-px depth-px cell base-y best))))

(defun kitty-apc (b64 &optional (extra ""))
  "A kitty graphics APC that transmits+displays a PNG (f=100,a=T) at native
pixel size, base64 payload chunked into <=4096-byte packets (control data on
the first, m=1 = more follows, m=0 = last)."
  (with-output-to-string (out)
    (let ((len (length b64)) (i 0) (first t))
      (loop
        (let* ((end (min len (+ i 4096)))
               (chunk (subseq b64 i end))
               (more (if (< end len) 1 0)))
          (if first
              (format out "~C_Gf=100,a=T~a,m=~d;~a~C\\" #\Escape extra more chunk #\Escape)
              (format out "~C_Gm=~d;~a~C\\" #\Escape more chunk #\Escape))
          (setf first nil i end)
          (when (>= i len) (return)))))))

(defun kitty-escape (png-path height-px depth-px width-px display-p)
  "(values ESCAPE TOTAL-ROWS ASCENT-ROWS COLS ADVANCE): a kitty escape drawing
the PNG at native size, with a sub-cell Y offset (when :math-pixel-align) so its
baseline lands on the text baseline row.  DISPLAY blocks always pin the cursor
with C=1 — the core reserves their rows and positions them explicitly; a moved
cursor there would desync the layout and degrade images to placeholders.  INLINE
images, in :terminal mode (the default), OMIT C=1: the terminal then advances x
past the image by its OWN column count — exact, where our COLS (an integer-cell
estimate) can be short for wide images — and leaves the cursor on the image's
bottom row, which the core corrects; ADVANCE :self reports that contract.  In
:manual mode inline also pins with C=1 and the core steps by COLS.  COLS is the
image width in cells, used for wrap budgeting (and manual stepping)."
  (let ((octets (evo.util:read-file-octets png-path)))
    (when (> (length octets) (math-max-bytes)) (return-from kitty-escape nil))
    (multiple-value-bind (total ascent y-off) (math-place height-px depth-px)
      (let* ((self (and (not display-p) (eq (math-x-advance) :terminal)))
             (cols (max 1 (ceiling width-px (math-cell-w-px))))
             (extra (format nil "~a~a"
                            (if self "" ",C=1")
                            (if (and (math-pixel-align) (plusp y-off))
                                (format nil ",Y=~d" y-off) ""))))
        (values (kitty-apc (evo.util:octets->base64 octets) extra)
                total ascent cols (and self :self))))))

;;; ---------------------------------------------------------------------------
;;; The renderer the core seam calls
;;; ---------------------------------------------------------------------------

(defvar *math-render-count* 0)

(defun latex-math-render (latex display-p)
  "Return (values ESCAPE TOTAL-ROWS ASCENT-ROWS COLS ADVANCE) rendering LATEX,
or NIL to fall back to source.  TOTAL-ROWS is the terminal rows the image spans;
ASCENT-ROWS how many sit above the text baseline; COLS its width in cells;
ADVANCE :self when the escape steps the cursor itself (see KITTY-ESCAPE).
Installed as EVO.TUI:*MATH-RENDERER*."
  (when (and (math-usable-p)
             (stringp latex)
             (plusp (length (string-trim '(#\Space #\Tab #\Newline) latex)))
             (<= (length latex) 2000))
    (let ((memo-key (format nil "~a|~a|~a|~a|~a|~a|~a|~a|~a|~a|~a"
                            (math-fg) latex display-p
                            (math-dpi) (math-supersample) (math-cell-px)
                            (math-cell-w-px) (math-baseline-frac)
                            (math-snap-px) (math-x-advance)
                            (and (math-pixel-align) t))))
      (multiple-value-bind (hit present) (gethash memo-key *math-escape-memo*)
        (if present
            (when hit (values-list hit))
            (multiple-value-bind (png h d w) (render-latex-png latex display-p)
              (multiple-value-bind (esc total ascent cols advance)
                  (if png
                      (kitty-escape png h d w display-p)
                      (values nil nil nil nil nil))
                (when esc (incf *math-render-count*))
                (setf (gethash memo-key *math-escape-memo*)
                      (and esc (list esc total ascent cols advance)))
                (when esc (values esc total ascent cols advance)))))))))

;;; ---------------------------------------------------------------------------
;;; Prompt note — with rendering ON, the agent should WRITE math as LaTeX.
;;; Models often avoid $…$ in terminal contexts and approximate formulas with
;;; Unicode; this note reverses that, and is withdrawn when math is off so the
;;; agent never emits markup the user would see raw.
;;; ---------------------------------------------------------------------------

(defun math-prompt-note ()
  (cat "## Mathematical notation~%"
       "This terminal renders LaTeX math as real typeset images.  Write "
       "mathematics as LaTeX: `$...$` inline within prose, `$$...$$` on its "
       "own lines for display equations (also `\\(...\\)` / `\\[...\\]`).  "
       "Do not approximate formulas with Unicode superscripts or ASCII art; "
       "prefer proper LaTeX for anything mathematical."))

(defun math-sync-prompt-note ()
  "Register the note when math will actually render, withdraw it otherwise."
  (evo:register-prompt-note "latex-math"
                            (and (math-usable-p)
                                 (format nil (math-prompt-note)))))

(defun math-status-text ()
  (format nil (cat "math ~a · theme ~a · protocol kitty · "
                   "latex ~a dvipng ~a · dpi ~d · cell ~dx~d css px · rendered ~d")
          (if (and (math-on-p) evo.tui:*math-enabled*) "on" "off")
          (string-downcase (math-theme))
          (if (find-exe "latex") "✓" "✗")
          (if (find-exe "dvipng") "✓" "✗")
          (math-dpi) (math-cell-w-px) (math-cell-px) *math-render-count*))

(defun math-clear-cache ()
  (clrhash *math-escape-memo*)
  (let ((n 0))
    (dolist (f (ignore-errors (directory (merge-pathnames "*.*" (math-cache-dir)))))
      (when (ignore-errors (delete-file f)) (incf n)))
    n))

(defun math-command (ctx)
  (let ((arg (string-downcase (string-trim " " (or (getf ctx :args) "")))))
    (cond
      ((or (string= arg "") (string= arg "status")) (math-status-text))
      ((string= arg "on")
       (evo:set-setting :math t)
       (evo.tui:register-math-renderer #'latex-math-render)
       (math-sync-prompt-note)
       "math rendering on")
      ((string= arg "off")
       (evo:set-setting :math nil)
       (setf evo.tui:*math-enabled* nil)
       (math-sync-prompt-note)
       "math rendering off")
      ((or (string= arg "clear-cache") (string= arg "clear"))
       (format nil "cleared ~d cached file~:p" (math-clear-cache)))
      (t "usage: /math [status | on | off | clear-cache]"))))

(evo:register-command "math" #'math-command
                      :description "LaTeX math rendering (kitty): status | on | off | clear-cache")

;;; ---------------------------------------------------------------------------
;;; Install
;;; ---------------------------------------------------------------------------

(when (math-usable-p)
  (evo.tui:register-math-renderer #'latex-math-render))
(math-sync-prompt-note)
