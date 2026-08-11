;;;; verify-latex-math.lisp — end-to-end proof of the LaTeX-math feature.
;;;;
;;;; Loads evo + the bundled extension from the on-disk source and asserts the
;;;; whole pipeline: grammar, placement into scrollback, source fallback, and
;;;; REAL rasterization to a valid iTerm2/sixel escape whose payload is a PNG.
;;;; Exits 0 on success, 1 on any failure — the shape the done_when predicate
;;;; (latex-math-render-done-p) checks.

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :evo))

;; Load the renderer extension exactly as boot would.
(let ((*package* (find-package :evo.user)))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "extensions/300-latex-math.lisp" (uiop:getcwd)))))

(defvar *fails* 0)
(defun ok (name test)
  (format t "~&~:[FAIL~;ok  ~] ~a~%" test name)
  (unless test (incf *fails*)))

(defpackage :vlm (:use :cl))
(in-package :vlm)

(defun seg-types (segs) (mapcar #'first segs))

(let ((split #'evo.tui:md-split-math))
  ;; --- grammar -------------------------------------------------------------
  (cl-user::ok "inline $..$ splits"
      (equal (funcall split "a $x^2$ b")
             '((:text "a ") (:math "x^2" nil) (:text " b"))))
  (cl-user::ok "display $$..$$ splits"
      (equal (funcall split "p $$E=mc^2$$ q")
             '((:text "p ") (:math "E=mc^2" t) (:text " q"))))
  (cl-user::ok "\\(..\\) inline"
      (equal (funcall split "see \\(a+b\\) ok")
             '((:text "see ") (:math "a+b" nil) (:text " ok"))))
  (cl-user::ok "\\[..\\] display"
      (equal (funcall split "see \\[a+b\\] ok")
             '((:text "see ") (:math "a+b" t) (:text " ok"))))
  (cl-user::ok "dollar inside code span is prose"
      (equal (seg-types (funcall split "cost `$5` and $y$")) '(:text :math)))
  (cl-user::ok "escaped \\$ is prose"
      (equal (funcall split "price \\$5 only") '((:text "price \\$5 only"))))
  (cl-user::ok "currency $5 and $10 is prose"
      (equal (funcall split "it is $5 and $10 today")
             '((:text "it is $5 and $10 today"))))
  (cl-user::ok "no math -> single text seg"
      (equal (funcall split "just prose") '((:text "just prose")))))

;; --- placement (stub renderer) ---------------------------------------------
(evo:set-setting :math-inline-mode :break)   ; exercise the :break layout
(let ((evo.tui:*math-enabled* t)
      (evo.tui:*math-renderer*
        (lambda (latex disp) (format nil "<IMG:~a:~a>" (if disp "D" "I") latex))))
  (cl-user::ok "placed image ends its line (no cascade)"
      (equal (evo.tui::md-render-line "see $x^2$ end" (evo.tui::make-md))
             (format nil "see <IMG:I:x^2>~% end")))
  ;; multi-line $$ block: interior lines suppressed (NIL), image on close
  (let ((md (evo.tui::make-md)))
    (cl-user::ok "open $$ suppressed" (null (evo.tui::md-render-line "$$" md)))
    (cl-user::ok "content1 suppressed" (null (evo.tui::md-render-line "a+b" md)))
    (cl-user::ok "content2 suppressed" (null (evo.tui::md-render-line "=c" md)))
    (cl-user::ok "close $$ emits block image"
        (equal (evo.tui::md-render-line "$$" md) "<IMG:D:a+b
=c>")))
  ;; multi-line $$ block where the opener SHARES its line with the first content
  ;; and the closer shares its line with the last content — the shape a model
  ;; emits a pmatrix in ($$A = \begin{pmatrix} … \end{pmatrix}$$).  Opener +
  ;; interior suppressed, the whole formula (opener content included) images at
  ;; the close.
  (let ((md (evo.tui::make-md)))
    (cl-user::ok "opener-with-content suppressed"
        (null (evo.tui::md-render-line "$$A = \\begin{pmatrix}" md)))
    (cl-user::ok "matrix row suppressed"
        (null (evo.tui::md-render-line "a & b \\\\" md)))
    (cl-user::ok "closer-with-content emits block image incl. opener content"
        (equal (evo.tui::md-render-line "\\end{pmatrix}$$" md)
               "<IMG:D:A = \\begin{pmatrix}
a & b \\\\
\\end{pmatrix}>")))
  ;; same shape with \[ … \]
  (let ((md (evo.tui::make-md)))
    (cl-user::ok "\\[-with-content opener suppressed"
        (null (evo.tui::md-render-line "\\[x =" md)))
    (cl-user::ok "\\]-with-content closer emits image"
        (equal (evo.tui::md-render-line "y\\]" md) "<IMG:D:x =
y>")))
  ;; CRITICAL: a SELF-CONTAINED single-line $$…$$ is NOT an opener — it must
  ;; image on its own line and leave IN-MATH nil, so the prose that follows is
  ;; NOT swallowed into a math block (the regression that rasterized a whole
  ;; paragraph between two display equations).
  (let ((md (evo.tui::make-md)))
    (cl-user::ok "single-line $$…$$ images inline, does not open a block"
        (equal (evo.tui::md-render-line "$$f(q) = 0$$" md) "<IMG:D:f(q) = 0>"))
    (cl-user::ok "single-line $$…$$ leaves in-math nil"
        (null (evo.tui::md-in-math md)))
    (cl-user::ok "prose after single-line $$…$$ renders as prose (not swallowed)"
        ;; layout-agnostic (inline mode is :break here): the point is the prose
        ;; survives with INLINE math, and is NOT rasterized as a DISPLAY block.
        (let ((r (evo.tui::md-render-line "Each $q$ has a dimension." md)))
          (and (search "<IMG:I:q>" r)
               (search "has a dimension." r)
               (not (search "<IMG:D:" r)))))
    (cl-user::ok "second single-line $$…$$ images on its own"
        (equal (evo.tui::md-render-line "$$[q] = D^a$$" md) "<IMG:D:[q] = D^a>")))
  ;; likewise a self-contained single-line \[…\] is not an opener
  (let ((md (evo.tui::make-md)))
    (cl-user::ok "single-line \\[…\\] images inline, does not open a block"
        (equal (evo.tui::md-render-line "\\[x = y\\]" md) "<IMG:D:x = y>"))
    (cl-user::ok "single-line \\[…\\] leaves in-math nil"
        (null (evo.tui::md-in-math md))))
  ;; preview renders math as SOURCE, never the stub image
  (cl-user::ok "preview shows source not image"
      (equal (evo.tui::md-render-preview "see $x^2$ end" (evo.tui::make-md))
             "see $x^2$ end"))
  ;; PREVIEW of an unclosed multi-line block: EVERY line — the opener included —
  ;; must show its raw source, never vanish and never image.  The real md
  ;; advances line-by-line (scrollback) while we preview the same line.
  (let ((real (evo.tui::make-md)))
    (cl-user::ok "preview opener-with-content shows source"
        (equal (evo.tui::md-render-preview "$$A = \\begin{pmatrix}" real)
               "$$A = \\begin{pmatrix}"))
    (evo.tui::md-render-line "$$A = \\begin{pmatrix}" real)
    (cl-user::ok "preview interior shows source"
        (equal (evo.tui::md-render-preview "a & b \\\\" real) "a & b \\\\"))
    (evo.tui::md-render-line "a & b \\\\" real)
    (cl-user::ok "preview closer-with-content shows source"
        (equal (evo.tui::md-render-preview "\\end{pmatrix}$$" real)
               "\\end{pmatrix}$$")))
  ;; preview must not toggle the real fence/math state (COPY-MD shield)
  (let ((real (evo.tui::make-md)))
    (evo.tui::md-render-preview "$$A = \\begin{pmatrix}" real)
    (cl-user::ok "preview leaves real in-math untouched"
        (null (evo.tui::md-in-math real)))))

;; --- disabled: byte-for-byte passthrough -----------------------------------
(let ((evo.tui:*math-enabled* nil))
  (cl-user::ok "disabled leaves $x$ untouched"
      (equal (evo.tui::md-render-line "a $x$ b" (evo.tui::make-md)) "a $x$ b")))

;; --- source fallback when renderer returns NIL -----------------------------
(let ((evo.tui:*math-enabled* t)
      (evo.tui:*math-renderer* (lambda (l d) (declare (ignore l d)) nil)))
  (cl-user::ok "nil renderer -> source"
      (equal (evo.tui::md-render-line "x $a+b$ y" (evo.tui::make-md))
             "x $a+b$ y")))

;; --- REAL rasterization ----------------------------------------------------
(flet ((kitty-png (esc)
         ;; Concatenate the base64 payload from every APC packet (the text
         ;; between each ';' and its ST = ESC \\), then decode to octets.
         (let ((st (format nil "~C\\" #\Escape)))
           (evo.util:base64->octets
            (with-output-to-string (o)
              (let ((i 0))
                (loop for semi = (position #\; esc :start i)
                      while semi
                      for end = (search st esc :start2 semi)
                      while end
                      do (write-string (subseq esc (1+ semi) end) o)
                         (setf i (+ end (length st)))))))))
       (png-p (octets)
         (and (>= (length octets) 8)
              (equalp (subseq octets 0 8)
                      #(137 80 78 71 13 10 26 10)))))
  (if (not (evo.user::latex-toolchain-ready-p))
      (progn (format t "~&FAIL latex toolchain not found (need latex + dvipng)~%")
             (incf cl-user::*fails*))
      (progn
        (evo:set-setting :math t)
        (let ((esc (evo.user::latex-math-render "E=mc^2" nil)))
          (cl-user::ok "kitty escape produced" (and (stringp esc) esc))
          (cl-user::ok "kitty APC header (f=100,a=T)"
              (and (stringp esc)
                   (search (format nil "~C_Gf=100,a=T" #\Escape) esc)))
          (let ((bytes (and (stringp esc) (kitty-png esc))))
            (cl-user::ok "kitty payload decodes to a PNG" (and bytes (png-p bytes)))))
        (let ((esc (evo.user::latex-math-render "\\int_0^1 x^2\\,dx" t)))
          (let ((bytes (and (stringp esc) (kitty-png esc))))
            (cl-user::ok "display formula rasterizes to PNG"
                (and bytes (png-p bytes)))))
        ;; CJK / Unicode: the classic 8-bit latex engine cannot set 素, so this
        ;; formula used to fall all the way back to raw source (the bug).  The
        ;; XeLaTeX + xeCJK fallback must turn it into a real image instead.
        ;; Skipped only where that toolchain is absent (e.g. CI without xelatex).
        (if (not (evo.user::latex-fallback-ready-p))
            (format t "~&skip CJK fallback (no xelatex + pdf rasterizer)~%")
            (let* ((esc (evo.user::latex-math-render
                         "\\prod_{p\\ \\text{素}}\\frac{1}{1-p^{-s}}" t))
                   (bytes (and (stringp esc) (kitty-png esc))))
              (cl-user::ok "CJK formula rasterizes to a PNG (xelatex fallback)"
                  (and bytes (png-p bytes)))))
        ;; cache: a second call returns the memoized escape (identical string)
        (cl-user::ok "escape memoized"
            (eq (evo.user::latex-math-render "E=mc^2" nil)
                (evo.user::latex-math-render "E=mc^2" nil)))
        ;; metrics: total rows, ascent (rows above the baseline), width in cells
        (multiple-value-bind (esc total ascent cols)
            (evo.user::latex-math-render "x_i" nil)
          (cl-user::ok "renderer reports total rows"
              (and esc (integerp total) (plusp total)))
          (cl-user::ok "renderer reports ascent within [0,total]"
              (and (integerp ascent) (<= 0 ascent total)))
          (cl-user::ok "renderer reports positive cols"
              (and (integerp cols) (plusp cols))))
        ;; inline (:terminal x-advance): NO C=1 — the terminal steps the cursor
        ;; past the image itself, exactly; the renderer reports :self so the
        ;; core only corrects vertically.  display: C=1 pin, core reserves rows.
        (multiple-value-bind (esc total ascent cols advance)
            (evo.user::latex-math-render "E=mc^2" nil)
          (declare (ignore total ascent cols))
          (cl-user::ok "inline image lets the terminal advance (no C=1)"
              (and esc (not (search ",C=1" esc))))
          (cl-user::ok "inline image reports :self advance"
              (eq advance :self)))
        (multiple-value-bind (esc total ascent cols advance)
            (evo.user::latex-math-render "\\sum x" t)
          (declare (ignore total ascent cols))
          (cl-user::ok "display image pins the cursor (C=1)"
              (and esc (search ",C=1" esc)))
          (cl-user::ok "display image reports no self advance"
              (null advance)))
        ;; while rendering is usable, the agent is asked to WRITE LaTeX
        (cl-user::ok "prompt note registered while math is usable"
            (let ((note (cdr (assoc "latex-math" evo.kernel::*prompt-notes*
                                    :test #'equal))))
              (and note (search "$$" note)))))))

(format t "~&~%verify-latex-math: ~[all checks passed~:;~:*~d FAILED~]~%"
        cl-user::*fails*)
(uiop:quit (if (zerop cl-user::*fails*) 0 1))
