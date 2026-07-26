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
  (in-code nil))

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
[text](url) links.  Unmatched markers stay literal."
  (let ((len (length text))
        (bold-close nil)     ; index of the pending ** closer
        (emph-close nil))    ; index of the pending * / _ closer
    (with-output-to-string (out)
      (loop with i = 0
            while (< i len)
            do (let ((c (char text i)))
                 (cond
                   ;; pending closers first: ** before * at the same index
                   ((and bold-close (= i bold-close))
                    (write-string (sgr 22) out)
                    (setf bold-close nil)
                    (incf i 2))
                   ((and emph-close (= i emph-close))
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
                         (write-string (sgr 33) out)
                         (write-string (subseq text run close) out)
                         (write-string (sgr 39) out)
                         (setf i (+ close ticks)))
                        (t (write-string (subseq text i run) out)
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
                      (cond (j (write-string (sgr 1) out)
                               (setf bold-close j)
                               (incf i 2))
                            (t (write-char c out) (incf i)))))
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
                      (cond (j (write-string (sgr 3) out)
                               (setf emph-close j)
                               (incf i))
                            (t (write-char c out) (incf i)))))
                   ;; [text](url)
                   ((char= c #\[)
                    (let* ((mid (search "](" text :start2 (1+ i)))
                           (end (and mid (position #\) text :start (+ mid 2)))))
                      (cond
                        (end
                         (let ((label (subseq text (1+ i) mid))
                               (url (subseq text (+ mid 2) end)))
                           (write-string (sgr 4) out)
                           (write-string label out)
                           (write-string (sgr 24) out)
                           (unless (equal label url)
                             (write-string (dim (format nil " (~a)" url)) out))
                           (setf i (1+ end))))
                        (t (write-char c out) (incf i)))))
                   (t (write-char c out) (incf i)))))
      ;; a closer swallowed by a code span leaves a style open: reset
      (when (or bold-close emph-close)
        (write-string (sgr 0) out)))))

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
                          (bold (md-inline (subseq rest hashes))))
             (md-inline line))))
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
       (concatenate 'string pad (cyan "•") (md-inline (subseq rest 1))))
      ;; 1. ordered list
      ((md-ordered-end rest)
       (let ((end (md-ordered-end rest)))
         (concatenate 'string pad (cyan (subseq rest 0 end))
                      (md-inline (subseq rest end)))))
      (t (concatenate 'string pad (md-inline rest))))))

;;; Entry points.

(defun md-render-line (line md)
  "Render one complete markdown LINE for scrollback, advancing the fence
state in MD."
  (cond
    ((md-fence-p line)
     (setf (md-in-code md) (not (md-in-code md)))
     (dim line))
    ((md-in-code md) line)
    (t (md-block-line line))))

(defun md-render-preview (line md)
  "Render a still-streaming LINE without advancing the fence state."
  (md-render-line line (copy-md md)))

(defun md-render-text (text)
  "Render complete multi-line markdown TEXT with a fresh fence state."
  (let ((md (make-md)))
    (string-join (string #\Newline)
                 (mapcar (lambda (line) (md-render-line line md))
                         (uiop:split-string text :separator '(#\Newline))))))
