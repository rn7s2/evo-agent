;;;; 350-bionic-reader.lisp — bionic reading for evo's agent output.
;;;;
;;;; "Bionic reading" bolds the LEADING letters of each word; the eye fixates
;;;; on the bold stem and the brain fills in the rest, which many readers find
;;;; faster and less tiring.  The TUI core (src/tui/markdown.lisp) exposes a
;;;; prose-styler seam — EVO.TUI:REGISTER-PROSE-STYLER — that hands us each run
;;;; of plain prose the markdown renderer would otherwise print verbatim: the
;;;; words between markers, never inside `code` spans, link URLs, or already-
;;;; bold text (a heading or **strong**).  We return the same text with each
;;;; word's fixation bolded.  Because it rides that one seam, it works on both
;;;; the streaming and the transcript-repaint paths for free, and adds no
;;;; images or width — only zero-width SGR bold toggles the region's cursor
;;;; math already ignores.
;;;;
;;;; ASCII ONLY.  A "word" here is a maximal run of ASCII Latin letters
;;;; ([a-zA-Z]) that is NOT glued to a non-ASCII character on either side.  So
;;;; English (UK + US) gets fixation bolding, while any word carrying an
;;;; accented Latin letter, or CJK / Cyrillic / Greek / etc., is left byte-for-
;;;; byte untouched — bionic reading is an English-typography trick and would
;;;; only mangle scripts it was never designed for.
;;;;
;;;; Settings (override in init.lisp, e.g. (evo:set-setting :bionic-fixation 0.4)):
;;;;   :bionic           t     master on/off
;;;;   :bionic-fixation  0.5   fraction of each word to bold (0<f<=1); at least
;;;;                           one letter, at most the whole word
;;;;
;;;; Command:  /bionic status | on | off | fixation <0..1>

(in-package :evo.user)

;;; ---------------------------------------------------------------------------
;;; Settings
;;; ---------------------------------------------------------------------------

(defun bionic-on-p () (and (evo:setting :bionic t) t))

(defun bionic-fixation ()
  "Fraction of each word to bold, clamped to (0, 1]; 0.5 by default.  A bad or
out-of-range value falls back to the default rather than signalling."
  (let ((v (evo:setting :bionic-fixation 0.5)))
    (if (and (realp v) (plusp v)) (min 1 v) 0.5)))

;;; ---------------------------------------------------------------------------
;;; The transform
;;; ---------------------------------------------------------------------------

;; Zero-width SGR toggles.  Bold-off is 22 (not 0), so a fixation ending inside
;; an *italic* / underlined / coloured run leaves those attributes intact — it
;; turns off bold and nothing else.
(defparameter +bionic-bold-on+ (format nil "~c[1m" #\Escape))
(defparameter +bionic-bold-off+ (format nil "~c[22m" #\Escape))

(defun bionic-ascii-letter-p (c)
  "True for an ASCII Latin letter (a–z, A–Z) — the alphabet bionic bolds."
  (or (char<= #\a c #\z) (char<= #\A c #\Z)))

(defun bionic-fixation-count (n)
  "How many of a length-N word's leading letters to bold: ceil(N·fixation),
never fewer than 1, never more than N."
  (max 1 (min n (ceiling (* n (bionic-fixation))))))

(defun bionic-word (word)
  "WORD (a run of ASCII letters) with its leading fixation wrapped in bold."
  (let ((k (bionic-fixation-count (length word))))
    (concatenate 'string
                 +bionic-bold-on+ (subseq word 0 k)
                 +bionic-bold-off+ (subseq word k))))

(defun bionic-transform (text)
  "Bold the leading fixation of every ASCII-letter word in TEXT.  A maximal run
of ASCII letters is bolded ONLY when it is not adjacent to a non-ASCII
character (code >= 128) on either side — that adjacency means the run is part
of a foreign word (café, naïve, 日本語word, …), which must pass through
untouched.  Spaces, punctuation and digits pass through verbatim."
  (let ((len (length text)))
    (with-output-to-string (out)
      (let ((i 0))
        (loop while (< i len)
              do (let ((c (char text i)))
                   (cond
                     ((bionic-ascii-letter-p c)
                      (let ((j (or (position-if-not #'bionic-ascii-letter-p text
                                                    :start i)
                                   len)))
                        ;; A neighbour just past ASCII (>= 128) means this run is
                        ;; a fragment of a non-English word — leave it alone.
                        (if (and (or (zerop i)
                                     (< (char-code (char text (1- i))) 128))
                                 (or (= j len)
                                     (< (char-code (char text j)) 128)))
                            (write-string (bionic-word (subseq text i j)) out)
                            (write-string (subseq text i j) out))
                        (setf i j)))
                     (t (write-char c out) (incf i)))))))))

;;; ---------------------------------------------------------------------------
;;; Install / status / command
;;; ---------------------------------------------------------------------------

(defun bionic-apply ()
  "Install the prose styler when :bionic is on, remove it otherwise.  Safe to
call repeatedly (idempotent) — from the command, and at load/session-start."
  (evo.tui:register-prose-styler (and (bionic-on-p) #'bionic-transform))
  (bionic-on-p))

(defun bionic-status-text ()
  (format nil "bionic ~a · fixation ~,2f · ascii-only"
          (if (and (bionic-on-p) evo.tui:*prose-styler*) "on" "off")
          (bionic-fixation)))

(defun bionic-command (ctx)
  (let* ((raw (string-trim " " (or (getf ctx :args) "")))
         (arg (string-downcase raw)))
    (cond
      ((or (string= arg "") (string= arg "status")) (bionic-status-text))
      ((string= arg "on")
       (evo:set-setting :bionic t) (bionic-apply) "bionic reading on")
      ((string= arg "off")
       (evo:set-setting :bionic nil) (bionic-apply) "bionic reading off")
      ((and (>= (length arg) 9) (string= "fixation " arg :end2 9))
       (let ((v (ignore-errors
                 (let ((*read-eval* nil)) (read-from-string (subseq raw 9))))))
         (if (and (realp v) (< 0 v) (<= v 1))
             (progn (evo:set-setting :bionic-fixation v) (bionic-apply)
                    (bionic-status-text))
             "usage: /bionic fixation <number in (0, 1]>")))
      (t "usage: /bionic [status | on | off | fixation <0..1>]"))))

(evo:register-command "bionic" #'bionic-command
                      :description "Bionic reading: status | on | off | fixation <0..1>")

;; Rebuild the styler from settings when a session (re)starts — the journal
;; replays this file's load, and the styler lives in memory, not the journal.
(evo:on :session-start (lambda (ev) (declare (ignore ev)) (bionic-apply)))

;; And install now, for the running session that just loaded us.
(bionic-apply)
