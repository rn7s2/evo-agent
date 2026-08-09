;;;; verify-bionic.lisp — end-to-end proof of the bionic-reader feature.
;;;;
;;;; Loads evo + the bundled extension from the on-disk source and asserts the
;;;; whole pipeline: the core prose-styler seam (off by default, excludes code
;;;; spans / link URLs / bold / headings, falls back on error), and the bionic
;;;; reader itself (English words get their leading letters bolded; any word
;;;; carrying a non-ASCII character is left byte-for-byte untouched), plus the
;;;; /bionic command.  Exits 0 on success, 1 on any failure — the shape the
;;;; done_when predicate (bionic-reader-done-p) checks.

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system :evo))

;; Load the reader extension exactly as boot would.
(let ((*package* (find-package :evo.user)))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "extensions/350-bionic-reader.lisp" (uiop:getcwd)))))

(defvar *fails* 0)
(defun ok (name test)
  (format t "~&~:[FAIL~;ok  ~] ~a~%" test name)
  (unless test (incf *fails*)))

(let ((bon (format nil "~c[1m" #\Escape))
      (boff (format nil "~c[22m" #\Escape))
      (bionic (uiop:find-symbol* :bionic-transform :evo.user))
      (cmd (uiop:find-symbol* :bionic-command :evo.user)))

  ;; --- the core seam -------------------------------------------------------
  (ok "register-prose-styler is exported and callable"
      (fboundp 'evo.tui:register-prose-styler))
  (ok "*prose-styler* is an exported special"
      (boundp 'evo.tui:*prose-styler*))

  ;; --- ASCII-only transform ------------------------------------------------
  (ok "an ascii word's leading fixation is bolded"
      (equal (funcall bionic "hello") (concatenate 'string bon "hel" boff "lo")))
  (ok "a one-letter word is fully bolded"
      (equal (funcall bionic "a") (concatenate 'string bon "a" boff)))
  (ok "punctuation and spacing are preserved"
      (equal (funcall bionic "hi, ok")
             (concatenate 'string bon "h" boff "i, " bon "o" boff "k")))
  (let ((accented (format nil "caf~c" (code-char 233))))          ; café
    (ok "an accented (non-ascii) word is left untouched"
        (equal (funcall bionic accented) accented)))
  (let ((cjk (coerce (list (code-char #x65E5) (code-char #x672C)
                           (code-char #x8A9E)) 'string)))         ; 日本語
    (ok "a CJK word is left untouched"
        (equal (funcall bionic cjk) cjk)))
  (let ((glued (coerce (list (code-char #x65E5) #\w #\o #\r #\d) 'string))) ; 日word
    (ok "ascii letters glued to a non-ascii char are left untouched"
        (equal (funcall bionic glued) glued)))
  (ok "an english word beside foreign text is still bolded"
      (search (concatenate 'string bon "wo" boff "rd")
              (funcall bionic
                       (coerce (list (code-char #x65E5) #\Space #\w #\o #\r #\d)
                               'string))))                        ; 日 word

  ;; --- seam integration: what must NOT reach the styler --------------------
  (evo.tui:register-prose-styler (lambda (s) (concatenate 'string "<" s ">")))
  (ok "plain prose reaches the styler"
      (search "<hi >" (evo.tui::md-inline "hi **b**")))
  (ok "bold text does NOT reach the styler"
      (not (search "<b>" (evo.tui::md-inline "hi **b**"))))
  (ok "code span content does NOT reach the styler"
      (not (find #\< (evo.tui::md-inline "`x`"))))
  (ok "link URL does NOT reach the styler"
      (not (search "<u>" (evo.tui::md-inline "[t](u)"))))
  (ok "a heading suppresses the styler (already bold)"
      (not (find #\< (evo.tui::md-render-line "## Title" (evo.tui::make-md)))))
  (let ((evo.tui:*prose-styler* (lambda (s) (declare (ignore s)) (error "boom"))))
    (ok "a signalling styler falls back to the source run"
        (equal "hello" (evo.tui::md-inline "hello"))))

  ;; --- off by default: byte-for-byte identity ------------------------------
  (evo.tui:register-prose-styler nil)
  (ok "with no styler installed the renderer is unchanged"
      (equal "hello there" (evo.tui::md-inline "hello there")))

  ;; --- the /bionic command drives the seam and the settings ----------------
  (funcall cmd '(:args "off"))
  (ok "/bionic off removes the styler" (null evo.tui:*prose-styler*))
  (funcall cmd '(:args "on"))
  (ok "/bionic on installs the styler" (and evo.tui:*prose-styler* t))
  (ok "/bionic status reports on" (search "bionic on" (funcall cmd '(:args "status"))))
  (funcall cmd '(:args "fixation 0.75"))
  (ok "/bionic fixation sets the fraction"
      (< (abs (- 0.75 (evo:setting :bionic-fixation 0))) 1d-4))

  ;; --- end to end through the renderer -------------------------------------
  (funcall cmd '(:args "fixation 0.5"))
  (ok "rendered prose is bionic-bolded once the reader is on"
      (search bon (evo.tui::md-render-line "plain words" (evo.tui::make-md)))))

(format t "~&~%verify-bionic: ~[all checks passed~:;~:*~d FAILED~]~%" *fails*)
(uiop:quit (if (zerop *fails*) 0 1))
