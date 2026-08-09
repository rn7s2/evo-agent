;;;; bionic-done.lisp — the goal's done_when predicate.
;;;;
;;;; Loaded into the live runtime with load_extension; attached via
;;;; update_goal done_when="bionic-reader-done-p".  Returns T iff the bionic
;;;; reader feature is really in place — proven from the on-disk source, not
;;;; from this session's memory:
;;;;   1. the core prose-styler seam exists in src/ (exported + wired into the
;;;;      inline markdown renderer),
;;;;   2. the bundled extension file exists,
;;;;   3. tests/verify-bionic.lisp — a fresh-Lisp end-to-end proof — exits 0.

(in-package :evo.user)

(defun %bionic-lisp ()
  "Path to an SBCL to run the proof with; NIL if none is found."
  (or (loop for c in '("sbcl" "/opt/homebrew/bin/sbcl" "/usr/local/bin/sbcl"
                       "/usr/bin/sbcl")
            when (ignore-errors
                  (zerop (nth-value 2
                          (uiop:run-program (list c "--version")
                                            :ignore-error-status t))))
              return c)))

(defun %bionic-file-has (rel &rest needles)
  "True iff every NEEDLE appears in the project file REL."
  (let ((path (merge-pathnames rel (uiop:getcwd))))
    (and (probe-file path)
         (let ((text (uiop:read-file-string path)))
           (every (lambda (n) (search n text)) needles)))))

(defun bionic-reader-done-p ()
  (let ((root (uiop:getcwd)))
    (and
     ;; 1. core seam wired in and exported
     (%bionic-file-has "src/tui/markdown.lisp"
                       "*prose-styler*" "register-prose-styler" "style-prose"
                       "*prose-styling-suppressed*")
     (%bionic-file-has "src/packages.lisp" "register-prose-styler")
     ;; 2. the bundled extension
     (probe-file (merge-pathnames "extensions/350-bionic-reader.lisp" root))
     (%bionic-file-has "extensions/350-bionic-reader.lisp"
                       "register-prose-styler" "bionic-transform")
     ;; 3. end-to-end proof passes in a fresh Lisp
     (let ((lisp (%bionic-lisp))
           (script (merge-pathnames "tests/verify-bionic.lisp" root)))
       (and lisp (probe-file script)
            (zerop (nth-value 2
                    (uiop:run-program
                     (list lisp "--non-interactive" "--load"
                           (namestring script))
                     :output *standard-output*
                     :error-output *standard-output*
                     :ignore-error-status t))))))))
