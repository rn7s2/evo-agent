;;;; render.lisp — scrollback + managed bottom region (§14).
;;;;
;;;; Content renders into normal terminal scrollback (history is part of the
;;;; UX — no alternate screen); the bottom region (streaming tail, todo
;;;; panel, status line, editor) is repainted in place with relative cursor
;;;; movement only: park at the end of the region, move up region-height-1 to
;;;; get back to its start, clear below, redraw.  Region lines are truncated
;;;; to the terminal width (wide content truncates with indicators); the
;;;; editor soft-wraps, so every painted line is exactly one terminal row and
;;;; the movement math stays exact.  Raw mode disables OPOST: every newline
;;;; we emit must be \r\n.

(in-package :evo.tui)

(defvar *tui-lock* (bt:make-lock "tui"))
(defvar *region-height* 0
  "Terminal rows the managed region currently occupies (0 = not drawn yet).")

(defun crlf (text)
  "LF -> CRLF for raw-mode output."
  (with-output-to-string (out)
    (loop for c across text
          do (if (char= c #\Newline)
                 (write-string (format nil "~c~c" #\Return #\Linefeed) out)
                 (write-char c out)))))

(defun goto-region-start ()
  (wr (string #\Return))
  (when (> *region-height* 1)
    (wr (cursor-up (1- *region-height*))))
  (wr (clear-below)))

(defun draw-region (lines cursor-row cursor-col)
  "Paint LINES as the managed region; leave the terminal cursor at
CURSOR-ROW/CURSOR-COL within it (row 0 = first region line)."
  (let ((lines (or lines (list ""))))
    (wr (hide-cursor))
    (goto-region-start)
    (loop for line in lines
          for first = t then nil
          unless first do (wr (format nil "~c~c" #\Return #\Linefeed))
          do (wr (truncate-visible line (1- *cols*))))
    ;; Cursor: currently at end of last line; navigate to target cell.
    (let ((up (- (length lines) 1 cursor-row)))
      (wr (string #\Return))
      (when (plusp up) (wr (cursor-up up)))
      (when (plusp cursor-col) (wr (cursor-right cursor-col))))
    (wr (show-cursor))
    (flush)
    (setf *region-height* (length lines))))

(defun emit-scrollback (text)
  "Write TEXT (may be multi-line, LF endings) into scrollback above the
region.  The caller must redraw the region afterwards (the region is
cleared here so scrolling can happen naturally)."
  (wr (hide-cursor))
  (goto-region-start)
  (wr (crlf text))
  (unless (and (plusp (length text))
               (char= (char text (1- (length text))) #\Newline))
    (wr (format nil "~c~c" #\Return #\Linefeed)))
  (setf *region-height* 0))
