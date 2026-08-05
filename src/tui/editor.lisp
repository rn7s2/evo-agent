;;;; editor.lisp — the plain multi-line editor.
;;;;
;;;; Enter sends, Shift+Enter (or Alt+Enter fallback) inserts a newline.
;;;; Pastes of more than 3 lines collapse to a placeholder token; the content
;;;; lives in a side buffer and is substituted back in full on send.
;;;; Pasting the exact same content again with the cursor right after the
;;;; placeholder expands it inline (paste once to keep it compact, paste
;;;; twice to edit).
;;;;
;;;; Attached images use the same trick from the other direction: the image
;;;; lives beside the text and a "[Image #n]" token stands in for it, so an
;;;; attachment is reviewed, moved, and deleted with the ordinary editing
;;;; keys — and only the ones whose token survives to Enter are sent.

(in-package :evo.tui)

(defstruct (edit-buffer (:conc-name eb-))
  (lines (list ""))     ; logical lines
  (line 0)              ; cursor logical line index
  (col 0)               ; cursor column (character index)
  (pastes nil)          ; alist id -> content
  (paste-counter 0)
  (attachments nil)     ; alist id -> :image content block
  (attach-counter 0))

(defun eb-current-line (eb) (nth (eb-line eb) (eb-lines eb)))

(defun (setf eb-current-line) (value eb)
  (setf (nth (eb-line eb) (eb-lines eb)) value))

(defun eb-text (eb)
  (string-join (string #\Newline) (eb-lines eb)))

(defun eb-empty-p (eb)
  (and (= 1 (length (eb-lines eb)))
       (zerop (length (first (eb-lines eb))))))

(defun eb-clear (eb)
  (setf (eb-lines eb) (list "") (eb-line eb) 0 (eb-col eb) 0
        (eb-pastes eb) nil (eb-attachments eb) nil))

(defun eb-set-text (eb text)
  (setf (eb-lines eb)
        (or (uiop:split-string text :separator '(#\Newline)) (list "")))
  (setf (eb-line eb) (1- (length (eb-lines eb)))
        (eb-col eb) (length (car (last (eb-lines eb))))))

(defun eb-insert-char (eb char)
  (let ((line (eb-current-line eb))
        (col (eb-col eb)))
    (setf (eb-current-line eb)
          (concatenate 'string (subseq line 0 col) (string char) (subseq line col)))
    (incf (eb-col eb))))

(defun eb-newline (eb)
  (let* ((line (eb-current-line eb))
         (col (eb-col eb))
         (head (subseq line 0 col))
         (tail (subseq line col)))
    (setf (eb-current-line eb) head)
    (setf (eb-lines eb)
          (append (subseq (eb-lines eb) 0 (1+ (eb-line eb)))
                  (list tail)
                  (subseq (eb-lines eb) (1+ (eb-line eb)))))
    (incf (eb-line eb))
    (setf (eb-col eb) 0)))

(defun eb-insert-text (eb text)
  "Insert TEXT literally (splitting on newlines) at the cursor."
  (loop for char across text
        do (if (char= char #\Newline)
               (eb-newline eb)
               (unless (char= char #\Return)
                 (eb-insert-char eb char)))))

(defun eb-backspace (eb)
  (cond ((plusp (eb-col eb))
         (let ((line (eb-current-line eb)))
           (setf (eb-current-line eb)
                 (concatenate 'string (subseq line 0 (1- (eb-col eb)))
                              (subseq line (eb-col eb))))
           (decf (eb-col eb))))
        ((plusp (eb-line eb))
         (let* ((prev (nth (1- (eb-line eb)) (eb-lines eb)))
                (cur (eb-current-line eb)))
           (setf (eb-lines eb)
                 (append (subseq (eb-lines eb) 0 (1- (eb-line eb)))
                         (list (concatenate 'string prev cur))
                         (subseq (eb-lines eb) (1+ (eb-line eb)))))
           (decf (eb-line eb))
           (setf (eb-col eb) (length prev))))))

(defun eb-delete (eb)
  (let ((line (eb-current-line eb)))
    (cond ((< (eb-col eb) (length line))
           (setf (eb-current-line eb)
                 (concatenate 'string (subseq line 0 (eb-col eb))
                              (subseq line (1+ (eb-col eb))))))
          ((< (eb-line eb) (1- (length (eb-lines eb))))
           (let ((next (nth (1+ (eb-line eb)) (eb-lines eb))))
             (setf (eb-lines eb)
                   (append (subseq (eb-lines eb) 0 (eb-line eb))
                           (list (concatenate 'string line next))
                           (subseq (eb-lines eb) (+ 2 (eb-line eb))))))))))

(defun word-char-p (c) (or (alphanumericp c) (char= c #\_) (char= c #\-)))

(defun eb-move (eb direction)
  (case direction
    (:left (cond ((plusp (eb-col eb)) (decf (eb-col eb)))
                 ((plusp (eb-line eb))
                  (decf (eb-line eb))
                  (setf (eb-col eb) (length (eb-current-line eb))))))
    (:right (cond ((< (eb-col eb) (length (eb-current-line eb)))
                   (incf (eb-col eb)))
                  ((< (eb-line eb) (1- (length (eb-lines eb))))
                   (incf (eb-line eb))
                   (setf (eb-col eb) 0))))
    (:up (when (plusp (eb-line eb))
           (decf (eb-line eb))
           (setf (eb-col eb) (min (eb-col eb) (length (eb-current-line eb))))))
    (:down (when (< (eb-line eb) (1- (length (eb-lines eb))))
             (incf (eb-line eb))
             (setf (eb-col eb) (min (eb-col eb) (length (eb-current-line eb))))))
    (:home (setf (eb-col eb) 0))
    (:end (setf (eb-col eb) (length (eb-current-line eb))))
    (:word-left
     (let ((line (eb-current-line eb)))
       (if (zerop (eb-col eb))
           (eb-move eb :left)
           (let ((i (eb-col eb)))
             (loop while (and (plusp i) (not (word-char-p (char line (1- i))))) do (decf i))
             (loop while (and (plusp i) (word-char-p (char line (1- i)))) do (decf i))
             (setf (eb-col eb) i)))))
    (:word-right
     (let ((line (eb-current-line eb)))
       (if (= (eb-col eb) (length line))
           (eb-move eb :right)
           (let ((i (eb-col eb)))
             (loop while (and (< i (length line)) (not (word-char-p (char line i)))) do (incf i))
             (loop while (and (< i (length line)) (word-char-p (char line i))) do (incf i))
             (setf (eb-col eb) i)))))))

(defun eb-delete-word (eb)
  (let ((end (eb-col eb)))
    (eb-move eb :word-left)
    (let ((line (eb-current-line eb)))
      (setf (eb-current-line eb)
            (concatenate 'string (subseq line 0 (eb-col eb)) (subseq line end))))))

(defun eb-kill-line (eb)
  (let ((line (eb-current-line eb)))
    (if (< (eb-col eb) (length line))
        (setf (eb-current-line eb) (subseq line 0 (eb-col eb)))
        (eb-delete eb))))

;;; Paste placeholders.

(defun paste-placeholder (id line-count)
  (format nil "[paste #~d: ~d lines]" id line-count))

(defun count-lines (text)
  (1+ (count #\Newline text)))

(defun eb-placeholder-before-cursor (eb)
  "If the text immediately before the cursor is a placeholder token, return
(values id token), else nil."
  (let ((line (subseq (eb-current-line eb) 0 (eb-col eb))))
    (loop for (id . content) in (eb-pastes eb)
          for token = (paste-placeholder id (count-lines content))
          when (and (>= (length line) (length token))
                    (string= token line :start2 (- (length line) (length token))))
            do (return (values id token)))))

(defun eb-paste (eb text)
  "Bracketed paste arrived.  >3 lines collapses to a placeholder;
re-pasting identical content right after its placeholder expands it inline."
  (let ((text (remove #\Return text)))
    (multiple-value-bind (id token) (eb-placeholder-before-cursor eb)
      (cond
        ;; paste-to-expand
        ((and id (equal (cdr (assoc id (eb-pastes eb))) text))
         (let ((line (eb-current-line eb)))
           (setf (eb-current-line eb)
                 (concatenate 'string
                              (subseq line 0 (- (eb-col eb) (length token)))
                              (subseq line (eb-col eb))))
           (decf (eb-col eb) (length token)))
         (setf (eb-pastes eb) (remove id (eb-pastes eb) :key #'car))
         (eb-insert-text eb text))
        ;; collapse
        ((> (count-lines text) 3)
         (let ((id (incf (eb-paste-counter eb))))
           (push (cons id text) (eb-pastes eb))
           (eb-insert-text eb (paste-placeholder id (count-lines text)))))
        (t (eb-insert-text eb text))))))

(defun eb-submit-text (eb)
  "Text to send: buffer content with placeholders substituted back in full."
  (let ((text (eb-text eb)))
    (loop for (id . content) in (eb-pastes eb)
          for token = (paste-placeholder id (count-lines content))
          do (setf text (string-replace token content text :all t)))
    text))

;;; Image attachments.
;;;
;;; An attached image lives beside the text as a numbered token, "[Image #1]".
;;; The token is the whole user interface: it shows the image is there, it
;;; marks where in the message it belongs, and it makes the attachment
;;; editable with the keys already in the user's fingers — backspace over the
;;; token and the image does not get sent, because EB-SUBMIT-IMAGES only
;;; yields attachments whose token survived to submit time.  The token goes to
;;; the model too, so "the second image" in a follow-up sentence resolves.

(defun image-placeholder (id)
  (format nil "[Image #~d]" id))

(defun eb-attach-image (eb block)
  "Attach an :image content block at the cursor.  Returns its token id."
  (let ((id (incf (eb-attach-counter eb))))
    (setf (eb-attachments eb) (append (eb-attachments eb) (list (cons id block))))
    (unless (or (zerop (eb-col eb))
                (char= (char (eb-current-line eb) (1- (eb-col eb))) #\Space))
      (eb-insert-char eb #\Space))
    (eb-insert-text eb (image-placeholder id))
    (eb-insert-char eb #\Space)
    id))

(defun eb-submit-images (eb)
  "Attached image blocks whose token is still in the buffer, ordered as they
appear in the text."
  (mapcar #'cdr
          (sort (loop with text = (eb-text eb)
                      for (id . block) in (eb-attachments eb)
                      for position = (search (image-placeholder id) text)
                      when position collect (cons position block))
                #'< :key #'car)))

;;; Display: soft-wrap logical lines into WIDTH-column rows.

(defun eb-display-rows (eb width)
  "Returns (values rows cursor-row cursor-col); every row is at most WIDTH
display columns.  Wrapping and the cursor position are measured in display
columns (wide characters count 2), so painted rows never exceed the
terminal width and ANSI cursor movement lands on the right cell."
  (let ((rows nil) (cursor-row 0) (cursor-col 0))
    (loop
      for line in (eb-lines eb)
      for line-index from 0
      for cursor-here = (= line-index (eb-line eb))
      do (let ((base (length rows)) (chunks nil) (start 0) (col 0))
           (loop for i from 0 below (length line)
                 for w = (max 1 (char-display-width (char line i)))
                 do (when (> (+ col w) width)   ; wrap before this char
                      (push (subseq line start i) chunks)
                      (setf start i col 0))
                    (when (and cursor-here (= i (eb-col eb)))
                      (setf cursor-row (+ base (length chunks)) cursor-col col))
                    (incf col w))
           (push (subseq line start) chunks)
           (when (and cursor-here (= (eb-col eb) (length line)))
             (when (and (plusp col) (>= col width))
               (push "" chunks)                 ; cursor opens a fresh wrap row
               (setf col 0))
             (setf cursor-row (+ base (1- (length chunks))) cursor-col col))
           (setf rows (append rows (nreverse chunks)))))
    (values rows cursor-row cursor-col)))
