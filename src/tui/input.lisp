;;;; input.lisp — incremental byte-stream -> key-event parser.
;;;;
;;;; Fed by the tui poll loop; unconsumed bytes (partial escape sequences,
;;;; unterminated bracketed pastes, split UTF-8) stay in the buffer until the
;;;; next tick.  A lone ESC is only emitted after the caller reports quiet
;;;; ticks (see PARSE-KEYS :flush-escape).
;;;;
;;;; Events: (:char c) (:paste string) :enter :shift-enter :newline
;;;; :backspace :delete :up :down :left :right :home :end :word-left
;;;; :word-right :escape (:ctrl char) :delete-word

(in-package :evo.tui)

(defstruct (input-state (:conc-name in-))
  (buffer (make-array 0 :element-type '(unsigned-byte 8)
                        :adjustable t :fill-pointer t)))

(defun in-push-bytes (state bytes)
  (loop for b across bytes do (vector-push-extend b (in-buffer state))))

(defun utf8-length (lead)
  (cond ((< lead #x80) 1)
        ((= (logand lead #xE0) #xC0) 2)
        ((= (logand lead #xF0) #xE0) 3)
        ((= (logand lead #xF8) #xF0) 4)
        (t 1)))

(defun decode-utf8 (bytes)
  (handler-case (sb-ext:octets-to-string (coerce bytes '(vector (unsigned-byte 8)))
                                         :external-format :utf-8)
    (error () (string #\Replacement_Character))))

(defparameter *paste-end* #(27 91 50 48 49 126)) ; ESC [ 2 0 1 ~

(defun find-subseq (needle haystack start)
  (search needle haystack :start2 start))

(defun csi-key (params final)
  "Map a CSI sequence (PARAMS: list of integers, FINAL: char) to an event."
  (case final
    (#\A :up) (#\B :down) (#\C (if (member 5 (cdr params)) :word-right
                                   (if (member 3 (cdr params)) :word-right :right)))
    (#\D (if (member 5 (cdr params)) :word-left
             (if (member 3 (cdr params)) :word-left :left)))
    (#\H :home) (#\F :end)
    (#\u ;; kitty / CSI-u: code;modifiers u
     (let ((code (or (first params) 0))
           (mod (or (second params) 1)))
       (case code
         (13 (case mod (2 :shift-enter) ((3 4) :newline) (t :enter)))
         (27 :escape)
         (9 (list :char #\Tab))
         (t (when (<= 32 code 126)
              (if (member mod '(1 2))
                  (list :char (code-char code))
                  nil))))))
    (#\~ (let ((n (first params)))
           (case n
             ((1 7) :home) ((4 8) :end) (3 :delete)
             (27 ;; modifyOtherKeys: 27;mod;code~
              (let ((mod (or (second params) 1))
                    (code (or (third params) 0)))
                (when (= code 13)
                  (case mod (2 :shift-enter) ((3 4) :newline) (t :enter)))))
             (t nil))))
    (t nil)))

(defun parse-keys (state &key flush-escape)
  "Parse buffered bytes into events.  FLUSH-ESCAPE: treat a trailing lone ESC
as the Escape key (caller saw quiet ticks).  Returns the list of events."
  (let ((buf (in-buffer state))
        (events nil)
        (i 0))
    (labels ((emit (e) (when e (push e events)))
             (bytes-left () (- (fill-pointer buf) i))
             (at (k) (and (< (+ i k) (fill-pointer buf)) (aref buf (+ i k)))))
      (loop
        (when (zerop (bytes-left)) (return))
        (let ((b (aref buf i)))
          (cond
            ;; --- escape sequences ---
            ((= b 27)
             (let ((b2 (at 1)))
               (cond
                 ((null b2)
                  (if flush-escape (progn (emit :escape) (incf i)) (return)))
                 ((= b2 91)             ; CSI
                  ;; Bracketed paste start?
                  (if (and (eql (at 2) 50) (eql (at 3) 48) (eql (at 4) 48) (eql (at 5) 126))
                      (let ((end (find-subseq *paste-end* buf (+ i 6))))
                        (if end
                            (progn
                              (emit (list :paste (decode-utf8 (subseq buf (+ i 6) end))))
                              (setf i (+ end (length *paste-end*))))
                            (return)))  ; wait for the rest of the paste
                      ;; Generic CSI: params then final byte in @..~
                      (let ((j (+ i 2)))
                        (loop while (and (< j (fill-pointer buf))
                                         (not (<= 64 (aref buf j) 126)))
                              do (incf j))
                        (if (>= j (fill-pointer buf))
                            (if (> (- j i) 32)
                                (setf i j) ; junk guard: discard runaway CSI
                                (return))  ; incomplete, wait
                            (let* ((param-str (decode-utf8 (subseq buf (+ i 2) j)))
                                   (params (mapcar (lambda (p) (parse-integer p :junk-allowed t))
                                                   (uiop:split-string param-str :separator '(#\;))))
                                   (final (code-char (aref buf j))))
                              (emit (csi-key (remove nil params) final))
                              (setf i (1+ j)))))))
                 ((= b2 79)             ; SS3
                  (let ((b3 (at 2)))
                    (if (null b3)
                        (return)
                        (progn (emit (case (code-char b3)
                                       (#\A :up) (#\B :down) (#\C :right) (#\D :left)
                                       (#\H :home) (#\F :end) (t nil)))
                               (incf i 3)))))
                 ((= b2 13) (emit :newline) (incf i 2))     ; Alt+Enter fallback
                 ((= b2 98) (emit :word-left) (incf i 2))   ; Alt+b
                 ((= b2 102) (emit :word-right) (incf i 2)) ; Alt+f
                 ((= b2 127) (emit :delete-word) (incf i 2)); Alt+Backspace
                 ((= b2 27)                                 ; ESC ESC
                  (emit :escape) (emit :escape) (incf i 2))
                 (t (incf i 2)))))      ; unknown alt-key: drop
            ;; --- plain bytes ---
            ((or (= b 13) (= b 10)) (emit :enter) (incf i))
            ((or (= b 127) (= b 8)) (emit :backspace) (incf i))
            ((= b 9) (emit (list :char #\Tab)) (incf i))
            ((< b 27)
             (emit (list :ctrl (code-char (+ 96 b))))
             (incf i))
            ((< b 32) (incf i))
            (t                          ; UTF-8 text
             (let ((len (utf8-length b)))
               (if (< (bytes-left) len)
                   (return)             ; split multibyte char, wait
                   (progn
                     (emit (list :char (char (decode-utf8 (subseq buf i (+ i len))) 0)))
                     (incf i len))))))))
      ;; Keep the unconsumed tail.
      (let ((rest (subseq buf i)))
        (setf (fill-pointer buf) 0)
        (loop for b across rest do (vector-push-extend b buf)))
      (nreverse events))))
