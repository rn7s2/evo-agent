;;;; windows-input-live.lisp — verify EVO.PORT:DRAIN-CONSOLE-INPUT against a
;;;; REAL Windows console, by injecting genuine INPUT_RECORDs with
;;;; WriteConsoleInputW and asserting the exact UTF-8 bytes evo produces.
;;;;
;;;; This is the proof for the virtual-key table that was written blind during
;;;; the remote bring-up: arrows, home/end/delete/pgup/pgdn, alt, CR-Enter,
;;;; surrogate pairs, repeat counts and ignored key-up events.  Run under the
;;;; built image's own environment:
;;;;
;;;;   sbcl --non-interactive --load tests/windows-input-live.lisp
;;;;
;;;; It opens CONIN$ (the console input buffer, reachable even when stdin is a
;;;; redirected pipe) and drives the exact code path the TUI uses.

(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload :evo :silent t)

(in-package :evo.port)

(sb-alien:define-alien-routine ("CreateFileW" %create-file-w) sb-alien:system-area-pointer
  (name (* sb-alien:unsigned-short)) (access sb-alien:unsigned-int)
  (share sb-alien:unsigned-int) (sa sb-alien:system-area-pointer)
  (disp sb-alien:unsigned-int) (flags sb-alien:unsigned-int)
  (templ sb-alien:system-area-pointer))
(sb-alien:define-alien-routine ("WriteConsoleInputW" %write-console-input) sb-alien:int
  (h sb-alien:system-area-pointer) (buf (* t)) (n sb-alien:unsigned-int)
  (written (* sb-alien:unsigned-int)))
(sb-alien:define-alien-routine ("FlushConsoleInputBuffer" %flush-console-input) sb-alien:int
  (h sb-alien:system-area-pointer))
(sb-alien:define-alien-routine ("PeekConsoleInputW" %peek-console-input) sb-alien:int
  (h sb-alien:system-area-pointer) (buf (* t)) (n sb-alien:unsigned-int)
  (got (* sb-alien:unsigned-int)))

(defun open-conin ()
  "A read/write handle on the console input buffer, independent of stdin."
  (sb-alien:with-alien ((name (sb-alien:array sb-alien:unsigned-short 8)))
    (loop for i from 0 for ch across "CONIN$" do (setf (sb-alien:deref name i) (char-code ch)))
    (setf (sb-alien:deref name 6) 0)
    (%create-file-w (sb-alien:cast (sb-alien:addr (sb-alien:deref name 0))
                                   (* sb-alien:unsigned-short))
                    #xC0000000 3 (sb-sys:int-sap 0) 3 0 (sb-sys:int-sap 0))))

(defun inject-keys (handle records)
  "RECORDS is a list of (:down D :repeat R :vk V :uchar U :state S) plists.
Writes each as a 20-byte INPUT_RECORD (KEY_EVENT) into HANDLE's buffer."
  (let ((n (length records)))
    (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-char 400))
                          (written sb-alien:unsigned-int))
      (let ((sap (sb-alien:alien-sap buf)))
        (dotimes (i (* n 20)) (setf (sb-sys:sap-ref-8 sap i) 0))
        (loop for rec in records
              for base from 0 by 20
              do (setf (sb-sys:sap-ref-16 sap base) 1) ; KEY_EVENT
                 (setf (sb-sys:sap-ref-32 sap (+ base 4)) (getf rec :down 1))
                 (setf (sb-sys:sap-ref-16 sap (+ base 8)) (getf rec :repeat 1))
                 (setf (sb-sys:sap-ref-16 sap (+ base 10)) (getf rec :vk 0))
                 (setf (sb-sys:sap-ref-16 sap (+ base 14)) (getf rec :uchar 0))
                 (setf (sb-sys:sap-ref-32 sap (+ base 16)) (getf rec :state 0)))
        (%write-console-input handle (sb-alien:cast (sb-alien:addr buf) (* t))
                              n (sb-alien:addr written))))))

(defun peek-first-key-repeat (handle)
  "The wRepeatCount the console actually stored for the first pending key-DOWN
event, read without consuming it (PeekConsoleInput).  NIL when no key-down is
pending.  Lets a test assert what DRAIN does with the count the console kept,
instead of assuming whether an injected repeat survives (a real console keeps
it; some redirected harnesses normalize it to 1)."
  (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-char 320))
                        (got sb-alien:unsigned-int))
    (let ((sap (sb-alien:alien-sap buf)))
      (dotimes (i 320) (setf (sb-sys:sap-ref-8 sap i) 0))
      (setf got 0)
      (when (and (/= 0 (%peek-console-input handle (sb-alien:cast (sb-alien:addr buf) (* t))
                                            16 (sb-alien:addr got)))
                 (plusp got))
        (loop for i below got
              for base = (* i 20)
              when (and (= (sb-sys:sap-ref-16 sap base) 1)          ; KEY_EVENT
                        (plusp (sb-sys:sap-ref-32 sap (+ base 4)))) ; bKeyDown
                do (return (sb-sys:sap-ref-16 sap (+ base 8))))))))  ; wRepeatCount

(defvar *conin* (open-conin))
(defvar *pass* 0)
(defvar *fail* 0)

(defun drained-bytes (records)
  "Inject RECORDS, drain, and return the UTF-8 bytes evo produced (a list)."
  (%flush-console-input *conin*)
  (setf *pending-surrogate* nil)
  (inject-keys *conin* records)
  (let ((v (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (drain-console-input *conin* v)
    (coerce v 'list)))

(defun check (name records expected)
  (let ((got (handler-case
                 (bt:with-timeout (5) (drained-bytes records))
               (bt:timeout () :timed-out))))
    (if (equal got expected)
        (progn (incf *pass*) (format t "  ok   ~a -> ~a~%" name got))
        (progn (incf *fail*)
               (format t "  FAIL ~a~%       expected ~a~%       got      ~a~%"
                       name expected got)))
    (finish-output)))

(format t "~&== EVO.PORT:DRAIN-CONSOLE-INPUT against a real console (CONIN$=~a) ==~%"
        (sb-sys:sap-int *conin*))

;; Plain characters (uChar carries them; vk is incidental).
(check "letter a"        '((:uchar 97))                 '(97))
(check "digit 7"         '((:uchar 55))                 '(55))
(check "space"           '((:uchar 32))                 '(32))
;; Enter in VT-input mode is a bare CR — the bug the branch chased.
(check "enter is CR"     '((:uchar 13 :vk #x0D))        '(13))
(check "tab"             '((:uchar 9 :vk #x09))         '(9))
(check "backspace"       '((:uchar 8))                  '(8))
(check "escape"          '((:uchar 27 :vk #x1B))        '(27))
(check "ctrl-c (ETX)"    '((:uchar 3))                  '(3))
;; The arrows carry no character: the virtual-key table synthesizes CSI.
(check "up"     '((:vk #x26))                           '(27 91 65))   ; ESC [ A
(check "down"   '((:vk #x28))                           '(27 91 66))   ; ESC [ B
(check "right"  '((:vk #x27))                           '(27 91 67))   ; ESC [ C
(check "left"   '((:vk #x25))                           '(27 91 68))   ; ESC [ D
(check "home"   '((:vk #x24))                           '(27 91 72))   ; ESC [ H
(check "end"    '((:vk #x23))                           '(27 91 70))   ; ESC [ F
(check "delete" '((:vk #x2E))                           '(27 91 51 126)) ; ESC [ 3 ~
(check "insert" '((:vk #x2D))                           '(27 91 50 126)) ; ESC [ 2 ~
(check "pgup"   '((:vk #x21))                           '(27 91 53 126)) ; ESC [ 5 ~
(check "pgdn"   '((:vk #x22))                           '(27 91 54 126)) ; ESC [ 6 ~
;; Alt is ESC then the key (LEFT_ALT_PRESSED = 0x0002).
(check "alt-x"  '((:uchar 120 :vk #x58 :state #x0002))  '(27 120))
;; Non-ASCII: one code unit becomes its UTF-8 bytes.
(check "CJK 中"  '((:uchar 20013))                       '(228 184 173))  ; U+4E2D
(check "e-acute" '((:uchar 233))                         '(195 169))      ; U+00E9
;; A surrogate pair arrives as two records and must be joined (U+1F600).
(check "emoji surrogate pair"
       '((:uchar #xD83D) (:uchar #xDE00))                '(240 159 152 128))
;; Key-up events are ignored; only key-down produces bytes.
(check "key-up ignored"  '((:down 0 :uchar 97))          '())
;; Repeat count: DRAIN replays the key wRepeatCount times (its (max 1 repeat)
;; loop).  A held key on real hardware raises wRepeatCount; we inject 3 but do
;; not assume it survives — a real console keeps 3, a redirected harness may
;; normalize it to 1.  Read back what the console actually stored and assert
;; DRAIN produced exactly that many copies, so the check exercises the repeat
;; loop wherever the count is preserved and still holds where it is not.
(let ()
  (%flush-console-input *conin*)
  (setf *pending-surrogate* nil)
  (inject-keys *conin* '((:uchar 97 :repeat 3)))
  (let* ((stored (or (peek-first-key-repeat *conin*) 1))
         (expected (make-list stored :initial-element 97))
         (v (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (drain-console-input *conin* v)
    (let ((got (coerce v 'list)))
      (if (equal got expected)
          (progn (incf *pass*)
                 (format t "  ok   repeat (console kept count ~a) -> ~a~%" stored got))
          (progn (incf *fail*)
                 (format t "  FAIL repeat (console kept count ~a)~%       expected ~a~%       got      ~a~%"
                         stored expected got)))
      (finish-output))))
;; A short burst in one drain, in order.
(check "burst h i + enter"
       '((:uchar 104) (:uchar 105) (:uchar 13 :vk #x0D)) '(104 105 13))

(format t "~%~d passed, ~d failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
