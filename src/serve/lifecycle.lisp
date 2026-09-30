;;;; lifecycle.lisp — how a serving process announces itself and when it
;;;; stops.
;;;;
;;;; Two facts about a served session are not in the protocol, because a
;;;; client needs them before it can speak it at all: the URL and the bearer
;;;; token.  They reach the client through the **ready file** — written
;;;; atomically once the listener is bound, rewritten by every life of the
;;;; process (so its epoch and pid are the live ones), deleted on a clean
;;;; exit.  One file, one watch: no stdout parsing, no /health polling, and
;;;; no window in which a client holds a token the server does not.
;;;;
;;;; The other half is parent death, seen as end-of-file on stdin.  A pipe a
;;;; client holds is the one liveness signal that cannot lie — a pid can be
;;;; reused, and a connection handler answers /health even when the session
;;;; thread is wedged.  EOF is immediate, and it is what --watch-stdin means.

(in-package :evo.serve)

(defun mint-epoch ()
  "A new epoch: the short opaque tag that qualifies every cursor of one
serving life.  Minted per process, so a client that reconnects after a
restart can tell — in-band, without comparing pids — that its cursor belongs
to a log that no longer exists."
  (format nil "~(~{~2,'0x~}~)" (coerce (evo.port:random-octets 4) 'list)))

(defun write-json-atomically (path value)
  "Write VALUE as one JSON document to PATH: a temporary file beside it,
private (0600) from the moment it exists, renamed over the target — so a
reader never sees a half-written file, however often it is rewritten.
Returns PATH."
  (let* ((path (merge-pathnames path))
         (tmp (format nil "~a.tmp" (namestring path)))
         (text (format nil "~a~%" (encode-json value))))
    (ensure-directories-exist path)
    (evo.port:write-private-file tmp text)
    (handler-case (rename-file tmp path)
      (error ()
        ;; An implementation whose RENAME-FILE will not replace an existing
        ;; file: write in place instead.  Still private, still valid JSON —
        ;; only the atomicity is lost, and only on a platform that has no
        ;; other way.
        (evo.port:write-private-file path text)
        (ignore-errors (delete-file tmp))))
    path))

(defun delete-ready-file (path)
  "Forget PATH's ready file: this process is going away and nobody should
trust what it says afterwards.  Best effort."
  (when path
    (ignore-errors (delete-file (merge-pathnames path)))))

(defun watch-stdin-eof (thunk &key (name "evo-watch-stdin"))
  "Call THUNK, on a thread of its own, the moment this process's stdin reaches
end of file — what --watch-stdin means (see the file header).  Returns the
thread, or NIL when stdin cannot be watched this way: on Windows a console is
read through its own API, not as a byte stream, and there is no pipe to lose.
The thread is a reader parked on the file descriptor; it needs no teardown,
and closing the stream at EOF releases nothing anybody else holds."
  (let ((stream (ignore-errors (evo.port:make-stdin-stream))))
    (when (typep stream 'stream)
      (bt:make-thread
       (lambda ()
         (handler-case (loop until (eq (read-byte stream nil :eof) :eof))
           (error () nil))
         (ignore-errors (close stream))
         (funcall thunk))
       :name name))))
