;;;; client.lisp — talking to a lane: serve's public HTTP API, nothing else.
;;;;
;;;; Every request carries the lane's bearer token and goes straight to
;;;; loopback (WITH-PROXY resolves no proxy for it).  Replies are decoded
;;;; through serve's one JSON mapping (EVO.SERVE:DECODE-JSON), so a lane's
;;;; state reads as the same plists the lane itself holds.

(in-package :evo.swarm)

(defun lane-url (lane path)
  "LANE's URL for PATH.  The base is the one its ready file published — the
only place a lane's port is written down, since a lane now picks its own
(--port 0) instead of being handed one nothing was listening on yet."
  (let ((base (getf (lane-ready lane) :url)))
    (unless base
      (error 'lane-error :lane lane :status nil
                         :text "its ready file has not been read yet"))
    (format nil "~a~a" (string-right-trim "/" base) path)))

(defun lane-headers (lane)
  (list (cons "Authorization" (format nil "Bearer ~a" (lane-token lane)))
        (cons "Content-Type" "application/json")))

(defun lane-request (lane method path &key body (timeout 60))
  "One request to LANE.  Returns (values STATUS REPLY), REPLY decoded to a
plist.  A 4xx/5xx is a normal return; a lane that cannot be reached signals
LANE-ERROR."
  (let ((url (lane-url lane path)))
    (handler-case
        (with-proxy (proxy url)
          (multiple-value-bind (text status)
              (apply #'dex:request url :method method
                                       :headers (lane-headers lane)
                                       :content (and body (evo.serve:encode-json body))
                                       :keep-alive nil :force-string t
                                       :connect-timeout 5 :read-timeout timeout
                                       (when proxy (list :proxy proxy)))
            (values status (ignore-errors (evo.serve:decode-json text)))))
      (dex:http-request-failed (e)
        (values (dex:response-status e)
                (ignore-errors (evo.serve:decode-json (dex:response-body e)))))
      (error (e)
        (error 'lane-error :lane lane :status nil :text (format nil "~a" e))))))

(defun lane-get (lane path &key (timeout 30))
  (lane-request lane :get path :timeout timeout))

(defun lane-post (lane path &key (body '(:none t)) (timeout 60))
  "POST BODY (a plist; serve ignores keys it does not know) to LANE."
  (lane-request lane :post path :body body :timeout timeout))

(defun lane-ok (lane status reply what)
  "REPLY when STATUS is 2xx, else signal LANE-ERROR naming WHAT and the
lane's own error text."
  (unless (and status (< status 300))
    (error 'lane-error :lane lane :status status
                       :text (format nil "~a: ~a" what
                                     (or (getf reply :error) "failed"))))
  reply)

(defun lane-health (lane)
  "The lane's /health plist, or NIL when it does not answer."
  (ignore-errors
    (multiple-value-bind (status reply) (lane-get lane "/health" :timeout 5)
      (and (eql status 200) reply))))

(defun lane-eval (lane code)
  "Evaluate CODE (a string: a body of forms) in LANE, as its `eval` tool
would.  Returns the reply; a failed evaluation signals LANE-ERROR."
  (multiple-value-bind (status reply) (lane-post lane "/eval" :body (list :code code))
    (lane-ok lane status reply "eval")))

(defun lane-command (lane text)
  "Run the slash command TEXT in LANE.  Returns (values STATUS REPLY)."
  (lane-post lane "/command" :body (list :text text)))

;;; The event stream.

(defun open-event-stream (lane &key since)
  "GET /events from LANE, as a character stream.  SINCE is the cursor to
replay after; :LIVE tails from the lane's next event; NIL (the default) is
what the coordinator's subscription does — resume after LANE's cursor."
  (let* ((query (cond ((eq since :live) "")
                      ((integerp since) (format nil "?since=~d" since))
                      (t (format nil "?since=~d" (lane-cursor lane)))))
         (url (lane-url lane (format nil "/events~a" query))))
    (with-proxy (proxy url)
      (flexi-streams:make-flexi-stream
       (apply #'dex:get url :headers (lane-headers lane)
                            :want-stream t :force-binary t :keep-alive nil
                            :connect-timeout 5 :read-timeout 60
                            (when proxy (list :proxy proxy)))
       :external-format (flexi-streams:make-external-format :utf-8 :eol-style :lf)))))

(defun read-sse-events (stream fn &key on-comment)
  "Call FN with (ID TYPE DATA) for each event on STREAM until it ends.  When
ON-COMMENT is given, call it with each SSE comment's text; relays use that to
preserve keepalives.  The kernel's MAP-SSE-EVENTS drops ids, but a lane's cursor
is made of them."
  (let ((id nil) (type nil) (data nil))
    (loop for line = (read-line stream nil nil)
          while line
          do (let ((line (string-right-trim '(#\Return) line)))
               (cond ((zerop (length line))
                      (when data
                        (funcall fn id type (format nil "~{~a~^~%~}" (nreverse data))))
                      (setf id nil type nil data nil))
                     ((string-prefix-p ":" line)
                      (when on-comment
                        (funcall on-comment (string-left-trim " " (subseq line 1)))))
                     ((string-prefix-p "id:" line)
                      (setf id (ignore-errors
                                 (parse-integer (string-trim " " (subseq line 3))))))
                     ((string-prefix-p "event:" line)
                      (setf type (string-trim " " (subseq line 6))))
                     ((string-prefix-p "data:" line)
                      (push (string-left-trim " " (subseq line 5)) data)))))))
