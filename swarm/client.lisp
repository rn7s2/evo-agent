;;;; client.lisp — talking to a lane: the new serve protocol, nothing else.
;;;;
;;;; A lane is an `evo-agent serve` process the coordinator owns (CONTRACT §6).
;;;; Its address and its bearer token come from the ready file it writes once
;;;; it is listening; every request goes straight to loopback (WITH-PROXY
;;;; resolves no proxy for it).  Reads — the snapshot and the op stream —
;;;; arrive as JSON through serve's one mapping (EVO.SERVE:DECODE-JSON), so a
;;;; lane's state reads as the same plists the lane itself holds.  Input and
;;;; commands go through POST /ops, exactly as a person's client would.

(in-package :evo.swarm)

(defun ready-file-path (lane)
  (merge-pathnames "ready.json" (lane-dir lane)))

(defun read-ready-file (lane)
  "LANE's ready file as a plist, or NIL while it has not been written yet.
The file is written atomically (tmp + rename), so a read sees all of it or
none of it."
  (let ((path (ready-file-path lane)))
    (when (probe-file path)
      (ignore-errors
        (let ((body (evo.serve:decode-json (read-file-string path))))
          (when (and (listp body) (getf body :port) (getf body :token))
            body))))))

(defun lane-refresh-ready (lane)
  "Re-read LANE's ready file into the lane and return it, or NIL."
  (let ((ready (read-ready-file lane)))
    (when ready
      (with-swarm-lock ()
        (setf (lane-ready lane) ready
              (lane-session-path lane) (getf (getf ready :session) :path))))
    ready))

(defun lane-port (lane) (getf (lane-ready lane) :port))
(defun lane-token (lane) (getf (lane-ready lane) :token))

(defun lane-url (lane path)
  "LANE's URL for PATH, from the base its ready file published — the only
place a lane's port is written down, since a lane picks its own (--port 0)."
  (let ((base (or (getf (lane-ready lane) :url)
                  (let ((port (lane-port lane)))
                    (and port (format nil "http://127.0.0.1:~d/" port))))))
    (unless base
      (error 'lane-error :lane lane :code "not_ready"
                         :text "the lane has not written its ready file yet"))
    (format nil "~a~a" (string-right-trim "/" base) path)))

(defun lane-headers (lane)
  (list (cons "Authorization" (format nil "Bearer ~a" (lane-token lane)))
        (cons "Content-Type" "application/json")))

(defun lane-request (lane method path &key body (timeout 60) binary)
  "One request to LANE.  Returns (values STATUS REPLY HEADERS), REPLY decoded
to a plist — or, with BINARY, the raw bytes.  A 4xx/5xx is a normal return; a
lane that cannot be reached or has no ready file yet signals LANE-ERROR."
  (unless (lane-ready lane)
    (error 'lane-error :lane lane :code "not_ready"
                       :text "the lane has not written its ready file yet"))
  (let ((url (lane-url lane path)))
    (handler-case
        (with-proxy (proxy url)
          (multiple-value-bind (body status headers)
              (apply #'dex:request url :method method
                                   :headers (lane-headers lane)
                                   :content (and body (evo.serve:encode-json body))
                                   :keep-alive nil :force-string (not binary)
                                   :force-binary binary
                                   :connect-timeout 5 :read-timeout timeout
                                   (when proxy (list :proxy proxy)))
            (values status
                    (if binary body (ignore-errors (evo.serve:decode-json body)))
                    headers)))
      (dex:http-request-failed (e)
        (values (dex:response-status e)
                (ignore-errors (evo.serve:decode-json (dex:response-body e)))
                (ignore-errors (dex:response-headers e))))
      (error (e)
        (error 'lane-error :lane lane :code nil :text (format nil "~a" e))))))

(defun lane-get (lane path &key (timeout 30))
  (lane-request lane :get path :timeout timeout))

(defun lane-get-body (lane path &key (timeout 30) (what "request"))
  "PATH's 200 body, or a LANE-ERROR naming WHAT."
  (multiple-value-bind (status reply) (lane-get lane path :timeout timeout)
    (unless (and status (< status 300))
      (error 'lane-error :lane lane
                         :code (and (listp reply) (getf (getf reply :error) :code))
                         :text (format nil "~a failed~@[: ~a~]" what
                                       (and (listp reply)
                                            (getf (getf reply :error) :message)))))
    reply))

;; NOTINLINE: the unit suite replaces this (and LANE-SNAPSHOT below) to
;; drive a swarm with no processes behind it; ECL inlines a same-file
;; call and would leave the suite talking to the real one.
(declaim (notinline lane-op lane-snapshot))

(defun lane-op (lane op args &key (timeout 60))
  "Run OP in LANE through POST /ops and return its result plist.  A refusal
signals LANE-ERROR carrying serve's error code and message."
  (multiple-value-bind (status reply)
      (lane-request lane :post "/ops"
                    :body (list :rid (gen-id 16) :op op :args (or args (list)))
                    :timeout timeout)
    (cond
      ((eql status 401)
       (error 'lane-error :lane lane :code "unauthorized" :text "the lane refused the token"))
      ((not (listp reply))
       (error 'lane-error :lane lane :code nil
                          :text (format nil "no reply (HTTP ~a)" status)))
      ((getf reply :ok) (getf reply :result))
      (t (let ((error (getf reply :error)))
           (error 'lane-error :lane lane :code (getf error :code)
                              :text (or (getf error :message) "op failed")))))))

;;; The reads.

(defun lane-snapshot (lane &key (topics "session") (items 200) (timeout 30))
  "LANE's /snapshot body: one atomic view of the topics asked for."
  (lane-get-body lane (format nil "/snapshot?topics=~a&items=~d" topics items)
                 :timeout timeout :what "snapshot"))

(defun lane-topic-snapshot (lane &key (topic "session") (items 200))
  "One topic's entry from LANE's snapshot: (:state … :items #(…) :has-more …)."
  (let* ((topic (if (stringp topic) (intern (string-upcase topic) :keyword) topic))
         (topics (getf (lane-snapshot lane :topics (string-downcase (symbol-name topic))
                                     :items items)
                       :topics)))
    (getf topics topic)))

(defun lane-health (lane)
  "The lane's /health plist, or NIL when it does not answer."
  (ignore-errors
    (multiple-value-bind (status reply) (lane-get lane "/health" :timeout 5)
      (and (eql status 200) reply))))

(defun lane-items-before (lane topic before limit)
  "Older items of LANE's TOPIC: (values ITEMS HAS-MORE)."
  (let ((body (lane-get-body lane (format nil "/items?topic=~a~@[&before=~a~]&limit=~d"
                                          topic before limit)
                             :what "items")))
    (values (coerce (or (getf body :items) #()) 'list) (getf body :has-more))))

(defun lane-item (lane topic id)
  "One item of LANE's TOPIC, untruncated: the item plist or NIL."
  (let ((body (ignore-errors
                (lane-get-body lane (format nil "/items/~a?topic=~a" id topic)
                               :what "item"))))
    (getf body :item)))

(defun lane-media (lane topic id n)
  "Raw bytes of one image on LANE's TOPIC: (values OCTETS MEDIA-TYPE)."
  (multiple-value-bind (status bytes headers)
      (lane-request lane :get (format nil "/media/~a/~d?topic=~a" id n topic)
                    :binary t :timeout 30)
    (unless (and status (< status 300))
      (error 'lane-error :lane lane :code nil :text "media request failed"))
    (values bytes (cdr (assoc "content-type" headers :test #'string-equal)))))

;;; The stream.

(defun lane-stream-path (&key (topics "session") since)
  (format nil "/stream?topics=~a~@[&since=~a~]" topics since))

(defun lane-open-stream (lane &key (topics "session") since)
  "GET /stream from LANE, as a character stream.  SINCE is the lane's cursor
(\"<epoch>.<seq>\"); NIL starts at the tip (after the hello frame)."
  (let ((url (lane-url lane (lane-stream-path :topics topics :since since))))
    (with-proxy (proxy url)
      (flexi-streams:make-flexi-stream
       (apply #'dex:get url :headers (lane-headers lane)
                            :want-stream t :force-binary t :keep-alive nil
                            :connect-timeout 5 :read-timeout 600
                            (when proxy (list :proxy proxy)))
       :external-format (flexi-streams:make-external-format :utf-8 :eol-style :lf)))))

(defun read-sse-events (stream fn &key on-comment)
  "Call FN with (ID TYPE DATA) for each event on STREAM until it ends.  ID is
the frame's id text, untouched — serve's is \"<epoch>.<seq>\", and a lane's
cursor is made of it.  With ON-COMMENT, call it with each SSE comment's text."
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
                      (setf id (string-trim " " (subseq line 3))))
                     ((string-prefix-p "event:" line)
                      (setf type (string-trim " " (subseq line 6))))
                     ((string-prefix-p "data:" line)
                      (push (string-left-trim " " (subseq line 5)) data)))))))

(defun split-cursor (id)
  "An SSE id \"<epoch>.<seq>\" as (values EPOCH SEQ), or NIL when it is not one."
  (when (and (stringp id) (plusp (length id)))
    (let ((dot (position #\. id)))
      (when dot
        (values (subseq id 0 dot)
                (ignore-errors (parse-integer id :start (1+ dot))))))))
