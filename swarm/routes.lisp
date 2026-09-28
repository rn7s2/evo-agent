;;;; routes.lisp — the swarm's read-only HTTP endpoints, registered on the
;;;; serve server that serves the coordinator (`evo-swarm serve`).
;;;;
;;;;   GET /lanes                  the swarm and its lanes, JSON
;;;;   GET /lanes/N/transcript     lane N's context, relayed from its serve
;;;;   GET /lanes/N/events         lane N's own SSE stream, relayed live
;;;;
;;;; One prefix route serves all three (ADD-ROUTE trims a trailing slash, so an
;;;; exact "/lanes" and a "/lanes/" prefix would be the same entry, the second
;;;; replacing the first).  Handlers take (SERVER REQUEST BODY STREAM), exactly
;;;; as serve's own do; BODY is parsed JSON and unused for a GET.
;;;;
;;;; Registered only for a swarm server, and only GETs: a lane is driven by the
;;;; coordinator, not by a second writer.  A reply that cannot be a stream —
;;;; no such lane, no lane number — is a plain 400/404 JSON error, written
;;;; before any SSE header, so a client sees it as an HTTP failure.

(in-package :evo.swarm)

(defparameter *swarm-identity* '(:name "evo-swarm" :version "0.1.0" :features ("swarm"))
  "Who this program is, as the coordinator's GET /health reports it.  A
separate program from the `evo` serving the lanes, so a client can tell which
one answered, and negotiate on :features.")

;;; Reading the request.

(defun serve-query (request name)
  "REQUEST's query parameter NAME.  EVO.SERVE exports REQUEST-QUERY (the raw
alist) but not its own accessor, so the lookup lives here."
  (cdr (assoc name (evo.serve:request-query request) :test #'string=)))

(defun request-limit (request)
  "The client's ?limit=, NIL when absent, or :INVALID."
  (let ((text (and request (serve-query request "limit"))))
    (cond ((null text) nil)
          ((let ((n (ignore-errors (parse-integer text))))
             (and n (not (minusp n)) n)))
          (t :invalid))))

(defun request-cursor (request)
  "The client's resume cursor: Last-Event-ID, then ?since=N, else :LIVE.  A
present cursor must be a non-negative integer; otherwise return :INVALID."
  (let ((text (and request
                   (or (evo.serve:request-header request "last-event-id")
                       (serve-query request "since")))))
    (cond ((null text) :live)
          ((let ((n (ignore-errors (parse-integer text))))
             (and n (not (minusp n)) n)))
          (t :invalid))))

(defun route-path (request)
  "The part of REQUEST's path below the /lanes prefix, \"\" for the bare
listing.  A prefix route's handler reads the rest of the path from serve's
*ROUTE-TAIL*; the request's own path is the fallback, so the handler works
however the route was reached."
  (let ((tail evo.serve:*route-tail*))
    (if tail
        (string-left-trim "/" tail)
        (let ((path (or (and request (evo.serve:request-path request)) "")))
          (cond ((string= (string-right-trim "/" path) "/lanes") "")
                ((string-prefix-p "/lanes/" path) (subseq path 7))
                (t (string-left-trim "/" path)))))))

(defun route-lane (path)
  "The lane number PATH names: \"3/transcript\" -> 3."
  (let* ((text (string-left-trim "/" (or path "")))
         (slash (position #\/ text)))
    (ignore-errors (parse-integer (subseq text 0 slash)))))

(defun route-action (path)
  "What PATH asks for: the part after the lane number, \"/transcript\"."
  (let* ((text (string-left-trim "/" (or path "")))
         (slash (position #\/ text)))
    (and slash (subseq text slash))))

;;; The endpoints.

(defun handle-swarm-lanes (server request body stream)
  "GET /lanes — the swarm and every lane."
  (declare (ignore server request body))
  (if *swarm*
      (evo.serve:write-json stream 200 (swarm-lanes-response))
      (evo.serve:write-error stream 503 "no swarm is running")))

(defun handle-swarm-lane-transcript (server request body stream)
  "GET /lanes/N/transcript — lane N's context, decoded."
  (declare (ignore server body))
  (let ((n (route-lane (route-path request)))
        (limit (request-limit request)))
    (cond
      ((null n) (evo.serve:write-error stream 400 "a lane number is required"))
      ((eq limit :invalid) (evo.serve:write-error stream 400 "limit must be an integer"))
      ((null (find-lane n)) (evo.serve:write-error stream 404 (format nil "no lane ~d" n)))
      (t (handler-case
             (evo.serve:write-json stream 200
                                   (lane-transcript (find-lane n) :limit limit))
           (lane-error (e) (evo.serve:write-error stream 502 (format nil "~a" e))))))))

(defun handle-swarm-lane-events (server request body stream)
  "GET /lanes/N/events — lane N's own event stream, relayed live."
  (declare (ignore server body))
  (let* ((n (route-lane (route-path request)))
         (lane (and n (find-lane n)))
         (cursor (request-cursor request)))
    (cond
      ((null n) (evo.serve:write-error stream 400 "a lane number is required"))
      ((eq cursor :invalid)
       (evo.serve:write-error stream 400
                              "Last-Event-ID / since must be a non-negative integer"))
      ((null lane) (evo.serve:write-error stream 404 (format nil "no lane ~d" n)))
      (t
       (evo.serve:write-sse-head stream)
       (evo.serve:write-sse-comment stream (format nil "lane ~d events" n))
       ;; Any failure — the lane is gone, the client left — ends the stream,
       ;; saying so in-band while the client is still there to read it.
       (handler-case (relay-lane-events lane stream :since cursor)
         (error (e)
           (ignore-errors
             (evo.serve:write-sse-event
              stream "error"
              (evo.serve:encode-json (list :error (format nil "~a" e)))))))))))

(defun handle-swarm-lane-route (server request body stream)
  "The /lanes prefix: the listing at the bare path, else N per action."
  (let* ((path (route-path request))
         (action (route-action path)))
    (cond
      ((equal path "") (handle-swarm-lanes server request body stream))
      ((equal action "/transcript") (handle-swarm-lane-transcript server request body stream))
      ((equal action "/events") (handle-swarm-lane-events server request body stream))
      (t (evo.serve:write-error stream 404 "no such swarm endpoint")))))

;;; Registration — the headless coordinator's setup calls this, once, on the
;;; server it is about to serve.  A lane's own serve never sees these routes.

(defun register-swarm-routes (&key (prefix "/lanes"))
  "Add the swarm's read-only routes to serve's route table.  Called for the
server that serves the coordinator; a lane's own serve never calls it."
  (evo.serve:add-route (concatenate 'string prefix "/") :get
                       #'handle-swarm-lane-route :prefix t))
