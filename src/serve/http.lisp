;;;; http.lisp — the smallest HTTP/1.1 evo needs, on octet streams.
;;;;
;;;; One request per connection: the server answers and closes (Connection:
;;;; close), which HTTP/1.1 permits and which keeps the whole server free of
;;;; keep-alive state.  Request bodies need a Content-Length — the clients this
;;;; serves (a coordinator, curl, a test) all send one — so chunked uploads are
;;;; refused with 411 rather than half-supported.  Responses are either a JSON
;;;; body with its length, or a Server-Sent Events stream that ends when the
;;;; server closes it.
;;;;
;;;; Everything here reads and writes (UNSIGNED-BYTE 8) streams and nothing
;;;; else, so the unit suite drives it with in-memory streams and the server
;;;; with sockets, through the same code.

(in-package :evo.serve)

(defparameter +utf-8+ (flexi-streams:make-external-format :utf-8 :eol-style :lf)
  "UTF-8 with #\\Newline as a bare LF, on every platform.  Plain :UTF-8 takes
flexi-streams' default eol-style, which is CRLF on Windows: every #\\Newline
would gain a CR there, and the CRLFs this file writes on purpose would become
CR CR LF — a malformed response.  HTTP framing decides its own line ends.")

(defparameter *max-header-bytes* (* 64 1024)
  "Request line plus headers.  Past this the request is refused (431).")

(defparameter *max-body-bytes* (* 64 1024 1024)
  "Largest request body accepted (413 past it).  Generous on purpose: a prompt
may carry images by value, base64 and all.")

(define-condition http-error (error)
  ((status :initarg :status :reader http-error-status)
   (text :initarg :text :reader http-error-text))
  (:report (lambda (c s) (format s "~d ~a" (http-error-status c) (http-error-text c)))))

(defun http-fail (status control &rest args)
  (error 'http-error :status status :text (apply #'format nil control args)))

(defstruct (request (:constructor %make-request))
  method          ; "GET", "POST", ...
  path            ; decoded path, no query: "/events"
  query           ; alist of (name . value), decoded
  headers         ; alist of (lowercased-name . value)
  body)           ; string (UTF-8 decoded), "" when none

(defun request-header (request name)
  "The value of header NAME (case-insensitive) in REQUEST, or NIL."
  (cdr (assoc (string-downcase name) (request-headers request) :test #'string=)))

(defun request-query-param (request name)
  (cdr (assoc name (request-query request) :test #'string=)))

;;; Reading.

(defun read-octet-line (stream budget)
  "One CRLF- (or LF-) terminated line from STREAM as a Latin-1 string, CR
stripped; NIL at end of stream before any byte.  BUDGET is a cons whose car
counts the header bytes still allowed."
  (let ((bytes (make-array 64 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (loop
      (let ((b (read-byte stream nil nil)))
        (cond ((null b)
               (return (and (plusp (length bytes))
                            (http-fail 400 "connection closed mid-line"))))
              ((= b 10)
               (when (and (plusp (length bytes))
                          (= 13 (aref bytes (1- (length bytes)))))
                 (vector-pop bytes))
               (return (map 'string #'code-char bytes)))
              (t
               (when (minusp (decf (car budget)))
                 (http-fail 431 "request headers too large"))
               (vector-push-extend b bytes)))))))

(defun hex-digit (char)
  (digit-char-p char 16))

(defun percent-decode (text &key plus-space)
  "Decode %XX escapes in TEXT as UTF-8 (and + as a space when PLUS-SPACE)."
  (let ((octets (make-array (length text) :element-type '(unsigned-byte 8)
                                          :fill-pointer 0 :adjustable t)))
    (loop with i = 0
          while (< i (length text))
          do (let ((c (char text i)))
               (cond ((and (char= c #\%) (< (+ i 2) (length text))
                           (hex-digit (char text (+ i 1)))
                           (hex-digit (char text (+ i 2))))
                      (vector-push-extend (+ (* 16 (hex-digit (char text (+ i 1))))
                                      (hex-digit (char text (+ i 2))))
                                   octets)
                      (incf i 3))
                     ((and plus-space (char= c #\+))
                      (vector-push-extend 32 octets) (incf i))
                     (t
                      (loop for b across (flexi-streams:string-to-octets
                                          (string c) :external-format +utf-8+)
                            do (vector-push-extend b octets))
                      (incf i)))))
    (flexi-streams:octets-to-string octets :external-format +utf-8+)))

(defun parse-query (text)
  "\"a=1&b=x%20y\" -> ((\"a\" . \"1\") (\"b\" . \"x y\"))."
  (loop for pair in (uiop:split-string (or text "") :separator "&")
        for eq = (position #\= pair)
        when (plusp (length pair))
          collect (cons (percent-decode (subseq pair 0 eq) :plus-space t)
                        (if eq (percent-decode (subseq pair (1+ eq)) :plus-space t) ""))))

(defun parse-request-line (line)
  "\"POST /x?y=1 HTTP/1.1\" -> (values method path query)."
  (let ((parts (remove "" (uiop:split-string line :separator " ") :test #'string=)))
    (unless (= 3 (length parts))
      (http-fail 400 "malformed request line"))
    (destructuring-bind (method target version) parts
      (unless (string-prefix-p "HTTP/1." version)
        (http-fail 505 "HTTP version not supported"))
      (unless (and (plusp (length target)) (char= (char target 0) #\/))
        (http-fail 400 "request target must be an absolute path"))
      (let ((q (position #\? target)))
        (values (string-upcase method)
                (percent-decode (subseq target 0 q))
                (and q (parse-query (subseq target (1+ q)))))))))

(defun read-request (stream)
  "Read one request from STREAM, an octet stream.  Returns a REQUEST, or NIL
when the peer closed without sending anything.  Signals HTTP-ERROR for
anything malformed, oversized, or using a framing this server does not take."
  (let* ((budget (list *max-header-bytes*))
         (line (loop for l = (read-octet-line stream budget)
                     ;; RFC 9112 §2.2: ignore empty lines before the request.
                     while (and l (zerop (length l)))
                     finally (return l))))
    (unless line (return-from read-request nil))
    (multiple-value-bind (method path query) (parse-request-line line)
      (let ((headers
              (loop for l = (read-octet-line stream budget)
                    until (or (null l) (zerop (length l)))
                    collect (let ((colon (position #\: l)))
                              (unless (and colon (plusp colon))
                                (http-fail 400 "malformed header line"))
                              (cons (string-downcase (string-trim " " (subseq l 0 colon)))
                                    (string-trim '(#\Space #\Tab) (subseq l (1+ colon))))))))
        (when (assoc "transfer-encoding" headers :test #'string=)
          (http-fail 411 "send a Content-Length; chunked bodies are not accepted"))
        (let* ((length-header (cdr (assoc "content-length" headers :test #'string=)))
               (length (if length-header
                           (or (ignore-errors (parse-integer length-header))
                               (http-fail 400 "bad Content-Length"))
                           0)))
          (when (minusp length) (http-fail 400 "bad Content-Length"))
          (when (> length *max-body-bytes*)
            (http-fail 413 "request body over ~d bytes" *max-body-bytes*))
          (let ((octets (make-array length :element-type '(unsigned-byte 8))))
            (unless (= length (read-sequence octets stream))
              (http-fail 400 "request body shorter than its Content-Length"))
            (%make-request
             :method method :path path :query query :headers headers
             :body (handler-case (flexi-streams:octets-to-string
                                  octets :external-format +utf-8+)
                     (error () (http-fail 400 "request body is not UTF-8"))))))))))

;;; Writing.

(defparameter *status-texts*
  '((200 . "OK") (400 . "Bad Request") (401 . "Unauthorized")
    (404 . "Not Found") (405 . "Method Not Allowed") (409 . "Conflict")
    (411 . "Length Required") (413 . "Content Too Large")
    (422 . "Unprocessable Content") (431 . "Request Header Fields Too Large")
    (500 . "Internal Server Error") (503 . "Service Unavailable")
    (505 . "HTTP Version Not Supported")))

(defun status-text (status)
  (or (cdr (assoc status *status-texts*)) "Unknown"))

(defun write-octets (stream string)
  (write-sequence (flexi-streams:string-to-octets string :external-format +utf-8+)
                  stream))

(defun write-head (stream status headers)
  "Status line and HEADERS (an alist), then the blank line."
  (let ((crlf (coerce '(#\Return #\Newline) 'string)))
    (write-octets stream (format nil "HTTP/1.1 ~d ~a~a" status (status-text status) crlf))
    (loop for (name . value) in headers
          do (write-octets stream (format nil "~a: ~a~a" name value crlf)))
    (write-octets stream crlf)))

(defun write-response (stream status body &key (content-type "application/json")
                                               extra-headers)
  "A complete response: STATUS, BODY (a string), closed after."
  (let ((octets (flexi-streams:string-to-octets body :external-format +utf-8+)))
    (write-head stream status
                (append (list (cons "Content-Type"
                                    (format nil "~a; charset=utf-8" content-type))
                              (cons "Content-Length" (length octets))
                              (cons "Cache-Control" "no-store")
                              (cons "Connection" "close"))
                        extra-headers))
    (write-sequence octets stream)
    (finish-output stream)))

(defun write-sse-head (stream)
  "Start a Server-Sent Events response.  No length: it ends when we close."
  (write-head stream 200 '(("Content-Type" . "text/event-stream; charset=utf-8")
                           ("Cache-Control" . "no-store")
                           ("Connection" . "close")
                           ("X-Accel-Buffering" . "no")))
  (finish-output stream))

(defun write-sse-event (stream type data &key id)
  "One event: optional ID, TYPE, and DATA (a JSON string, one line).  The ID
is a cursor, \"<epoch>.<seq>\", not a number: it is what a client sends back
in `since` when it reconnects."
  (write-octets stream (format nil "~@[id: ~a~%~]event: ~a~%data: ~a~%~%" id type data))
  (finish-output stream))

(defun write-response-octets (stream status octets content-type)
  "A complete response whose body is bytes — an image, served as it is."
  (write-head stream status
              (list (cons "Content-Type" content-type)
                    (cons "Content-Length" (length octets))
                    (cons "Cache-Control" "no-store")
                    (cons "Connection" "close")))
  (write-sequence octets stream)
  (finish-output stream))

(defun write-sse-comment (stream text)
  "A comment line — a keepalive a client ignores, and a write that notices a
peer which went away."
  (write-octets stream (format nil ": ~a~%~%" text))
  (finish-output stream))
