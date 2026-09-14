;;;; core.lisp — provider infrastructure shared by all APIs: the JSON
;;;; bridge, the handoff pass, the SSE transport loop, HTTP + retry, and
;;;; API dispatch in call-provider.  The bundled wire adapter lives in
;;;; anthropic.lisp (protocol: api.lisp).
;;;;
;;;; One unified message model; stateless replay (full history each request);
;;;; hand-rolled SSE; errors are data — this layer never signals into the
;;;; loop: failures become an assistant message with :stop-reason :error (or
;;;; :aborted) plus :error-message.

(in-package :evo.provider)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (evo.port:add-package-local-nickname :jzon :com.inuoe.jzon :evo.provider))

(define-condition provider-error (error)
  ((message :initarg :message :reader provider-error-message)
   (retryable :initarg :retryable :initform nil :reader provider-error-retryable-p)
   (retry-after :initarg :retry-after :initform nil :reader provider-error-retry-after))
  (:report (lambda (c s) (format s "~a" (provider-error-message c)))))

;;; JSON <-> sexpr bridging.
;;;
;;; Rule: JSON objects are plists (keyword keys, `_` <-> `-`), JSON arrays are
;;; CL vectors — never lists — so the two are unambiguous in the journal.

(defun key->keyword (string)
  (intern (substitute #\- #\_ (string-upcase string)) :keyword))

(defun keyword->key (keyword)
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun json->sexpr (value)
  (typecase value
    (hash-table (loop for k being the hash-keys of value using (hash-value v)
                      append (list (key->keyword k) (json->sexpr v))))
    (string value)
    (integer value)
    (double-float value)
    (real value)
    (vector (map 'vector #'json->sexpr value))
    (symbol (cond ((eq value t) t) (t nil)))  ; false/null -> nil
    (t nil)))

(defun sexpr->json (value)
  (typecase value
    (cons (let ((h (make-hash-table :test #'equal)))
            (loop for (k v) on value by #'cddr
                  do (setf (gethash (keyword->key k) h) (sexpr->json v)))
            h))
    (string value)
    (number value)
    (keyword (string-downcase (symbol-name value)))
    (vector (map 'vector #'sexpr->json value))
    (t value)))                         ; t/nil pass through as true/false

(defun jobj (&rest kvs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k h) v))
    h))

(defun jget (obj &rest keys)
  (let ((v obj))
    (dolist (k keys v)
      (setf v (and (hash-table-p v) (gethash k v))))))

;;; Handoff pass: run at request build time.
;;;  - errored/aborted assistant turns are elided
;;;  - same-model thinking replays verbatim; cross-model thinking is dropped
;;;  - images degrade to text for a model without vision
;;;  - even with vision, only the last few images ride along as pixels; older
;;;    ones become named placeholders
;;;  - orphaned tool calls get synthetic error results

(defparameter *max-request-images* 3
  "How many of the most recent image blocks a request carries as pixels.

Images travel by value and every request re-uploads the ones it carries, so a
session that keeps taking screenshots pays for all of them on every turn, for
the rest of the session.  A real session measured here: eighteen screenshots,
8.9 MB of base64 in each of 305 requests, 1.2 GB uploaded in under an hour —
and one link hiccup during any of those uploads is indistinguishable from a
hang.  The model needs the picture it just asked to see, not the one from
forty turns ago; what it can no longer see, it can read again.")

(defparameter *max-request-image-data-chars* (* 4 1024 1024)
  "Maximum total base64 payload from image blocks in one provider request.
The second gate, and the one that bites when the pictures are large: a handful
of recent screenshots is still megabytes, and providers cap the HTTP request
itself.  Both gates apply only to the request copy: the journal keeps the
original blocks, so the session file and a later replay are unaffected.")

(defun message-role (m) (pget m :role))
(defun message-content (m) (pget m :content))
(defun message-stop-reason (m) (pget m :stop-reason))
(defun message-usage (m) (pget m :usage))

(defun image-placeholder-block (block)
  "What an image degrades to for a model that cannot see it.  The name is
kept: the model should know something was there and that it is blind to it,
rather than reading a conversation with a hole in it."
  (list :type :text
        :text (format nil "[image not shown: ~a — the current model has no vision]"
                      (pget block :name "image"))))

(defun image-request-size-placeholder-block (block)
  "What an image degrades to when its bytes no longer fit the request body."
  (list :type :text
        :text (format nil "[image omitted to keep the request size under the limit: ~a]"
                      (pget block :name "image"))))

(defun image-stale-placeholder-block (block)
  "What an image degrades to once newer images have pushed it out of the
request window.  The name stays, and so does the way back: an agent that
needs the old picture again can simply read it again."
  (list :type :text
        :text (format nil (cat "[image not resent: ~a — only the ~d most recent "
                               "images stay in the request; read the file again to look]")
                      (pget block :name "image") *max-request-images*)))

(defun image-data-chars (block)
  (let ((data (and (eq (pget block :type) :image) (pget block :data))))
    (if (stringp data) (length data) 0)))

(defun enforce-image-request-budget (messages)
  "Keep the newest images as pixels and replace the rest with placeholders —
by count (*MAX-REQUEST-IMAGES*) and by bytes (*MAX-REQUEST-IMAGE-DATA-CHARS*),
whichever bites first.  This rewrites only the request copy.

The walk is newest-first, so `read` followed by the next model turn still
shows the picture the model just asked to inspect, and the oldest image is
always the one to go.  That direction is also what keeps the prompt cache
survivable: the placeholder swap moves *forward* through the transcript, so a
new screenshot invalidates the cached prefix from the image it evicts and
never from further back."
  (let ((left *max-request-image-data-chars*)
        (slots *max-request-images*))
    (nreverse
     (loop for message in (reverse messages)
           collect
           (if (find :image (message-content message) :key (lambda (b) (pget b :type)))
               (pput message :content
                     (nreverse
                      (loop for block in (reverse (message-content message))
                            collect
                            (let ((n (image-data-chars block)))
                              (cond ((zerop n) block)
                                    ((not (plusp slots))
                                     (image-stale-placeholder-block block))
                                    ((<= n left)
                                     (decf left n)
                                     (decf slots)
                                     block)
                                    (t (image-request-size-placeholder-block block)))))))
               message)))))

(defun degrade-images (content)
  (mapcar (lambda (block)
            (if (eq (pget block :type) :image) (image-placeholder-block block) block))
          content))

(defun handoff-pass (messages target-model-id &key (vision t))
  "Rewrite MESSAGES for a request to TARGET-MODEL-ID.  VISION nil degrades
image blocks to text, so a session that collected screenshots survives a
switch to a text-only model instead of failing every turn from then on."
  (let* ((messages (if vision
                       (enforce-image-request-budget messages)
                       (mapcar (lambda (m)
                                 (if (find :image (message-content m) :key (lambda (b) (pget b :type)))
                                     (pput m :content (degrade-images (message-content m)))
                                     m))
                               messages)))
         (live (remove-if (lambda (m)
                            (and (eq (message-role m) :assistant)
                                 (member (message-stop-reason m) '(:error :aborted))))
                          messages))
         (result-ids (loop for m in live
                           when (eq (message-role m) :tool-result)
                             collect (pget m :tool-call-id))))
    (loop for m in live
          append
          (case (message-role m)
            (:assistant
             (let* ((same-model (equal (pget m :model) target-model-id))
                    (content (remove-if
                              (lambda (block)
                                (and (eq (pget block :type) :thinking)
                                     (not same-model)))
                              (message-content m)))
                    (orphans (loop for block in content
                                   when (and (eq (pget block :type) :tool-call)
                                             (not (member (pget block :id) result-ids
                                                          :test #'equal)))
                                     collect block)))
               (if (null content)
                   nil
                   (cons (pput m :content content)
                         (loop for call in orphans
                               collect (list :role :tool-result
                                             :tool-call-id (pget call :id)
                                             :tool-name (pget call :name)
                                             :is-error t
                                             :content (list (list :type :text
                                                                  :text "Tool execution was interrupted before completion; the call did not run. Re-issue it if still needed."))))))))
            (t (list m))))))

(defun usage-total-tokens (usage)
  (+ (pget usage :input 0) (pget usage :output 0)
     (pget usage :cache-read 0) (pget usage :cache-write 0)))

;;; SSE transport: the framing loop shared by every SSE-based API.

(defvar *sse-progress* nil
  "Called by MAP-SSE-EVENTS with :ALIVE or :CONTENT while a stream is read.
Bound by PERFORM-REQUEST on the request thread, so the stall watchdog is fed
by the one framing loop every SSE adapter already shares — no API has to
thread a callback of its own down through PARSE-STREAM.")

(defun note-sse-progress (kind)
  "Report stream progress to whoever is watching: :ALIVE means bytes arrived,
:CONTENT means the response actually advanced.  A keepalive is the whole
reason the two are not the same thing."
  (let ((fn *sse-progress*))
    (when fn (funcall fn kind))))

(defun sse-keepalive-p (event-type)
  "Is this SSE event a heartbeat rather than a piece of the response?"
  (and event-type (string-equal event-type "ping")))

(defun map-sse-events (char-stream dispatch &key abort-flag)
  "Drive the SSE framing loop: event:/data: accumulation, CR trimming,
dispatch on blank lines (multi-line data joined with newlines), final
flush at EOF.  DISPATCH is called as (event-type-or-nil data-string);
return :stop to end the stream.  Returns :aborted when ABORT-FLAG fires,
else :done.

Every line read reports :ALIVE progress and every non-keepalive event reports
:CONTENT (see NOTE-SSE-PROGRESS): the transport above uses the first to tell a
dead connection from a live one, and the second to tell a live connection from
one that is only breathing."
  (let ((event-type nil) (data-lines nil))
    (flet ((flush ()
             (when data-lines
               (let ((type event-type)
                     (data (string-join (string #\Newline) (nreverse data-lines))))
                 (setf event-type nil data-lines nil)
                 (unless (sse-keepalive-p type) (note-sse-progress :content))
                 (funcall dispatch type data)))))
      (loop for line = (read-line char-stream nil :eof)
            do (when (and abort-flag (funcall abort-flag))
                 (return :aborted))
               (when (eq line :eof)
                 (flush)
                 (return :done))
               (note-sse-progress :alive)
               (let ((line (string-right-trim '(#\Return) line)))
                 (cond ((zerop (length line))
                        (when (eq (flush) :stop) (return :done)))
                       ((string-prefix-p "event:" line)
                        (setf event-type (string-trim " " (subseq line 6))))
                       ((string-prefix-p "data:" line)
                        (push (string-trim " " (subseq line 5)) data-lines))
                       (t nil)))))))

;;; HTTP + retry: in-request retry honoring retry-after, exponential
;;; backoff with jitter; classification on HTTP status, not string matching.

(defun retryable-status-p (status)
  (or (member status '(408 409 429 425 529))
      (and status (<= 500 status 599))))

(defun retry-delay (attempt retry-after)
  "Seconds to sleep before retry ATTEMPT (0-based).  NIL = don't retry."
  (cond ((and retry-after (> retry-after 60)) nil) ; refuse silently-long delays
        (retry-after retry-after)
        (t (min 30 (+ (expt 2 attempt) (random 1.0))))))

(defun parse-retry-after (headers)
  (let ((v (and (hash-table-p headers) (gethash "retry-after" headers))))
    (typecase v
      (string (ignore-errors (parse-integer v :junk-allowed t)))
      (number v)
      (t nil))))

(defparameter *max-attempts* 4)

;;; Deadlines.  The socket read timeout below is SO_RCVTIMEO — it fires per
;;; read, so it cannot see a request that was never answered at all, and a
;;; stream that only carries keepalives resets it forever.  These two are the
;;; deadlines that matter, and both are measured in *progress*, not in wall
;;; clock: a model that legitimately streams for twenty minutes must not be
;;; killed for taking twenty minutes.

(defparameter *request-stall-timeout* 120
  "Seconds a request may go without a single byte of response before evo
gives up on the attempt and retries it.  The window covers connect, the
upload of the whole request body, the provider's prefill and the first SSE
frame — everything that can silently never happen.  Generous, because the
body can be megabytes on a bad link, but finite, because a request that has
said nothing for two minutes is a hang, and the observed one lasted nine.")

(defparameter *request-idle-timeout* 600
  "Seconds a *live* stream may go without producing any content before evo
gives up on the attempt.  This is the ping-only zombie: bytes keep arriving,
the read timeout never fires, and nothing is ever said.  Much longer than the
stall timeout, because a long prefill behind a chatty keepalive is normal.")

;;; Default transport: streamed SSE POST over dexador, parsed by the API's
;;; parse-stream.  Lives here (not api.lisp) because it owns the HTTP/proxy
;;; helpers above.

(define-condition provider-request-cancelled (serious-condition) ())

(defstruct (provider-request-task (:conc-name request-task-))
  "One HTTP request owned by its request thread.  The caller may request
cancellation, but only the owner thread opens, parses and closes the stream.
Every slot here is read or written under LOCK.

THREAD is published by the owner itself as its first act, not by the thread's
creator: MAKE-THREAD returns to the caller and the new thread starts in an
unspecified order, so a caller-side assignment leaves a window in which the
request is running but has no handle to interrupt.

ALIVE-TIME and CONTENT-TIME are the watchdog's two clocks, written by the
owner as it makes progress and read by the caller, which is the only thread
that may decide the request has stalled."
  (lock (bt:make-lock "evo-provider-request"))
  thread
  (state :starting)                    ; :starting, :streaming, :finished
  (cancel-requested-p nil)
  (interrupted-p nil)                  ; the one interrupt has been delivered
  (alive-time (get-internal-real-time))
  (content-time (get-internal-real-time))
  result
  error)

(defun request-task-finished-p (task)
  (bt:with-lock-held ((request-task-lock task))
    (eq (request-task-state task) :finished)))

(defun note-request-progress (task kind)
  "Restart the watchdog's clocks: KIND :ALIVE means the connection produced
something, :CONTENT means the response itself moved on."
  (bt:with-lock-held ((request-task-lock task))
    (let ((now (get-internal-real-time)))
      (setf (request-task-alive-time task) now)
      (when (eq kind :content)
        (setf (request-task-content-time task) now)))))

(defun request-task-stall-reason (task)
  "Why this request should be given up on now, as a message — or NIL while it
is making progress.  Called only by the caller's watchdog loop."
  (bt:with-lock-held ((request-task-lock task))
    (unless (eq (request-task-state task) :finished)
      (let ((now (get-internal-real-time)))
        (flet ((silent-since (then)
                 (/ (- now then) internal-time-units-per-second)))
          (cond ((and *request-stall-timeout*
                      (> (silent-since (request-task-alive-time task))
                         *request-stall-timeout*))
                 (format nil "No response from the provider for ~ds — giving up on this attempt"
                         (round *request-stall-timeout*)))
                ((and *request-idle-timeout*
                      (> (silent-since (request-task-content-time task))
                         *request-idle-timeout*))
                 (format nil "Stream open but silent for ~ds — giving up on this attempt"
                         (round *request-idle-timeout*)))))))))

(defun request-task-outcome (task)
  (bt:with-lock-held ((request-task-lock task))
    (values (request-task-result task) (request-task-error task))))

(defun cancel-request-task (task)
  "Ask TASK to stop by interrupting its owner thread with a private condition.
The condition unwinds Dexador on that same thread, so connection and stream
cleanup remain owner-local; no other thread ever closes the stream.

At most ONE interrupt is ever delivered (INTERRUPTED-P), so the owner's
publish cleanup needs to survive at most one late-landing condition.  But the
delivery *retries* across calls until the owner has published its handle: the
first cancel can arrive in the instant before the thread's first form runs,
and giving up then would leave the request uncancellable."
  (let (thread)
    (bt:with-lock-held ((request-task-lock task))
      (setf (request-task-cancel-requested-p task) t)
      (unless (or (request-task-interrupted-p task)
                  (eq (request-task-state task) :finished))
        (setf thread (request-task-thread task))
        (when thread (setf (request-task-interrupted-p task) t))))
    (when thread
      (bt:interrupt-thread
       thread (lambda () (error 'provider-request-cancelled))))
    t))

(defun request-task-cancelled-p (task)
  (bt:with-lock-held ((request-task-lock task))
    (request-task-cancel-requested-p task)))

(defmethod perform-request ((api provider-api) url headers body
                            &key on-event abort-flag &allow-other-keys)
  (labels ((aborted-result ()
             (list :aborted-p t :content nil :stop-reason :aborted
                   :usage (list :input 0 :output 0
                                :cache-read 0 :cache-write 0))))
    (let ((task (make-provider-request-task)))
      (labels ((emit (event)
                 ;; An adapter that emits without going through MAP-SSE-EVENTS
                 ;; still counts as progress: the watchdog must never be
                 ;; blinded by a non-SSE framing.
                 (note-request-progress task :content)
                 (unless (request-task-cancelled-p task)
                   (when on-event (funcall on-event event))))
               (request-body ()
                 ;; This thread is the sole owner of the Dexador stream.  Even
                 ;; cancellation runs here, by interrupting this thread with a
                 ;; private condition; the unwind closes STREAM before the task
                 ;; becomes :finished and before the caller can return.
                 (let ((stream nil)
                       (result nil)
                       (failure nil))
                   (labels ((publish ()
                              ;; The one thing that must happen on EVERY exit:
                              ;; close the stream and mark the task finished —
                              ;; the caller's wait loop terminates on nothing
                              ;; else.  NB. IGNORE-ERRORS does not catch the
                              ;; cancel condition (a SERIOUS-CONDITION, not an
                              ;; ERROR), hence the explicit handler.
                              (ignore-errors (when stream (close stream)))
                              (bt:with-lock-held ((request-task-lock task))
                                (setf (request-task-result task)
                                      (or result
                                          (and (request-task-cancel-requested-p task)
                                               (aborted-result)))
                                      (request-task-error task) failure
                                      (request-task-state task) :finished))))
                     (unwind-protect
                          (handler-case
                              (setf result
                                    (progn
                                      ;; Publish the handle inside the protected
                                      ;; form: from here on a cancel can find a
                                      ;; thread to interrupt, and the interrupt
                                      ;; can land no earlier than the cleanup
                                      ;; below is armed.
                                      (bt:with-lock-held ((request-task-lock task))
                                        (setf (request-task-thread task)
                                              (bt:current-thread)))
                                      (when (request-task-cancelled-p task)
                                        ;; The unwind cleanup publishes.
                                        (return-from request-body))
                                      (with-proxy (proxy url)
                                        (setf stream
                                              (apply #'dex:post url
                                                     :headers headers :content body
                                                     :want-stream t :force-binary t
                                                     :keep-alive nil
                                                     :connect-timeout 15
                                                     :read-timeout 600
                                                     (when proxy (list :proxy proxy)))))
                                      (bt:with-lock-held ((request-task-lock task))
                                        (setf (request-task-state task) :streaming))
                                      ;; Response headers are back: the upload
                                      ;; and the prefill are behind us, so the
                                      ;; stall clock starts again from here.
                                      (note-request-progress task :alive)
                                      (if (request-task-cancelled-p task)
                                          (aborted-result)
                                          (let ((*sse-progress*
                                                  (lambda (kind)
                                                    (note-request-progress task kind))))
                                            (parse-stream
                                             api
                                             (flexi-streams:make-flexi-stream
                                              stream :external-format :utf-8)
                                             :on-event #'emit
                                             :abort-flag
                                             (lambda ()
                                               (request-task-cancelled-p task)))))))
                            (provider-request-cancelled ()
                              (setf result (aborted-result)))
                            (serious-condition (e)
                              (if (request-task-cancelled-p task)
                                  (setf result (aborted-result))
                                  (setf failure e))))
                       ;; The single cancel interrupt can land HERE, after the
                       ;; body's handlers have unwound; uncaught it would kill
                       ;; the thread without publishing, and the caller would
                       ;; wait forever.  One handler suffices because at most
                       ;; one interrupt is ever delivered.
                       (handler-case (publish)
                         (provider-request-cancelled ()
                           (setf result (or result (aborted-result)))
                           (publish))))))))
        ;; JOIN uses this handle, never the task slot: the slot exists so a
        ;; canceller can interrupt the owner, and may still be unset in the
        ;; instant before the thread runs its first form.
        (let ((thread (bt:make-thread #'request-body :name "evo-provider-request"))
              (stalled nil))
          (unwind-protect
               (progn
                 ;; The watchdog lives here, in the caller, because cancelling
                 ;; is the caller's move: the owner is the thread that is stuck.
                 ;; Cancellation is re-requested every pass, like the abort path
                 ;; — the first request can land before the owner has published
                 ;; a thread to interrupt, and giving up then would hang here.
                 (loop until (request-task-finished-p task)
                       do (unless stalled
                            (setf stalled (request-task-stall-reason task)))
                          (when (or stalled (and abort-flag (funcall abort-flag)))
                            (cancel-request-task task))
                          (sleep 0.02))
                 (multiple-value-bind (result failure) (request-task-outcome task)
                   (when failure (error failure))
                   ;; A stall is the caller's verdict on its own request, so the
                   ;; caller rewrites its own return value: an error the retry
                   ;; loop can act on, never the :aborted the user alone means.
                   (if (and stalled (not (and abort-flag (funcall abort-flag))))
                       (list :error-message stalled)
                       result)))
            ;; A request may never outlive the call that created it.  Joining is
            ;; unconditional, including cancellation and non-local exits.
            (unless (request-task-finished-p task)
              (cancel-request-task task))
            (ignore-errors (bt:join-thread thread))))))))

(defun abortible-sleep (seconds abort-flag)
  "Sleep up to SECONDS, waking quickly when ABORT-FLAG becomes true."
  (loop with deadline = (+ (get-internal-real-time)
                           (* seconds internal-time-units-per-second))
        until (or (and abort-flag (funcall abort-flag))
                  (>= (get-internal-real-time) deadline))
        do (sleep (min 0.05 (/ (max 0 (- deadline (get-internal-real-time)))
                               internal-time-units-per-second)))))

(defun call-provider (&key model system messages tools thinking-level
                           on-event abort-flag abort-cleanup)
  "Make one streamed request.  ALWAYS returns an assistant message plist —
errors come back as data with :stop-reason :error/:aborted, never signals.
The model's :api tag names the wire API; its :provider names the endpoint
config.  Prompt caching is the adapter's business (cache_control
breakpoints on Anthropic Messages)."
  (let* ((model (if (consp model) model (find-model model)))
         (api (find-api (pget model :api)))
         (config (provider-config (pget model :provider)))
         (url (concatenate 'string
                           (string-right-trim "/" (pget config :base-url))
                           (endpoint-path api)))
         (headers (cons '("content-type" . "application/json")
                        (auth-headers api config)))
         (body (build-request api :model model :system system
                              :messages messages :tools tools
                              :thinking-level thinking-level))
         (last-error nil))
    (flet ((finish (result)
             (let ((usage (pget result :usage)))
               (append (list :role :assistant
                             :api (pget model :api)
                             :provider (pget model :provider)
                             :model (pget model :id)
                             :stop-reason (pget result :stop-reason)
                             :usage usage
                             :content (pget result :content))
                       (when (pget result :error-message)
                         (list :error-message (pget result :error-message))))))
           (error-message (text retryable)
             (list :role :assistant
                   :api (pget model :api) :provider (pget model :provider)
                   :model (pget model :id)
                   :stop-reason :error :error-message text
                   :retryable (and retryable t)
                   :usage (list :input 0 :output 0 :cache-read 0 :cache-write 0)
                   :content nil))
           (aborted-message (&optional usage)
             (list :role :assistant
                   :api (pget model :api) :provider (pget model :provider)
                   :model (pget model :id)
                   :stop-reason :aborted
                   :usage (or usage (list :input 0 :output 0
                                          :cache-read 0 :cache-write 0))
                   :content nil)))
      (dotimes (attempt *max-attempts*
                        (error-message (format nil "Provider request failed after ~d attempts: ~a"
                                               *max-attempts* last-error)
                                       t))
        (let ((outcome
                (handler-case
                    (let ((result (perform-request api url headers body
                                                   :on-event on-event
                                                   :abort-flag abort-flag
                                                   :abort-cleanup abort-cleanup)))
                      (cond
                        ((pget result :aborted-p)
                         (list :done (aborted-message (pget result :usage))))
                        ((pget result :error-message)
                         (list :retry (pget result :error-message) nil))
                        ((not (pget result :stopped-p))
                         (list :retry "Stream ended without a terminal event (truncated response)" nil))
                        (t (list :done (finish result)))))
                  (dexador.error:http-request-failed (e)
                    (let* ((status (dexador.error:response-status e))
                           (body (ignore-errors
                                  (let ((b (dexador.error:response-body e)))
                                    (if (streamp b)
                                        (ignore-errors
                                         (let ((fb (flexi-streams:make-flexi-stream
                                                    b :external-format :utf-8)))
                                           (with-output-to-string (s)
                                             (loop for line = (read-line fb nil)
                                                   while line do (write-line line s)))))
                                        (format nil "~a" b)))))
                           (text (format nil "HTTP ~a: ~a" status
                                         (truncate-string (or body "") 2000))))
                      (if (retryable-status-p status)
                          (list :retry text (parse-retry-after (dexador.error:response-headers e)))
                          (list :done (error-message text nil)))))
                  (provider-error (e)
                    (list :done (error-message (provider-error-message e) nil)))
                  (error (e)
                    (list :retry (format nil "~a: ~a" (type-of e) e) nil)))))
          (ecase (first outcome)
            (:done (return (second outcome)))
            (:retry
             (setf last-error (second outcome))
             (when (and abort-flag (funcall abort-flag))
               (return (aborted-message)))
             (let ((delay (retry-delay attempt (third outcome))))
               (cond ((null delay)
                      (return (error-message last-error t)))
                     ((< attempt (1- *max-attempts*))
                      ;; Say so.  A silent retry re-sends the whole request —
                      ;; megabytes, sometimes minutes — behind a spinner that
                      ;; looks exactly like progress, and the user is left
                      ;; guessing whether evo is working or wedged.
                      (when on-event
                        (funcall on-event (list :type :provider-retry
                                                :attempt (1+ attempt)
                                                :max *max-attempts*
                                                :delay delay
                                                :reason last-error)))
                      (abortible-sleep delay abort-flag)
                      (when (and abort-flag (funcall abort-flag))
                        (return (aborted-message)))))))))))))
