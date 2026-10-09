;;;; compact.lisp — context compaction.
;;;;
;;;; Trigger: estimated context tokens > context-window - reserve (defaults:
;;;; reserve 16k, keep-recent 20k), or — for a model registered with
;;;; :max-input-items — input items > that cap - item reserve (default 64),
;;;; plus overflow-error recovery and manual /compact.  Token accounting is anchored on the last provider-reported
;;;; usage; only the tail after it is estimated (chars/4).  Cut points are
;;;; never at a tool result.  The result is a :compaction entry carrying the
;;;; summary AND the retained tail materialized on the entry — a
;;;; self-contained checkpoint: context rebuild is
;;;; [summary, ...retained-tail, ...entries-after], O(1), no walk past it.
;;;; Summaries reach the model as ordinary user messages in <summary> tags.

(in-package :evo.kernel)

(defparameter *compact-reserve-tokens* 16000)
(defparameter *compact-keep-recent-tokens* 20000)

(defun estimate-message-tokens (message)
  "chars/4; an image flat at *IMAGE-BLOCK-TOKENS*."
  (let ((chars 0) (images 0))
    (dolist (block (message-content message))
      (case (pget block :type)
        (:text (incf chars (length (or (pget block :text) ""))))
        (:thinking (incf chars (length (or (pget block :thinking) ""))))
        ;; An opaque reasoning slot is replayed on the wire verbatim, so it
        ;; costs context like anything else that is sent.
        (:redacted-thinking (incf chars (length (or (pget block :data) ""))))
        (:tool-call (incf chars (length (format nil "~s" (pget block :arguments)))))
        (:image (incf images))))
    (+ (ceiling chars 4) (* images *image-block-tokens*))))

(defun estimate-context-tokens (messages)
  "Anchor on the last message with provider-reported usage; only
the tail after it is estimated."
  (let ((anchor-index nil) (anchor-tokens 0))
    (loop for m in messages
          for i from 0
          when (and (message-usage m)
                    (plusp (usage-total-tokens (message-usage m))))
            do (setf anchor-index i
                     anchor-tokens (usage-total-tokens (message-usage m))))
    (+ anchor-tokens
       (loop for m in messages
             for i from 0
             when (or (null anchor-index) (> i anchor-index))
               sum (estimate-message-tokens m)))))

;;; Input items.  Some endpoints cap the number of input items per request
;;; (Ark behind aiden: 1000, whatever the token count), and a long session
;;; of small tool calls reaches that far below its token window.  The count
;;; mirrors a Responses-style input array: one item for the system prompt,
;;; one per user message and per tool result, and per assistant message one
;;; per reasoning block, one per tool call and one for its text (one if the
;;; message is empty).  Measured against four sessions the Ark gateway
;;; rejected, this runs a few items ABOVE the gateway's own count — the safe
;;; side.

(defparameter *compact-input-item-reserve* 64)

(defun count-message-items (message)
  (let ((blocks (message-content message)))
    (if (eq (message-role message) :assistant)
        (if (null blocks)
            1
            (flet ((n (type) (count type blocks :key (lambda (b) (pget b :type)))))
              (+ (n :thinking) (n :tool-call)
                 (if (or (eq (pget message :api) :openai-responses)
                         (some (lambda (b) (pget b :responses-item-json)) blocks))
                     (n :text)
                     (min 1 (n :text))))))
        1)))

(defun count-input-items (messages)
  "Estimated provider input items for a request carrying MESSAGES."
  (1+ (loop for m in messages sum (count-message-items m))))

(defun input-item-threshold (model)
  "Compact above this many items, or NIL when MODEL has no item cap.  The
reserve never eats more than half the cap."
  (let ((cap (model-max-input-items model)))
    (and cap
         (max (ceiling cap 2)
              (- cap (setting :compact-input-item-reserve
                              *compact-input-item-reserve*))))))

(defun tail-item-cap (model)
  "Most items a compaction retains verbatim for MODEL, or NIL (no cap)."
  (let ((cap (model-max-input-items model)))
    (and cap (max 1 (floor cap 3)))))

(defun compaction-needed-p (state model)
  (let ((messages (evo.journal:state-messages state))
        (item-threshold (input-item-threshold model)))
    (and messages
         (or (> (estimate-context-tokens messages)
                (- (model-context-window model)
                   (setting :compact-reserve *compact-reserve-tokens*)))
             (and item-threshold
                  (> (count-input-items messages) item-threshold)))
         ;; A compaction that would drop nothing is not a compaction.
         (plusp (select-cut messages :max-items (tail-item-cap model))))))

;;; Cut-point selection: retain a recent tail worth ~keep-recent tokens,
;;; then extend backwards so the tail never starts at a tool result.  With
;;; MAX-ITEMS the tail is then trimmed from the front to at most that many
;;; input items — a run of tiny tool calls can fit hundreds of items in the
;;; keep-recent tokens — again never starting at a tool result.

(defun select-cut (messages &key max-items)
  "Index of the first retained message."
  (let ((cut (length messages))
        (acc 0)
        (keep (setting :compact-keep-recent *compact-keep-recent-tokens*)))
    (loop for i from (1- (length messages)) downto 0
          do (incf acc (estimate-message-tokens (nth i messages)))
             (setf cut i)
          while (< acc keep))
    ;; Never start the tail at a tool result (its call must stay adjacent).
    (loop while (and (< cut (length messages))
                     (eq (message-role (nth cut messages)) :tool-result))
          do (decf cut))
    (setf cut (max 0 cut))
    (when max-items
      (let ((items (loop for m in (nthcdr cut messages)
                         sum (count-message-items m))))
        (loop while (and (< cut (length messages)) (> items max-items))
              do (decf items (count-message-items (nth cut messages)))
                 (incf cut))
        ;; Moving forward: skip results whose call was just dropped.
        (loop while (and (< cut (length messages))
                         (eq (message-role (nth cut messages)) :tool-result))
              do (incf cut))))
    cut))

;;; Deterministic facts: read/modified file sets accumulate across
;;; compactions.

(defun collect-file-sets (messages)
  (let ((read-files nil) (modified nil))
    (dolist (m messages)
      (when (eq (message-role m) :assistant)
        (dolist (block (message-content m))
          (when (eq (pget block :type) :tool-call)
            (let ((path (pget (pget block :arguments) :path)))
              (when (stringp path)
                (cond ((equal (pget block :name) "read")
                       (pushnew path read-files :test #'equal))
                      ((member (pget block :name) '("write" "edit") :test #'equal)
                       (pushnew path modified :test #'equal)))))))))
    (values (nreverse read-files) (nreverse modified))))

;;; Summarization prompts: structured summary; a separate iterative
;;; UPDATE prompt fed the previous summary.

(defparameter *summary-structure*
  "Structure the summary EXACTLY as:
## Goal
## Constraints
## Progress
## Key Decisions
## Next Steps
## Critical Context
Preserve exact file paths, function/symbol names, shell commands, and error
messages verbatim — they must survive the summary.")

(defun render-transcript (messages)
  (with-output-to-string (out)
    (dolist (m messages)
      (case (message-role m)
        (:user (format out "[user] ~a~%"
                       (or (pget (find :text (message-content m)
                                       :key (lambda (b) (pget b :type))) :text) "")))
        (:assistant
         (dolist (block (message-content m))
           (case (pget block :type)
             (:text (format out "[assistant] ~a~%" (pget block :text)))
             (:tool-call (format out "[tool-call ~a] ~s~%"
                                 (pget block :name) (pget block :arguments))))))
        (:tool-result
         ;; A result may carry blocks the summarizer cannot read (an image);
         ;; RESULT-DISPLAY-TEXT names those instead of dropping them, so the
         ;; summary records that a picture was looked at.
         (format out "[tool-result~:[~; ERROR~]] ~a~%"
                 (pget m :is-error)
                 (truncate-string (result-display-text (message-content m)) 1500)))))))

(defun summarize (model thinking messages previous-summary hint &key abort-flag abort-cleanup)
  "One summarization call.  Returns the summary text and the provider usage."
  (let* ((instruction
           (if previous-summary
               (format nil "Below is the running summary of an agent session so far, followed by the next chunk of transcript. UPDATE the summary to incorporate the new events. ~a~@[~%Extra focus requested by the user: ~a~]~2%<previous-summary>~%~a~%</previous-summary>"
                       *summary-structure* hint previous-summary)
               (format nil "Summarize this agent session transcript for seamless continuation in a fresh context. ~a~@[~%Extra focus requested by the user: ~a~]"
                       *summary-structure* hint)))
         (message (list :role :user
                        :content (list (list :type :text
                                             :text (format nil "~a~2%<transcript>~%~a</transcript>"
                                                           instruction
                                                           (render-transcript messages))))))
         (result (call-provider :model model
                                :system "You are a precise summarizer of agent work sessions."
                                :messages (list message)
                                :thinking-level thinking
                                :abort-flag abort-flag
                                :abort-cleanup abort-cleanup)))
    (when (eq (message-stop-reason result) :error)
      (error "Summarization failed: ~a" (pget result :error-message)))
    ;; Every :text block, in order — a response can carry an empty leading
    ;; text block (GPT over anthropic-messages does), and the summary may be
    ;; split across blocks with thinking/tool blocks between.
    (let ((text (with-output-to-string (out)
                  (dolist (block (message-content result))
                    (when (eq (pget block :type) :text)
                      (write-string (or (pget block :text) "") out))))))
      (unless (plusp (length text))
        (error "Summarization returned no text"))
      (values text (message-usage result)))))

(defun previous-compaction (journal)
  (find :compaction (entry-path journal) :from-end t
        :key (lambda (e) (pget e :type))))

(defun compact-now (agent &key hint manual)
  "Manual or automatic compaction: summarize everything before the cut,
retain the tail on the :compaction entry.  MANUAL says a person asked for it
(/compact, context.compact) rather than the threshold firing; the entry records
it with the context size it came from and went to.  Returns the entry."
  (let* ((journal (agent-journal agent))
         (state (fold-state journal))
         (messages (evo.journal:state-messages state))
         (tokens-before (estimate-context-tokens messages))
         (model (effective-model state agent))
         (cut (select-cut messages :max-items (tail-item-cap model)))
         (dropped (subseq messages 0 cut))
         (tail (subseq messages cut))
         (previous (previous-compaction journal))
         (summary nil)
         (summary-tokens 0))
    (unless dropped
      (error "Nothing to compact: the whole context is within the keep-recent tail"))
    (multiple-value-bind (text usage)
        (summarize model
                   (or (normalize-thinking-level (evo.journal:state-thinking state))
                       :low)
                   dropped
                   (and previous (pget previous :summary))
                   (and (plusp (length (or hint ""))) hint)
                   :abort-flag (lambda () (agent-abort-flag agent))
                   :abort-cleanup (lambda (cleanup)
                                    (add-abort-cleanup agent cleanup)))
      (setf summary text
            summary-tokens (and usage (pget usage :output 0))))
    (multiple-value-bind (read-files modified) (collect-file-sets dropped)
      (let ((all-read (union (coerce (or (and previous (pget previous :files-read)) #()) 'list)
                             read-files :test #'equal))
            (all-modified (union (coerce (or (and previous (pget previous :files-modified)) #()) 'list)
                                 modified :test #'equal)))
        (let ((retained (coerce
                         (loop for m in tail
                               collect (let ((copy (copy-list m)))
                                         (remf copy :usage)
                                         copy))
                         'vector)))
          (append-entry journal
                        (list :type :compaction
                              :summary summary
                              :summary-tokens summary-tokens
                              :retained-tail retained
                              :files-read (coerce all-read 'vector)
                              :files-modified (coerce all-modified 'vector)
                              :dropped-messages (length dropped)
                              ;; What a reader needs: how big the context was,
                              ;; how big it is now, and whether a person asked.
                              :tokens-before tokens-before
                              :tokens-after (estimate-context-tokens
                                             (evo.journal:compaction-entry->messages
                                              (list :summary summary
                                                    :summary-tokens summary-tokens
                                                    :retained-tail retained)))
                              :manual (and manual t))))))))

(defun overflow-error-p (message)
  "Context-overflow classification for compact+retry-once recovery.  An
input-item cap counts: compacting is what brings the item count down."
  (let ((text (or (pget message :error-message) "")))
    (and (eq (message-stop-reason message) :error)
         (or (search "prompt is too long" text)
             (search "input items" text)
             ;; Ark: "单次请求最多支持 1000 个输入项" (at most 1000 input items).
             (search "个输入项" text)
             (search "too many tokens" text)
             (search "context length" text)
             (search "maximum context" text)
             (search "context_length" text)
             (search "exceeds the context window" text)))))
