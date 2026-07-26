;;;; compact.lisp — context compaction.
;;;;
;;;; Trigger: estimated context tokens > context-window - reserve (defaults:
;;;; reserve 16k, keep-recent 20k), plus overflow-error recovery and manual
;;;; /compact.  Token accounting is anchored on the last provider-reported
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
  "chars/4; images flat ~4800."
  (let ((chars 0) (images 0))
    (dolist (block (message-content message))
      (case (pget block :type)
        (:text (incf chars (length (or (pget block :text) ""))))
        (:thinking (incf chars (length (or (pget block :thinking) ""))))
        (:tool-call (incf chars (length (format nil "~s" (pget block :arguments)))))
        (:image (incf images))))
    (+ (ceiling chars 4) (* images 4800))))

(defun estimate-context-tokens (messages)
  "Anchor on the last assistant message with provider-reported usage; only
the tail after it is estimated."
  (let ((anchor-index nil) (anchor-tokens 0))
    (loop for m in messages
          for i from 0
          when (and (eq (message-role m) :assistant)
                    (message-usage m)
                    (plusp (usage-total-tokens (message-usage m))))
            do (setf anchor-index i
                     anchor-tokens (usage-total-tokens (message-usage m))))
    (+ anchor-tokens
       (loop for m in messages
             for i from 0
             when (or (null anchor-index) (> i anchor-index))
               sum (estimate-message-tokens m)))))

(defun compaction-needed-p (state model)
  (let ((messages (evo.journal:state-messages state)))
    (and messages
         (> (estimate-context-tokens messages)
            (- (model-context-window model)
               (setting :compact-reserve *compact-reserve-tokens*)))
         ;; A compaction that would drop nothing is not a compaction.
         (plusp (select-cut messages)))))

;;; Cut-point selection: retain a recent tail worth ~keep-recent tokens,
;;; then extend backwards so the tail never starts at a tool result.

(defun select-cut (messages)
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
    (max 0 cut)))

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
         (format out "[tool-result~:[~; ERROR~]] ~a~%"
                 (pget m :is-error)
                 (truncate-string
                  (or (pget (first (message-content m)) :text) "") 1500)))))))

(defun summarize (model thinking messages previous-summary hint)
  "One summarization call.  Returns the summary text or signals."
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
                                :thinking-level thinking)))
    (when (eq (message-stop-reason result) :error)
      (error "Summarization failed: ~a" (pget result :error-message)))
    (let ((text (pget (find :text (message-content result)
                            :key (lambda (b) (pget b :type)))
                      :text)))
      (unless (and text (plusp (length text)))
        (error "Summarization returned no text"))
      text)))

(defun previous-compaction (journal)
  (find :compaction (entry-path journal) :from-end t
        :key (lambda (e) (pget e :type))))

(defun compact-now (agent &key hint)
  "Manual or automatic compaction: summarize everything before the cut,
retain the tail on the :compaction entry.  Returns the entry."
  (let* ((journal (agent-journal agent))
         (state (fold-state journal))
         (messages (evo.journal:state-messages state))
         (model (find-model (or (evo.journal:state-model state)
                                (agent-model-override agent)
                                (setting :model "claude-sonnet-5"))))
         (cut (select-cut messages))
         (dropped (subseq messages 0 cut))
         (tail (subseq messages cut))
         (previous (previous-compaction journal))
         (summary (progn
                    (unless dropped
                      (error "Nothing to compact: the whole context is within the keep-recent tail"))
                    (summarize model
                               (or (evo.journal:state-thinking state) :low)
                               dropped
                               (and previous (pget previous :summary))
                               (and (plusp (length (or hint ""))) hint)))))
    (multiple-value-bind (read-files modified) (collect-file-sets dropped)
      (let ((all-read (union (coerce (or (and previous (pget previous :files-read)) #()) 'list)
                             read-files :test #'equal))
            (all-modified (union (coerce (or (and previous (pget previous :files-modified)) #()) 'list)
                                 modified :test #'equal)))
        (append-entry journal
                      (list :type :compaction
                            :summary summary
                            :retained-tail (coerce tail 'vector)
                            :files-read (coerce all-read 'vector)
                            :files-modified (coerce all-modified 'vector)
                            :dropped-messages (length dropped)))))))

(defun overflow-error-p (message)
  "Context-overflow classification for compact+retry-once recovery."
  (let ((text (or (pget message :error-message) "")))
    (and (eq (message-stop-reason message) :error)
         (or (search "prompt is too long" text)
             (search "too many tokens" text)
             (search "context length" text)
             (search "maximum context" text)
             (search "context_length" text)
             (search "exceeds the context window" text)))))
