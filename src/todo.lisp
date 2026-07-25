;;;; todo.lisp — todo checklists, a core extension (D14, §11.1).
;;;;
;;;; The model replaces the whole list per call; items = text + status.
;;;; State rides :custom entries (invisible to the LLM as entries — the tool
;;;; call/result already put it in context when it mattered), so the current
;;;; list = fold over the path: it survives restart and compaction untouched.

(in-package :evo.todo)

(defparameter *statuses* '(:pending :in-progress :done))

(defun current-todos (agent)
  "Current checklist for AGENT: vector of (:text s :status kw), or nil."
  (custom-state (fold-state (agent-journal agent)) "todo"))

(defun status-glyph (status)
  (case status (:done "☑") (:in-progress "◐") (t "☐")))

(defun format-todos (todos &key (indent ""))
  (with-output-to-string (out)
    (loop for item across (or todos #())
          do (format out "~a~a ~a~%" indent
                     (status-glyph (pget item :status))
                     (pget item :text)))))

(defun parse-status (s)
  (let ((kw (cond ((equal s "pending") :pending)
                  ((equal s "in-progress") :in-progress)
                  ((equal s "in_progress") :in-progress)
                  ((equal s "done") :done))))
    (or kw (error "Invalid status ~s (use pending / in-progress / done)" s))))

(defun tool-todo (args)
  (let ((items (pget args :items)))
    (unless (vectorp items)
      (error "items must be an array"))
    (let ((todos (map 'vector
                      (lambda (item)
                        (let ((text (pget item :text))
                              (status (pget item :status)))
                          (unless (and (stringp text) (plusp (length text)))
                            (error "Every item needs non-empty text"))
                          (list :text text :status (parse-status status))))
                      items)))
      (append-entry (agent-journal evo:*agent*)
                    (list :type :custom :key "todo" :data todos))
      (run-hooks :todo-changed (list :todos todos))
      (format nil "Todo list updated: ~d item~:p (~d done, ~d in progress).~%~a"
              (length todos)
              (count :done todos :key (lambda (i) (pget i :status)))
              (count :in-progress todos :key (lambda (i) (pget i :status)))
              (format-todos todos)))))

(evo:register-tool "todo"
  :description "Replace your whole todo checklist (shown to the user as your visible plan). Use it for multi-step work: set the list up front, keep exactly one item in-progress, mark items done as you finish them, and update it whenever the plan changes."
  :schema '(:object
            (:items :type :array
             :description "The complete new checklist (replaces the old one)"
             :items (:type :object
                     :properties ((:text :type :string :description "The task")
                                  (:status :type :string
                                   :enum ("pending" "in-progress" "done")
                                   :description "Task state")))))
  :execute #'tool-todo)
