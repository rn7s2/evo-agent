;;;; tools.lisp — tool registry + sexpr schema -> JSON Schema.
;;;;
;;;; Tool interface: name, description, sexpr schema, execute function,
;;;; :content (model-visible) vs :details (host-visible) in the result.
;;;; Execution is sequential.  Tools signal conditions; the loop
;;;; converts them to error tool-results.

(in-package :evo.kernel)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname :jzon :com.inuoe.jzon :evo.kernel))

(defstruct tool
  name          ; string
  description   ; string
  schema        ; sexpr schema (see below)
  execute-fn    ; (lambda (args-plist) ...) -> string, or (values content details)
  (source :builtin))

(defvar *tool-registry* (make-hash-table :test #'equal))
(defvar *registry-generation* 0
  "Bumped on every registry mutation; the loop rebuilds the system prompt when it changes.")

(defun register-tool* (&key name description schema execute (source :builtin))
  (check-type name string)
  (check-type execute function)
  (setf (gethash name *tool-registry*)
        (make-tool :name name :description description :schema schema
                   :execute-fn execute :source source))
  (incf *registry-generation*)
  name)

(defun find-tool (name)
  (gethash name *tool-registry*))

(defun all-tool-names ()
  (sort (loop for k being the hash-keys of *tool-registry* collect k) #'string<))

(defun active-tools (state)
  "Tools active for STATE (a journal fold): the :tools-change fold if present,
else every registered tool."
  (let ((names (or (evo.journal:state-tools state) (all-tool-names))))
    (loop for name in names
          for tool = (find-tool name)
          when tool collect tool)))

;;; Sexpr schema -> JSON Schema.
;;;
;;; Schema syntax:
;;;   (:object (name :type <t> :description "..." [:optional t] [:enum (..)]
;;;                  [:items <schema>] [:properties (...)]) ...)
;;; where <t> is :string :integer :number :boolean :object :array.
;;; Property names are keywords; `-` becomes `_` on the wire.

(defun prop-type->json (spec)
  (let ((type (pget spec :type))
        (h (make-hash-table :test #'equal)))
    (setf (gethash "type" h)
          (ecase type
            (:string "string") (:integer "integer") (:number "number")
            (:boolean "boolean") (:object "object") (:array "array")))
    (let ((desc (pget spec :description)))
      (when desc (setf (gethash "description" h) desc)))
    (let ((enum (pget spec :enum)))
      (when enum
        (setf (gethash "enum" h)
              (map 'vector (lambda (v) (if (keywordp v) (string-downcase v) v)) enum))))
    (when (eq type :array)
      (setf (gethash "items" h)
            (prop-type->json (or (pget spec :items) '(:type :string)))))
    (when (and (eq type :object) (pget spec :properties))
      (multiple-value-bind (props required) (props->json (pget spec :properties))
        (setf (gethash "properties" h) props)
        (when (plusp (length required))
          (setf (gethash "required" h) required))))
    h))

(defun props->json (props)
  (let ((h (make-hash-table :test #'equal))
        (required nil))
    (dolist (p props)
      (destructuring-bind (name &rest spec) p
        (let ((key (evo.provider::keyword->key name)))
          (setf (gethash key h) (prop-type->json spec))
          (unless (pget spec :optional)
            (push key required)))))
    (values h (coerce (nreverse required) 'vector))))

(defun schema->json-schema (schema)
  "SCHEMA: (:object <prop>...) -> JSON Schema hash-table."
  (assert (eq (first schema) :object))
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "type" h) "object")
    (multiple-value-bind (props required) (props->json (rest schema))
      (setf (gethash "properties" h) props)
      (setf (gethash "required" h) required))
    h))

(defun tool->provider-spec (tool)
  (list :name (tool-name tool)
        :description (tool-description tool)
        :input-schema (schema->json-schema (tool-schema tool))))

(defun execute-tool (tool args)
  "Run TOOL with ARGS (plist).  Returns (values content-string details is-error).
Conditions become error results (errors-as-data at the loop boundary)."
  (handler-case
      (multiple-value-bind (content details) (funcall (tool-execute-fn tool) args)
        (values (or content "") details nil))
    (error (e)
      (values (format nil "Tool error: ~a" e) nil t))))
