;;;; tools.lisp — tool registry + sexpr schema -> JSON Schema.
;;;;
;;;; Tool interface: name, description, sexpr schema, execute function,
;;;; :content (model-visible) vs :details (host-visible) in the result.
;;;; Execution is sequential.  Tools signal conditions; the loop
;;;; converts them to error tool-results.

(in-package :evo.kernel)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (evo.port:add-package-local-nickname :jzon :com.inuoe.jzon :evo.kernel))

(defstruct tool
  name          ; string
  description   ; string
  schema        ; sexpr schema (see below), or a ready-made JSON Schema hash-table
  execute-fn    ; (lambda (args) ...) -> string, or (values content details)
  (source :builtin)
  ;; What EXECUTE-FN is handed: :plist, the keywordized plist every evo tool
  ;; reads, or :json, the model's arguments exactly as it wrote them (jzon
  ;; values: hash-tables, vectors, strings).  See TOOL-CALL-ARGUMENTS.
  (arguments :plist))

(defvar *tool-registry* (make-hash-table :test #'equal))
(defvar *registry-generation* 0
  "Bumped on every registry mutation; the loop rebuilds the system prompt when it changes.")

(defun register-tool* (&key name description schema execute (source :builtin)
                            (arguments :plist))
  (check-type name string)
  (check-type execute function)
  (assert (member arguments '(:plist :json)) ()
          "Tool ~a: :arguments must be :plist or :json, not ~s" name arguments)
  (setf (gethash name *tool-registry*)
        (make-tool :name name :description description :schema schema
                   :execute-fn execute :source source :arguments arguments))
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
  "SCHEMA: (:object <prop>...) -> JSON Schema hash-table.

A hash-table is already a JSON Schema and passes through verbatim.  That is
the escape hatch for a tool whose contract was written somewhere else — an
MCP server's inputSchema, an OpenAPI operation — where re-expressing it in
the sexpr DSL would silently drop everything the DSL cannot say
\(additionalProperties, unions, min/max) and hand the model a schema its
server will then reject."
  (when (hash-table-p schema)
    (return-from schema->json-schema schema))
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

;;; Tool results are content blocks, not just text.
;;;
;;; A tool normally returns a string and means one text block by it.  It may
;;; instead return content blocks — that is how a tool hands back something
;;; the model has to SEE rather than read, an image being the only such
;;; thing today (READ on a screenshot).  The two shapes are told apart by
;;; the car: a block is a plist, so it starts with a keyword.

(defparameter *image-block-tokens* 4800
  "What one image costs in context, in tokens.  Both vision stacks resize an
image to roughly 1568px on its long edge and tokenize the result, which lands
near this; the number is flat because the block carries no dimensions.")

(defparameter *tool-result-block-types* '(:text :image)
  "Block types a tool result may carry.  Anything else a tool hands back is
stringified into a text block here — a userspace tool is agent-written, and
an unknown block reaching a wire adapter would fail the whole request at
build time and name the adapter rather than the tool that produced it.")

(defun tool-content-blocks (content)
  "Normalize what a tool returned into a list of content blocks."
  (labels ((text-block (x) (list :type :text :text (princ-to-string x)))
           (block-p (x) (and (consp x)
                             (member (pget x :type) *tool-result-block-types*)))
           (as-block (x) (if (block-p x) x (text-block x))))
    (cond ((null content) nil)
          ((stringp content) (list (list :type :text :text content)))
          ((not (consp content)) (list (text-block content)))
          ;; A block is a plist, so it starts with a keyword; a list of
          ;; blocks starts with a list.  That is the whole disambiguation.
          ((keywordp (car content)) (list (as-block content)))
          (t (loop for x in content unless (null x) collect (as-block x))))))

;;; What a tool call is handed.
;;;
;;; The JSON<->plist bridge upcases keys and swaps `_` for `-`, which is what
;;; makes `(:by-line t)` pleasant to read — and what makes it lossy when the
;;; keys are data rather than a fixed contract: a map of file paths to
;;; contents comes back as `src/app.jsx`, a knob named `bgColor` as `bgcolor`.
;;; Evo's own tools have fixed lowercase contracts and never notice.  A tool
;;; whose schema came from elsewhere does, so it may register :arguments :json
;;; and be handed the model's arguments exactly as written.

(defun tool-call-arguments (tool call args)
  "The value TOOL's execute function receives for CALL.

ARGS is the plist after :tool-call interception; a hook that rewrote it wins
over the raw text, because the rewrite is the thing that must run.  Falls back
to re-encoding the plist when no raw text survives (a resumed session journaled
before this existed), so a :json tool degrades to lossy rather than broken."
  (if (eq (tool-arguments tool) :plist)
      args
      (let ((raw (and (eq args (pget call :arguments)) (pget call :arguments-json))))
        (or (and raw (handler-case (jzon:parse raw) (error () nil)))
            (and args (evo.provider::sexpr->json args))
            (make-hash-table :test #'equal)))))

(defun tool-call-display-arguments (name arguments arguments-json)
  "What a frontend shows for a call's arguments: the model's raw JSON text for
a :json tool, whose plist form has lossy keys, and the plist for everyone else
\(where `key=value` reads better than JSON)."
  (let ((tool (find-tool name)))
    (or (and tool (eq (tool-arguments tool) :json) (stringp arguments-json)
             arguments-json)
        arguments)))

(defun execute-tool (tool args)
  "Run TOOL with ARGS — the plist, or the model's exact JSON for a tool
registered :arguments :json (see TOOL-CALL-ARGUMENTS, which the loop calls to
decide).  Returns (values content details is-error),
CONTENT being a string or a list of content blocks.
Conditions become error results (errors-as-data at the loop boundary)."
  (handler-case
      (multiple-value-bind (content details) (funcall (tool-execute-fn tool) args)
        (values (or content "") details nil))
    (error (e)
      (values (format nil "Tool error: ~a" e) nil t))))
