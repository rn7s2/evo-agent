;;;; json.lisp — the one mapping between evo's sexprs and JSON.
;;;;
;;;; Everything serve sends is a journal-vocabulary sexpr (design.md §4.3) —
;;;; events are the --events plists, state is a fold — and it crosses the wire
;;;; through exactly this mapping, the inverse of EVO:JSON->SEXPR:
;;;;
;;;;   plist (keyword keys)  -> object; :line-count -> "line_count"
;;;;   JSON-OBJECT           -> object with keys that are not identifiers
;;;;   a dotted pair         -> object of one key, its ALIST entry
;;;;   any other proper list -> array
;;;;   improper list, or an object this mapping has never seen
;;;;                         -> its printed form, as a string: the mapping is
;;;;                            total, because an error reply quotes the value
;;;;                            it choked on, and a value can be a secret
;;;;   vector (not a string) -> array
;;;;   string, integer       -> string, number
;;;;   ratio, float          -> number (double)
;;;;   T                     -> true
;;;;   NIL                   -> null (nil is also the empty list: "nothing")
;;;;   the keyword :FALSE    -> false, and so does the symbol FALSE
;;;;                            (a keyword is the one spelling every package
;;;;                            shares, which is why a field that must read as
;;;;                            false — has_more, truncated, ready — says
;;;;                            :false rather than the NIL that means null)
;;;;   keyword (as a value)  -> string, lowercased: :text-delta -> "text-delta"
;;;;
;;;; JSON->SEXPR reads it back: objects become plists under the same keys,
;;;; arrays vectors, null and false NIL.  What JSON cannot say is a keyword
;;;; value (it comes back as its string) and the list/vector distinction (it
;;;; comes back a vector) — so the round trip is exact at the JSON level:
;;;; encode, decode, encode again gives back the same JSON value (object key
;;;; order aside, which JSON does not define), which the unit suite checks for
;;;; every event shape the kernel emits.

(in-package :evo.serve)

(defun keyword->json-key (key)
  "KEY — a keyword, or a string for a name that is not an identifier (a topic
name, \"lane:1\") — as its JSON key."
  (if (stringp key)
      key
      (substitute #\_ #\- (string-downcase (symbol-name key)))))

(defun json-key-name (key)
  "KEY as an object key: a keyword's snake_case name, a string as it stands."
  (cond ((keywordp key) (keyword->json-key key))
        ((stringp key) key)
        (t (string-downcase (symbol-name key)))))

(defun list-shape (value)
  "How VALUE walks as a list: :PROPER, :DOTTED, or :CIRCULAR.  Walking it must
end for every value, a cycle included: a journal holds whatever a session put
there, and an encoder that hangs on one is not total (CONTRACT §5.6, R1).
Two steps against one, so a cycle is found instead of followed."
  (let ((slow value) (fast value))
    (loop
      (cond ((null fast) (return :proper))
            ((atom fast) (return :dotted))
            ((null (cdr fast)) (return :proper))
            ((atom (cdr fast)) (return :dotted)))
      (setf slow (cdr slow) fast (cddr fast))
      (when (eq slow fast) (return :circular)))))

(defun proper-list-p (value)
  "True for a list that ends in NIL.  A dotted pair, an improper list and a
circular one are all lists to CONSP, and none can be walked by MAP — which is
exactly how one `(\"Authorization\" . \"Bearer …\")` in *settings* used to turn
/registry into a 500."
  (eq (list-shape value) :proper))

(defun plist-p (value)
  "True for a non-empty list of keyword/value pairs: a plist is an object on
the wire, anything else (a vector, a list of non-keyword first elements) an
array.  A name that is not an identifier — a topic, \"lane:1\" — needs
JSON-OBJECT-VALUE instead."
  (and (consp value)
       (eq (list-shape value) :proper)
       (loop for tail on value by #'cddr
             always (and (keywordp (car tail)) (consp (cdr tail))))))

(defun unprintable-string (value)
  "VALUE as a string, whatever it is.  The last resort of the mapping: a value
outside the vocabulary must not fail the whole document, and printing it is
already better than dropping it.  *PRINT-CIRCLE* so a self-referential
structure terminates."
  (let ((*print-circle* t))
    (or (ignore-errors (princ-to-string value)) "#<unprintable>")))

;;; An object, holding its keys in the order they were written.  A map keyed
;;; by topic name cannot be a plist (the first element would read as an
;;; array), so it says so — and every object the mapping builds is this type,
;;; because a hash table's keys come out in whatever order the implementation
;;; walks them, which JSON does not define and two implementations do not
;;; agree on.  A document evo emits is compared to itself (a test, a golden
;;; file, an SSE line a client replays), so the order is ours to keep.

(defstruct (json-object-value (:constructor %make-json-object-value))
  (pairs nil))

(defun object-from-pairs (pairs)
  "A JSON object from PAIRS, an alist of (key . value), in that order.  The
pairs are converted once, here: a value inside an object is written as it
stands, never through this mapping a second time (which would read the NIL a
converted false became as null)."
  (%make-json-object-value
   :pairs (loop for (key . value) in pairs
                collect (cons (json-key-name key) (sexpr->json-value value)))))

(defun json-object-value (pairs)
  "PAIRS — a flat list of (key . value) — as a JSON object.  KEY may be a
keyword or a string.  Not named JSON-OBJECT: that is EVO.JOURNAL's (a plist
as JSON text), which a server inherits through its package, and one of the two
would silently replace the other."
  (object-from-pairs pairs))

(defmethod com.inuoe.jzon:write-value ((writer com.inuoe.jzon:writer)
                                       (value json-object-value))
  "Write VALUE's keys in the order they were given (see above)."
  (com.inuoe.jzon:begin-object writer)
  (loop for (key . item) in (json-object-value-pairs value)
        do (com.inuoe.jzon:write-key writer key)
           (com.inuoe.jzon:write-value writer item))
  (com.inuoe.jzon:end-object writer))

(defun sexpr->json-value (value)
  "VALUE as the jzon value the mapping above says.  Total: every object has an
answer, and none of them is a condition — a registry holding an alist of
strings, a dotted pair or an object this mapping has never seen is a document
to send, not a request to fail (an error body quotes the value it choked on,
and a value can be a secret)."
  (cond ((eq value t) t)
        ((null value) 'null)
        ((or (eq value :false) (eq value 'false)) nil)
        ;; Enum values are snake_case too (CONTRACT §4: \"lane-report\" on the
        ;; wire is "lane_report").
        ((keywordp value) (substitute #\_ #\- (string-downcase (symbol-name value))))
        ((symbolp value) (string-downcase (symbol-name value)))
        ((stringp value) value)
        ((integerp value) value)
        ((realp value) (coerce value 'double-float))
        ;; Built already converted (OBJECT-FROM-PAIRS): its pairs are the
        ;; jzon values the writer emits, not sexprs to convert again.
        ((json-object-value-p value) value)
        ((plist-p value)
         (object-from-pairs (loop for (k v) on value by #'cddr collect (cons k v))))
        ((consp value)
         (cond ((proper-list-p value) (map 'vector #'sexpr->json-value value))
               ;; A dotted pair: an alist entry ("Authorization" . "Bearer …")
               ;; is one key and its value, and that is what it becomes.
               ((and (atom (cdr value))
                     (or (stringp (car value)) (symbolp (car value))))
                (object-from-pairs (list (cons (car value) (cdr value)))))
               (t (unprintable-string value))))
        ((vectorp value) (map 'vector #'sexpr->json-value value))
        ((pathnamep value) (namestring value))
        (t (unprintable-string value))))

(defun json-value->sexpr (value)
  "The reverse direction: EVO:JSON->SEXPR, named here beside its inverse."
  (json->sexpr value))

(defun encode-json (value)
  "VALUE (a sexpr) as JSON text."
  (com.inuoe.jzon:stringify (sexpr->json-value value)))

(defun decode-json (text)
  "JSON TEXT as a sexpr (objects as keyword plists).  Signals on bad JSON.

The parse is EVO.UTIL:PARSE-JSON: a body evo accepts carries an image's bytes
inside one string, and the parser's own default string limit is smaller than
one picture (see EVO.UTIL:*MAX-JSON-STRING-LENGTH*)."
  (json->sexpr (evo.util:parse-json text)))

(defun event->json (event)
  "EVENT (an --events plist) as one line of JSON.  An event holding a value
outside the vocabulary must not kill the stream: it goes out as
{\"type\":\"unprintable-event\"} naming the original type, which is what
--events prints for the same case."
  (handler-case (encode-json event)
    (error ()
      (encode-json (list :type :unprintable-event
                         :original-type (let ((type (getf event :type)))
                                          (if (symbolp type) type "?")))))))
