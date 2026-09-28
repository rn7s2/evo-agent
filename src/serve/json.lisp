;;;; json.lisp — the one mapping between evo's sexprs and JSON.
;;;;
;;;; Everything serve sends is a journal-vocabulary sexpr (design.md §4.3) —
;;;; events are the --events plists, state is a fold — and it crosses the wire
;;;; through exactly this mapping, the inverse of EVO:JSON->SEXPR:
;;;;
;;;;   plist (keyword keys)  -> object; :line-count -> "line_count"
;;;;   any other list        -> array
;;;;   vector (not a string) -> array
;;;;   string, integer       -> string, number
;;;;   ratio, float          -> number (double)
;;;;   T                     -> true
;;;;   NIL                   -> null (nil is also the empty list: "nothing")
;;;;   the symbol FALSE      -> false (serve's own replies, where a boolean
;;;;                            must read as one)
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

(defun keyword->json-key (keyword)
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun plist-p (value)
  "True for a non-empty list of keyword/value pairs."
  (and (consp value)
       (loop for tail on value by #'cddr
             always (and (keywordp (car tail)) (consp (cdr tail))))))

(defun sexpr->json-value (value)
  "VALUE as the jzon value the mapping above says."
  (cond ((eq value t) t)
        ((null value) 'null)
        ((eq value 'false) nil)
        ((keywordp value) (string-downcase (symbol-name value)))
        ((symbolp value) (string-downcase (symbol-name value)))
        ((stringp value) value)
        ((integerp value) value)
        ((realp value) (coerce value 'double-float))
        ((plist-p value)
         (let ((object (make-hash-table :test #'equal)))
           (loop for (k v) on value by #'cddr
                 do (setf (gethash (keyword->json-key k) object)
                          (sexpr->json-value v)))
           object))
        ((listp value) (map 'vector #'sexpr->json-value value))
        ((vectorp value) (map 'vector #'sexpr->json-value value))
        ((pathnamep value) (namestring value))
        (t (princ-to-string value))))

(defun json-value->sexpr (value)
  "The reverse direction: EVO:JSON->SEXPR, named here beside its inverse."
  (json->sexpr value))

(defun encode-json (value)
  "VALUE (a sexpr) as JSON text."
  (com.inuoe.jzon:stringify (sexpr->json-value value)))

(defun decode-json (text)
  "JSON TEXT as a sexpr (objects as keyword plists).  Signals on bad JSON."
  (json->sexpr (com.inuoe.jzon:parse text)))

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
