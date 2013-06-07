(in-package :cl-mongo)

;;
;; This is intended to be a helper function for key-value pairs..
;; Here's a caveat : (kv "nested" (kv "hello" "again" (kv "678" 678)))
;; will generate ("nested" ("hello" "again")) because kv 
;; (kv "hello" "again" (kv "678" 678))) the third argument is ignored.
;;
;; Basically don't use kv to construct nested conses; use list instead.
;;
;; pair  -> (string value)
;; value -> atom | cons 
;; kv    -> pair | list
;; list  -> (elem list) | nil
;;

(defstruct (pair
             (:conc-name pair-)
	     (:constructor pair (key value)))
  key value)

(defclass kv-container () 
  ((container :initform (make-array 2 :fill-pointer 0 :adjustable t) :accessor container)))

(defmethod print-object ((kv-container kv-container) stream)
  (format stream "kv-container : ~A " (container kv-container)))

(defun make-kv-container ()
  (make-instance 'kv-container))

(defgeneric kv-container-push (el cont))

(defmethod kv-container-push ((el pair) (kv-container kv-container))
  (vector-push-extend el (container kv-container)))

(defgeneric kv-container-pop (cont))

(defmethod kv-container-pop ((kv-container kv-container))
  (unless (eql 0 (fill-pointer (container kv-container))) (vector-pop (container kv-container))))

(defgeneric kv-container-length (cont)
 )

(defmethod kv-container-length ((kv-container kv-container))
  (length (container kv-container)))

(defgeneric kv-container-aref (cont index))

(defmethod kv-container-aref ((index integer) (kv-container kv-container))
  (aref (container kv-container) index))

(defgeneric kv-container-kv (index cont)
 )

(defmethod kv-container-kv ((index integer) (kv-container kv-container))
  (let ((pair (kv-container-aref index kv-container)))
    (values (pair-key pair) (pair-value pair))))

(defgeneric kv-container-add (pair container))

(defmethod kv-container-add ((pair pair) (kv-container kv-container))
  (kv-container-push pair kv-container)
  kv-container)

(defmethod kv-container-add ((contb kv-container) (kv-container kv-container))
  (dotimes (index (kv-container-length contb))
    (let ((el (kv-container-aref index contb)))
      (kv-container-push el kv-container)))
  kv-container)

(defgeneric kv (a &rest rest) 
  (:documentation "This a helper function for key-value pairs and sets of key-value pairs.  
In a key-value pair like (kv key value) the key has to be a string and the
value something which is serializable. 
key-value pairs can be combined using kv as well : (kv (kv key1 val1) (kv key2 val2)).
This combination of key-value pairs is equivalent to a document without a unique id.
The server will assign a unique is if a list of key-value pairs is saved."))

(defmethod kv ((a (eql nil)) &rest rest)
  (pair nil (car rest)))

;; basically b can be anything with
;; an encoder..

(defmethod kv ((a string) &rest rest)
  (pair a (car rest)))

(defmethod kv ((a symbol) &rest rest)
  (declare (ignore rest))
  (pair (string-downcase a) (car rest)))

(defmethod kv ((kv-container kv-container) &rest rest)
  ;;FIXME clean up, move to kv-container constructor, remove rest from kv.
  (dolist (el rest)
    (when el (kv-container-add el kv-container)))
  kv-container)

(defmethod kv ((a pair) &rest rest)
  (let ((kvc (make-kv-container)))
    (kv-container-add a kvc)
    (dolist (el rest)
      (when el (kv-container-add el kvc)))
    kvc))
    

;(kv (kv "query" (kv nil nil)) (kv "orderby" (kv "k" 1)))

(defgeneric kv->doc (kv)
  (:documentation "turn a pair of key/value pairs into a document"))

(defmethod kv->doc ((kv pair)) 
  (let ((doc (make-document)))
    (add-element (pair-key kv) (pair-value kv) doc)))

(defmethod kv->doc ((kv-container kv-container)) 
  (let ((doc (make-document)))
    (dotimes (index (kv-container-length kv-container))
      (let ((kv (kv-container-aref index kv-container)))
	(add-element (pair-key kv) (pair-value kv) doc)))
    doc))

(defmethod kv->doc ((kv hash-table))
  (ht->document kv))

(defgeneric kv->ht (kv)
  (:documentation "turn a pair of key/value pairs into a hash table"))

(defmethod kv->ht ((kv pair))
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash (pair-key kv) ht) (pair-value kv))
    ht))

(defmethod kv->ht ((kv hash-table))
  kv)

(defmethod kv->ht ((kv-container kv-container))
  (elements (kv->doc kv-container)))

