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

(defgeneric kv (a b &rest rest) )

(defmethod kv ( (a (eql nil) ) (b (eql nil)) &rest rest)
  (declare (ignore rest))
  (pair nil nil))

;; basically b can be anything with
;; an encoder..

(defmethod kv ( (a string) b &rest rest)
  (declare (ignore rest))
  (pair a b))

(defmethod kv ( (a symbol) b &rest rest)
  (declare (ignore rest))
  (pair (string-downcase a) b))

;
;; TODO -> don't return a cons as that's basically an array,
;; a compound query is a document/hash table.
;; when returning a cons --> need to call kv->ht on the cons !!  
;; so you might as well incorporate that stright in here
(defmethod kv ( (a pair) (b pair) &rest rest)
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash (pair-key a) ht) (pair-value a))
    (setf (gethash (pair-key b) ht) (pair-value b))
    (dolist (el rest)
      (setf (gethash (pair-key el) ht) (pair-value el)))
    ht))

(defmethod kv-alt ( (a pair) (b pair) &rest rest)
  (let ((lst (list b a)))
    (dolist (el rest)
      (push el lst))
    (nreverse lst)))

    
(defun bson-encode-pair ( kv )
  (bson-encode (pair-key kv) (pair-value kv)))

(defmethod bson-encode ( (key string) (value pair) &key (array nil array-supplied-p) 
			(size 10 size-supplied-p) 
			(type nil) (encoder nil))
  (declare (ignore encoder) (ignore array) (ignore type) (ignore size) 
	   (ignore size-supplied-p) (ignore array-supplied-p) )
  (bson-encode key (bson-encode-pair value)))


(defgeneric kv->doc ( kv )
  (:documentation "turn a pair of key/value pairs into a document"))

(defmethod kv->doc ( (kv pair) ) 
  (let ((doc (make-document)))
    (add-element (pair-key kv) (pair-value kv) doc)))

(defmethod kv->doc ( (kv cons) ) 
  (let ((doc (make-document)))
    (dolist (pair kv)
      (add-element (pair-key pair) (pair-value pair) doc))
    doc))


(defmethod kv->ht ( (kv cons) )
  (elements (kv->doc kv)))

