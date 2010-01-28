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


(defmethod kv ( (a pair) (b pair) &rest rest)
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash (pair-key a) ht) (pair-value a))
    (setf (gethash (pair-key b) ht) (pair-value b))
    (dolist (el rest)
      (setf (gethash (pair-key el) ht) (pair-value el)))
    ht))

;this allows for (kv (kv "a" b) nil ("l" k) ) where
; the nil maybe the result of some test. it's skipped..
(defmethod kv ( (a pair) (b pair) &rest rest)
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash (pair-key a) ht) (pair-value a))
    (setf (gethash (pair-key b) ht) (pair-value b))
    (dolist (el rest)
      (when el (setf (gethash (pair-key el) ht) (pair-value el))))
    ht))

;this adds more kv's to an existing container..
;I don't check the type of the rest variables, 
;assuming they're all pairs..
(defmethod kv ( (ht hash-table) (b pair) &rest rest )
  (setf (gethash (pair-key b) ht) (pair-value b))
  (dolist (el rest)
    (setf (gethash (pair-key el) ht) (pair-value el)))
  ht)

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

(defmethod kv->doc ( (kv hash-table) ) 
  (ht->document kv))

(defgeneric kv->ht ( kv )
  (:documentation "turn a pair of key/value pairs into a hash table"))

(defmethod kv->ht ( (kv pair) )
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash (pair-key kv) ht) (pair-value kv))
    ht))

(defmethod kv->ht ( (kv hash-table) )
  kv)

