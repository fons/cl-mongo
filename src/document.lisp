(in-package :cl-mongo)

#|
  Document is a collection of key/value pairs
|#

(defclass document()
  ((elements  :initform (make-hash-table :test #'equal) :accessor elements)
   (_local_id :initarg :local :reader _local)
   (_id       :initarg :oid   :reader _id))
  (:default-initargs
   :local t
   :oid (make-bson-oid)))


(defun make-document ( &key (oid nil) )
  (if oid
      (make-instance 'document :oid oid :local nil)
      (make-instance 'document)))

(defgeneric add-element ( key value document) 
  ( :documentation "add element to a document" ) )

(defmethod add-element (key value document)
  document)

(defmethod add-element ( (key string) value (document document) )
  (setf (gethash key (elements document)) value)
  (call-next-method))

(defgeneric get-element ( key document) 
  ( :documentation "retrieve element value from a document" ) )

(defmethod get-element ( (key string) (document document) ) 
  (gethash key (elements document)))

(defgeneric rm-element (key document) 
  ( :documentation "remove element from a document" ) )

(defmethod rm-element ( (key string) (document document) ) 
  (remhash key (elements document)))


;;
;; When the to-hash-able finalizer is used, embedded docs/tables in the response aren't converted
;; to hash tables but to documents. When print-hash is used we want to see hash table like output
;; so that's what this tries to do..
;;
(defun print-hash (ht stream)
  (labels ((prdocs (docs) 
	     (dolist (doc docs)
	       (if (typep doc 'document)
		   (print-hash (elements doc) stream)
		   (format stream "    ~A ~%" doc))))
	   (vpr (v)
	     (cond ( (typep v 'cons)     (prdocs v)        )
		   ( (typep v 'document) (prdocs (list v)) )
		   (  t                  v))))
    (format stream "~%{~%") 
    (with-hash-table-iterator (iterator ht)
      (dotimes (repeat (hash-table-count ht))
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p (format stream "  ~A  -> ~A ~%" key (vpr value) )))))
    (format stream "}~%")))

;
; suppress the printing of the object id if the objectis locally generated
;

(defmethod print-object ((document document) stream)
  (format stream "~%{  ~S ~%" (type-of document) ) 
  (unless (slot-boundp  document '_id)       (format stream "  _id not set"))
  (unless (slot-boundp  document '_local_id) (format stream "  _local_id not set"))
  (when (and (slot-boundp  document '_local_id) (slot-boundp  document '_id) )
    (unless (_local document) (format stream "  _id : ~A~%" (_id document))))
  (if (slot-boundp document 'elements)
      (print-hash (elements document) stream)
      "no elements set..")
  (format stream "}~%")) 
