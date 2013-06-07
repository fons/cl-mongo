(in-package :cl-mongo)

#|
  
|#

(defclass bson-oid ()
  ((id :reader id :initarg :oid))
  (:default-initargs
   :oid (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))

(defun make-bson-oid(&key (oid nil oid-supplied-p))
  (if oid-supplied-p
      (make-instance 'bson-oid :oid oid)
      (make-instance 'bson-oid)))

(defmethod print-object ((bson-oid bson-oid) stream)
  (format stream "~S [~A]" (type-of bson-oid) 
	  (if (slot-boundp bson-oid 'id) 
	      (id bson-oid)
	      "no id set..")))

(defgeneric _id (bson-oid) 
  (:documentation 
   "return a 12 byte array, irrespective of the internals of bson-oid, which may have a larger  array")) 

(defmethod _id ((bson-oid bson-oid))
  (subseq (id bson-oid) 0 12))

