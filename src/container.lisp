(in-package :cl-mongo)

#|
  containers are collections of key-value pairs. These are either cons's, hash tables
  or documents..
|#

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
