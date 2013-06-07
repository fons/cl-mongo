(in-package :cl-mongo)

(defconstant +bson-binary-generic+  #x00 "binary/generic")
(defconstant +bson-binary-function+ #x01 "function")
(defconstant +bson-binary+          #x02 "ordinary binary")
(defconstant +bson-binary-uuid+     #x03 "uuid")
(defconstant +bson-binary-md5+      #x05 "md5")
(defconstant +bson-binary-user+     #x80 "user defined")

(defclass bson-binary-base ()
  ((data :reader data :initarg :data))
  (:documentation "bson binary type; this is the base class."))

(defclass bson-binary-function (bson-binary-base)
  ((type-id :reader type-id :initform +bson-binary-function+))
  (:documentation "bson function binary type"))

(defclass bson-binary (bson-binary-base)
  ((type-id :reader type-id :initform +bson-binary+))
  (:documentation "bson ordinary binary type"))

(defclass bson-binary-uuid (bson-binary-base)
  ((type-id :reader type-id :initform +bson-binary-uuid+))
  (:documentation "bson uuid binary type"))

(defclass bson-binary-md5 (bson-binary-base)
  ((type-id :reader type-id :initform +bson-binary-md5+))
  (:documentation "bson md5 binary type"))

(defclass bson-binary-user (bson-binary-base)
  ((type-id :reader type-id :initform +bson-binary-user+))
  (:documentation "bson user defined binary type"))

(defgeneric bson-binary (type data)
  (:documentation "Create a bson serializable binary object. type is a keyword; either one of
:function :binary :uuid :md5 or :user. data is the data encapsulated by this type."))

(defmethod bson-binary ((type (eql :function)) data)
  (make-instance 'bson-binary-function :data data))

(defmethod bson-binary ((type (eql +bson-binary-function+)) data)
  (make-instance 'bson-binary-function :data data))

(defmethod bson-binary ((type (eql :binary)) data)
  (make-instance 'bson-binary :data data))

(defmethod bson-binary ((type (eql +bson-binary+)) data)
  (make-instance 'bson-binary :data data))

(defmethod bson-binary ((type (eql :md5)) data)
  (make-instance 'bson-binary-md5 :data data))

(defmethod bson-binary ((type (eql +bson-binary-md5+)) data)
  (make-instance 'bson-binary-md5 :data data))

(defmethod bson-binary ((type (eql :uuid)) data)
  (make-instance 'bson-binary-uuid :data data))

(defmethod bson-binary ((type (eql +bson-binary-uuid+)) data)
  (make-instance 'bson-binary-uuid :data data))

(defmethod bson-binary ((type (eql :user)) data)
  (make-instance 'bson-binary-user :data data))

(defmethod bson-binary ((type (eql +bson-binary-user+)) data)
  (make-instance 'bson-binary-user :data data))

(defun str-md5 (md5)
  (ironclad:byte-array-to-hex-string md5))

(defgeneric rep (stream bson-binary)
  (:documentation "a human-readable representation of the binary data ."))

(defmethod rep (stream (bson-binary bson-binary-base))
  (format stream " [binary data of type ~A] " (type-id bson-binary)))

(defmethod rep :after (stream (bson-binary bson-binary-md5))
  (format stream " {~A}" (str-md5 (data bson-binary))))

(defmethod print-object ((bson-binary-base bson-binary-base) stream)
  (format stream "[~S] " (type-of bson-binary-base))
  (if (slot-boundp bson-binary-base 'data)
      (rep stream bson-binary-base)
      "binary not set.."))

