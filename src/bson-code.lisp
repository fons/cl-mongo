(in-package :cl-mongo)

(defclass bson-code ()
  ((code :reader code :initarg :code)))

(defun make-bson-code (js)
  (make-instance 'bson-code :code js))

(defmethod print-object ((bson-code bson-code) stream)
  (format stream "~S [~A] ~%" (type-of bson-code)
          (if (slot-boundp bson-code 'code)
	      (code bson-code)
	      "code not set..")))
