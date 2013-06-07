(in-package :cl-mongo)

(defclass bson-regex()
  ((regex    :reader regex    :initarg :regex)
   (options  :reader options  :initarg :options)))

(defun make-bson-regex (regex options)
  (make-instance 'bson-regex :regex regex :options options))

(defmethod print-object ((bson-regex bson-regex) stream)
  (format stream "~S [/~A/~A] ~%" (type-of bson-regex)
 	  (if (slot-boundp bson-regex 'regex) 
	      (regex bson-regex)
	      "regex not set..")
	  (if (slot-boundp bson-regex 'options) 
	      (options bson-regex)
	      "options not set..")))
