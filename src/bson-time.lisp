(in-package :cl-mongo)

(defconstant +java-script-epoch+           2208988800 "(encode-universal-time 0 0 0 1 1 1970 0)")
(defconstant +milliseconds-multiplier+     1000       "'convert' to milliseconds")

; from the cl cookbook
(defvar +bt-day-names+ '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defvar +bt-months+    '("xxx" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar *bt-time-zone* 5 "the current time zone; 5 -> EST")

#|
  bson/mongo uses miliseconds since epoch (1/1/1970 0:0:0 GMT).
  this encapsulates that concept
|#
(defun bson-time-to-ut (bson-time) 
  (floor (+ (/ (raw bson-time) +milliseconds-multiplier+) +java-script-epoch+)))

(defun gmt-to-bson-time (gmt) 
  (* +milliseconds-multiplier+ (- gmt +java-script-epoch+)))

(defclass bson-time ()
  ((raw   :reader raw   :initarg :raw)
   (local :reader local :initarg :local))
  (:default-initargs
   :local t      
    :raw (gmt-to-bson-time (get-universal-time))))

(defun make-bson-time (&optional raw-time)
  (if raw-time  
      (make-instance 'bson-time :raw raw-time :local nil)
      (make-instance 'bson-time)))

(defgeneric decode (time)
  (:documentation "decode the bson-time for human consumption"))

(defmethod decode ((bson-time bson-time)) 
  (decode-universal-time (floor (+ (/ (raw bson-time) +milliseconds-multiplier+) +java-script-epoch+))
			 *bt-time-zone*))

(defgeneric fmt (bson-time &optional stream)
  (:documentation "format bson time to be in human readable form"))

(defmethod fmt ((bson-time bson-time) &optional (stream t))
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz) (decode bson-time)
    (declare (ignore dst-p))
    (format stream "~a ~a ~d ~d ~2,'0d:~2,'0d:~2,'0d (GMT~@d)"
	    (nth day-of-week +bt-day-names+)
	    (nth month +bt-months+)
	    date
	    year
	    (- hour 0)
	    minute
	    second
	    (- tz))))

(defmethod print-object ((bson-time bson-time) stream)
  (format stream "~S " (type-of bson-time))
  (if (slot-boundp bson-time 'raw) 
      (fmt bson-time stream)
      "no time set.."))

(defun time-zone ()
  "Set the time zone appropriate for the current environment."
  (setf *bt-time-zone* (car (last (multiple-value-list (get-decoded-time))))))

  


