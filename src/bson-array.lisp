(in-package :cl-mongo)

#|
A note of the bson-array design

1. The encoders add the length of the element to the head of the byte stream
   and null terminate the byte stream.

2. The array format from what I can tell is this :
   <array length> [0 or more <bson-encoding minus header and null terminator>] <null terminator>

3. That means I need to keep add/update the header with the array length and add a null terminator each
   time the array is changed.

4. I remove the header (which is a size) and the null terminator returned from the bson-encoding. As an
   alternative I could change bson-encoding to not add these if this is an array element !

5. Make instance should produce a recognizable empty array.

|#

(defun make-int-vector (sz &key (init-fill 0))
  (make-array sz :element-type 'integer :initial-element 0 :fill-pointer init-fill :adjustable t))

(defclass bson-array()
  ((array :initarg :data-array :accessor data-array)
   (index-array :initarg :index-array :accessor index-array)))

;; terminate the bson array with a null and
;; encode the length in the first for bytes..

(defun normalize-array (array)
    (null-terminate-array array)
    (set-array-length array :start 0))

(defun normalize-bson-array(array)
  (normalize-array (data-array array)))
  
(defun make-bson-array(&key (size 10))
  (let ((array (make-instance 'bson-array 
			      :data-array (add-octets (int32-to-octet 0) (make-octet-vector (+ size 4)))
			      :index-array (make-int-vector 5))))
    (normalize-bson-array array)
    array))

(defun pop-from-array(arr)
  (when (positive (fill-pointer arr))
    (vector-pop arr)))

(defgeneric bson-array-push (element array)
  (:documentation "push element to arrray"))

(defmethod bson-array-push (element (array bson-array))
  (let ((key (format nil "~D" (fill-pointer (index-array array)))))
    (pop-from-array (data-array array)) ; remove terminating null
    (vector-push-extend (fill-pointer (data-array array)) (index-array array))
    ;; this skips the first four bytes returned from the encoding which
    ;; has the size of encoded object as well as the terminating null
    ;; (which does get added on by normalize !)
    (add-octets (bson-encode key element) (data-array array)  :start 4 :from-end 1)
    (normalize-bson-array array)
    array))

(defgeneric bson-array-pop (array)
  (:documentation "pop element from arrray"))

(defmethod bson-array-pop-element ((array bson-array))
  (let* ((retval  (copy-seq (subseq (data-array array) (vector-pop (index-array array)))))
	 (newfill (- (fill-pointer (data-array array)) (length retval))))
    (setf (fill-pointer (data-array array)) newfill)
    (normalize-bson-array array)
    retval))

(defmethod bson-array-pop ((array bson-array))
  (if (zerop (fill-pointer (index-array array)))
      (values nil nil)
      (values (bson-array-pop-element array) (fill-pointer (index-array array)))))

(defgeneric bson-array-reset (array))

(defmethod  bson-array-reset ((array bson-array))
  (setf (fill-pointer (data-array array)) 0) 
  (setf (fill-pointer (index-array array)) 0))

(defmethod print-object ((arr bson-array) stream)
  (format stream "~% ~S [~A] ~A" (type-of arr) 
	  (if (slot-boundp arr 'index-array) 
	      (index-array arr)
	      "no index array set..")
	  (if (slot-boundp arr 'array)
	      (data-array arr)
	      "no array set..")))


(defmethod bson-encode ((key string) (value bson-array) &key (array nil array-supplied-p)
                                                          (size 10 size-supplied-p)
                                                          (type +bson-data-array+) (encoder nil))
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value (array)
	       (add-octets (data-array value) array)))    ; add value
      (call-next-method key value :array array :type type :encoder (if encoder encoder #'encode-value)))))

(defun bson-encode-cons (list stack bson-array-stack)
  (labels ((encode-cons-helper-1 (element bson-array-stack)
	     (bson-array-push element (car bson-array-stack))
	     bson-array-stack)
	   (encode-cons-helper-2 (bson-array-stack)
	     (bson-array-push (car bson-array-stack) (cadr bson-array-stack))
	     (cdr bson-array-stack))
	   (encode-cons-done (list stack)
	     (and (zerop (length list)) (zerop (length stack)))))
    (cond ((encode-cons-done list stack) (car bson-array-stack))
	  ((zerop (length list)) (bson-encode-cons (car stack) (cdr stack)
                                                   (encode-cons-helper-2 bson-array-stack)))
	  ((consp (car list)) (bson-encode-cons
                               (car list) (cons (cdr list) stack) (cons (make-bson-array) bson-array-stack)))
	  (t (bson-encode-cons (cdr list) stack (encode-cons-helper-1 (car list) bson-array-stack))))))

(defmethod bson-encode ((key string) (value cons) &key)
  (bson-encode key (bson-encode-cons value () (list (make-bson-array :size (* (length value) 12))))))
