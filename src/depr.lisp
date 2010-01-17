
(defun pop-to(src tar)
  (let ((elem (pop-from-array src)))
    (cond ((null elem)  tar)
	  (t (pop-to src (add-to-array elem tar))))))


#|
(defmethod bson-encode-ht ( (ht hash-table) &key (array nil array-supplied-p) (size 10 size-supplied-p) )
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (add-octets (int32-to-octet 0) array )
    (with-hash-table-iterator (iterator ht)
      (dotimes (repeat (hash-table-count ht))
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p (add-octets (bson-encode key value) array :start 4 :from-end 1)))))
    (normalize-array array)))
|#
(defgeneric $op ( lhs rhs &key )
  (:documentation "mongo db $ operators..."))

(defmethod $op ( field value &key mongo op)

#|
   Test functions below..
|#

(defun test-do()
  (let ((lst (list 1 2 3 4)))
    (do () (( null lst ))
      (format t "~A ~%" (pop lst)))))
