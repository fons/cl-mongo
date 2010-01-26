(in-package :cl-mongo)

#|
 shell commands
|# 


(defun docs ( result )
  (cadr (db.stop result)))
  
(defun iter ( result &key (mongo nil) (max-per-call 0) )
    (loop 
       (setf result (db.iter result :mongo mongo :limit max-per-call ) )
       (when (zerop (db.iterator result) ) (return result) )))

(defun rm (result &key (mongo nil) )
  (multiple-value-bind (iterator collection docs) (db.iterator result) 
    (db.stop iterator :mongo mongo)
    (db.delete collection docs)))

(defgeneric pp (result &key)
  (:documentation "pretty-printer for the command shell"))

(defmethod pp ( (result (eql nil) ) &key )
  nil)

(defmethod pp ( (result cons) &key (stream t) (nd nil) )
  (labels ((br+ ()
	     (unless nd (format stream "~%{")))
	   (br- ()
	     (unless nd (format stream "~%}~%")))

	   (pp-oid ( value )
	     (format stream "~%  \"_id\" -> objectid(")
	     (let* ((arr  (id value))
		    (size (length arr)))
	       (dotimes (index size)
		 (let ((x (aref arr index)))
		   (if (< x 10) 
		       (format stream "0~1X" x) 
		       (format stream "~2X" x)))))
	     (format stream ")"))

	   (pp* ( value )
	     (cond ( (typep value 'document)   (pp-doc  value) )
		   ( (typep value 'hash-table) (pp-ht   value))
		   ( (typep value 'cons)       (pp-cons value))
		   ( (typep value 'bson-oid)   (pp-oid  value))
		   ( t                         (format stream " ~A" value))))

	   (pp-cons (lst)
	     (progn 
	       (format stream "[")
	       (dolist (el lst)
		 (progn
		   (pp* el)
		   (format stream ",")))
	       (format stream "]")))

	   (pp-doc ( d )
	     (br+)
	     (unless (_local d) (pp* (_id d)))
	     (pp*  (elements d) )
	     (br-) )

	   (pp-ht ( ht )
	     (with-hash-table-iterator (iterator ht)
	       (dotimes (repeat (hash-table-count ht))
		 (multiple-value-bind (exists-p key value) (iterator)
		   (if exists-p
		       (progn 
			 (format stream "~%  \"~A\"  -> " key) 
			 (pp* value))))))))
    (dolist (d (docs result)) 
      (pp-doc d)
      )))

(defun nd (result &key (stream t) )
  (pp result :stream stream :nd t))

(defun now()
  (make-bson-time))

(defun date-time (second minute hour day month year &optional (time-zone *bt-time-zone*) )
  (make-bson-time (gmt-to-bson-time (encode-universal-time second minute hour day month year time-zone))))

;(defgeneric index ( collection name action &key )
;  (:documentation "shell index managment"))

;(defmethod (index (collection string) (name string) (action (eql 'create)) &key (unique nil) (asc t) (mongo nil) )
