

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


(defgeneric bson-encode-container ( container &key )
  (:documentation "encode a container of key-value pairs.."))

;;(defmethod bson-encode-container ( (container array) )
;;  container)

(defmethod bson-encode-container ( (container document) &key (array nil) (size 10) )
  (setf (gethash "_id" (elements container)) (_id container))
  (bson-encode-container (elements container) :array array :size size))

(defmethod bson-encode-container ( (container hash-table) &key (array nil) (size nil) )
  (let* ((size (or (size 10)))
	 (array (or array (make-octet-vector size))))
    (add-octets (int32-to-octet 0) array )
    (with-hash-table-iterator (iterator container)
      (dotimes (repeat (hash-table-count container))
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p (add-octets (bson-encode key value) array :start 4 :from-end 1)))))
    (normalize-array array)))

(defmethod bson-encode ( (key string) (value hash-table) &key (array nil array-supplied-p) 
			(size 10 size-supplied-p) 
			(type nil) (encoder nil))
  (declare (ignore encoder) (ignore array) (ignore type) (ignore size) 
	   (ignore size-supplied-p) (ignore array-supplied-p) )
  (bson-encode key (bson-encode-container value :array (make-octet-vector (* (hash-table-count value) 12)))))

(defmethod bson-encode ( (key string) (value document) &key (array nil array-supplied-p) 
			(size 10 size-supplied-p) 
			(type nil) (encoder nil))
  (declare (ignore encoder) (ignore array) (ignore type) (ignore size) 
	   (ignore size-supplied-p) (ignore array-supplied-p) )
  (bson-encode key (bson-encode-container value )))

(defun ht->document (ht) 
  (multiple-value-bind (oid oid-supplied) (gethash "_id" ht)
    (let ((doc (make-document :oid (if oid-supplied oid nil))))
      (when oid-supplied (remhash "_id" ht))
      (with-hash-table-iterator (iterator ht)
	(dotimes (repeat (hash-table-count ht))
	  (multiple-value-bind (exists-p key value) (iterator)
	    (if exists-p (add-element key value doc)))))
      doc)))



