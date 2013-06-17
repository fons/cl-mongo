(in-package :cl-mongo)


(defgeneric bson-encode-container (container &key)
  (:documentation "encode a container of key-value pairs.."))

(defmethod bson-encode-container ((container pair) &key (array nil) (size 10))
  (bson-encode-container (kv->ht container) :array array :size size))

(defmethod bson-encode-container ((container (eql nil)) &key (array nil))
  (bson-encode nil nil :array array))

(defmethod bson-encode-container ((container document) &key (array nil) (size 10))
  (setf (gethash "_id" (elements container)) (_id container))
  (bson-encode-container (elements container) :array array :size size))

(defmethod bson-encode-container ((container hash-table) &key (array nil) (size nil))
  (let* ((size (or size 10))
	 (array (or array (make-octet-vector size))))
    (add-octets (int32-to-octet 0) array)
    (with-hash-table-iterator (iterator container)
      (dotimes (repeat (hash-table-count container))
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p (add-octets (bson-encode key value) array :start 4 :from-end 1)))))
    (normalize-array array)))

(defmethod bson-encode-container ((kv-container kv-container) &key (array nil) (size nil))
  (let* ((size (or size 10))
	 (array (or array (make-octet-vector size))))
    (add-octets (int32-to-octet 0) array)
    (dotimes (index (kv-container-length kv-container))
      (multiple-value-bind (key value) (kv-container-kv index kv-container)
	(add-octets (bson-encode key value) array :start 4 :from-end 1)))
    (normalize-array array)))

(defmethod bson-encode-container ((kv pair) &key)
  (bson-encode (pair-key kv) (pair-value kv)))

(defmethod bson-encode ((key string) (value hash-table) &key)
  (bson-encode key (bson-encode-container value :array (make-octet-vector (* (hash-table-count value) 12)))))

(defmethod bson-encode ((key string) (value document) &key)
  (bson-encode key (bson-encode-container value)))

(defmethod bson-encode ((key string) (value kv-container) &key)
  (bson-encode key (bson-encode-container value)))


(defmethod bson-encode ((key string) (value pair) &key)
  (bson-encode key (bson-encode-container value)))
