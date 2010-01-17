(in-package :cl-mongo)

(defconstant +bson-data-number+   1   "bson number encoding")
(defconstant +bson-data-string+   2   "bson string encoding")
(defconstant +bson-data-object+   3   "bson data array; bson object")
(defconstant +bson-data-array+    4   "bson array")
(defconstant +bson-data-oid+      7   "bson oid encoding")
(defconstant +bson-data-boolean+  8   "bson boolean encoding")
(defconstant +bson-data-int32+    16  "bson 32 bit int encoding")
(defconstant +bson-data-long+     18  "bson 64 bit int encoding")


#|
  bson-encode encodes a complete bson object.
  It includes the obj_size at the start and the terminating null at the end.
  When creating composites these need to be skipped or removed..
|#

(defgeneric bson-encode(key value &key array size type encoder)
  (:documentation "encode a bson data element"))

(defun set-array-length(array &key (start 0 start-supplied-p))
  (let* ((head  (if start-supplied-p start (fill-pointer array))))      ; save the stack pointer
    (set-octets head (int32-to-octet (- (length array) head) ) array))) ; set length    

(defun null-terminate-array(array)
  (add-octets (byte-to-octet 0) array))                       ; ending nul

(defmethod bson-encode(key value &key array size type encoder)
  (declare (ignore size))
  (let* ((head  (fill-pointer array)))                         ; save the stack pointer
    (add-octets (int32-to-octet 0)    array)                   ; length, set to zero
    (add-octets (byte-to-octet  type) array)                   ; data element code
    (add-octets (string-to-null-terminated-octet key) array)   ; key
    (funcall encoder array)                                    ; call type sepcifi encoder
    (add-octets (byte-to-octet 0) array)                       ; ending nul
    (set-octets head (int32-to-octet (- (length array) head) ) array)      ; set length    
    array))

(defmethod bson-encode( (key (eql nil)) (value (eql nil)) &key (array nil array-supplied-p) size type encoder)
  (declare (ignore size) (ignore type) (ignore encoder) )
  (let ((array (if array-supplied-p array (make-octet-vector 5))))
    (add-octets (int32-to-octet 0)    array)                   ; length, set to zero
    (add-octets (byte-to-octet 0)     array)                  ; ending nul
    (set-octets 0 (int32-to-octet (length array)  ) array)      ; set length    
    array))

(defmethod bson-encode ( (key integer) value &key (array nil array-supplied-p) 
			(size 10 size-supplied-p) 
			(type +bson-data-array+) (encoder nil))
  (declare (ignore encoder) (ignore array) (ignore type) (ignore size) 
	   (ignore size-supplied-p) (ignore array-supplied-p) )
  (bson-encode (format nil "~A" key) value)) 
	       
(defmethod bson-encode( (key string) (value string) &key (array nil array-supplied-p) (size 10 size-supplied-p) 
		       (type +bson-data-string+) (encoder nil) )
  (declare (ignore encoder))
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value (array)
	       (add-octets (int32-to-octet (1+ (length value)) ) array)   ; length of the value string
	       (add-octets (string-to-null-terminated-octet value) array))) ; value string, null terminated
      (call-next-method key value :array array :type type :encoder #'encode-value))))

(defmethod bson-encode( (key string) (value integer) &key (array nil array-supplied-p) (size 10 size-supplied-p) 
		       (type +bson-data-int32+) (encoder nil))
  (declare (ignore encoder) (ignore type))
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value32(array)
	       (add-octets (int32-to-octet value ) array)) ; value converted to 32 bits
	     (encode-value64(array)
      	       (add-octets (int64-to-octet value ) array))) ; value converted to 64 bits
      (if (> 32 (integer-length value)) 
	  (call-next-method key value :array array :type +bson-data-int32+ :encoder #'encode-value32)
	  (call-next-method key value :array array :type +bson-data-long+ :encoder #'encode-value64)))))
      
(defmethod bson-encode( (key string) (value float) &key (array nil array-supplied-p) (size 10 size-supplied-p) 
		       (type +bson-data-number+) (encoder nil))
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value(array) 
	       (add-octets (int64-to-octet (encode-double-float-bits value)) array))) ;convert float to octet
      (call-next-method key value :array array :type type :encoder (if encoder encoder #'encode-value)))))


(defmethod bson-encode( (key string) (value (eql t)) &key (array nil array-supplied-p) (size 10 size-supplied-p) 
		       (type +bson-data-boolean+) (encoder nil))
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value (array)
	       (add-octets (byte-to-octet (bool-to-byte value)) array)))    ; add value
      (call-next-method key value :array array :type type :encoder (if encoder encoder #'encode-value)))))


(defmethod bson-encode( (key string) (value (eql nil)) &key (array nil array-supplied-p) (size 10 size-supplied-p) 
		       (type +bson-data-boolean+) (encoder nil))
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value (array)
	       (add-octets (byte-to-octet (bool-to-byte value)) array)))    ; add value
      (call-next-method key value :array array :type type :encoder (if encoder encoder #'encode-value)))))

(defmethod bson-encode( (key string) (value array) &key (array nil array-supplied-p) 
		       (size 10 size-supplied-p) 
		       (type +bson-data-object+) (encoder nil))
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value (array)
	       (add-octets value array)))    ; add value
      (call-next-method key value :array array :type type :encoder (if encoder encoder #'encode-value)))))


(defmethod bson-encode ( (key string) (value bson-oid) &key (array nil array-supplied-p) (size 10 size-supplied-p) 
			(type +bson-data-oid+) (encoder nil) )
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value (array)
	       (add-octets (_id value) array))) ; twelf byte oid
      (call-next-method key value :array array :type type :encoder (if encoder encoder #'encode-value)))))


;;(defgeneric bson-encode-document (document)   
;;  (:documentation "encode a document"))

;;(defmethod bson-encode-document ( (document array) )
;;  document)

;;(defmethod bson-encode-document ( (document document) )
;;  (setf (gethash "_id" (elements document)) (_id document))
;;  (bson-encode-ht (elements document) ))

;; Something like this SHOULD be made to work !
;; The issue is that bson-encode returns 
;;  (bson-encode-ht (elements document) :array (bson-encode "_id" (_id document))))



(defgeneric bson-type-extract (code array) 
  (:documentation "return data and the remaining array"))

(defmethod bson-type-extract (code array)
  (format t "~% code : ~A ~%" code)
  (format t "~% array : ~A ~% " array)
  (values array (make-octet-vector 1 :init-fill 1)))

(defmethod bson-type-extract ( (code (eql +bson-data-number+)) array)
  (values (decode-double-float-bits (octet-to-int64 (subseq array 0 8))) (subseq array 8)))
 
(defmethod bson-type-extract ( (code (eql +bson-data-string+)) array)
  (let* ((size (octet-to-int32 (subseq array 0 4)))
	 (str  (null-terminated-octet-to-string (subseq array 4 (+ 4 size)) size))
	 (rest (subseq array (+ 4 size))))
    (values str rest)))

(defmethod bson-type-extract ( (code (eql +bson-data-oid+)) array)
  (values (make-bson-oid :oid (subseq array 0 12)) (subseq array 12)))

(defmethod bson-type-extract ( (code (eql +bson-data-boolean+)) array)
  (values (byte-to-bool (octet-to-byte (subseq array 0 1))) (subseq array 1)))

(defmethod bson-type-extract ( (code (eql +bson-data-int32+)) array)
  (values (octet-to-int32 (subseq array 0 4)) (subseq array 4)))

(defmethod bson-type-extract ( (code (eql +bson-data-long+)) array)
  (values (octet-to-int64 (subseq array 0 8)) (subseq array 8)))

#|
  Compound Types : arrays, objects 
  This deals with the extraction of data objects, arrays and general replies
  Should be merged because they're all similar !
|#

(defun array@eoo (array) 
  (and (eql 1 (length array)) (eql (aref array 0) 0)))

(defun extract-elements (array accum)
  ;;(format t "array : ~A | accum ~A ~%" array accum)
  (if (array@eoo array)
      accum
      (progn 
	(let* ((obj-type (octet-to-byte  (subseq array 0 1) ))
	       (eos      (1+ (position 0  (subseq array 1))))
	       (key      (null-terminated-octet-to-string (subseq array 1 (+ 1 eos)) eos))
	       (buffer   (subseq array (+ 1 eos))))
	  (multiple-value-bind (value rest)  (bson-type-extract obj-type buffer) 
	    (extract-elements rest (cons (list obj-type key value ) accum)))))))

(defun to-element (array) 
  (let* ((obj-size (octet-to-int32 (subseq array 0 4) ))
	 (rest     (subseq array 4)))
    (list obj-size (extract-elements rest () ))))

(defun to-hash-map (array)
  (labels ((to-element* (array)
	     (let ((elem (to-element array)))
	       (values (car elem) (cadr elem)))))  
    (let ((ht (make-hash-table :test 'equal)))
      (multiple-value-bind (size array) (to-element* array)
	(declare (ignore size))
      ;; remove the type prefix
      (mapcar (lambda (x) (setf (gethash (car x) ht) (cadr x)))  (mapcar #'cdr array)))
      ht)))

(defun array->ht (array) 
  (to-hash-map array))

(defun to-document (array)
    (ht->document (to-hash-map array)))

(defun code-key-rest (array)
  (let* ((obj-type (octet-to-byte  (subseq array 0 1) ))
	 (eos      (1+ (position 0  (subseq array 1))))
	 (key      (null-terminated-octet-to-string (subseq array 1 (+ 1 eos)) eos))
	 (rest     (subseq array (+ 1 eos))))
    (values obj-type key rest)))


(defun array@end (array) 
  (if (array@eoo array)
      0
      (length array)))

;;this is the same as the 'main' extraction routine to-elements above; so the two should merge..
(defmethod bson-type-extract ( (code (eql +bson-data-object+)) array)
  (let*  ((accum ())
	  (array-size   (octet-to-int32 (subseq array 0 4) ))
	  (array-buffer (subseq array 4 array-size))
	  (rest         (subseq array array-size)))
    (do () ((zerop (array@end array-buffer) ))
      (multiple-value-bind (type key rest) (code-key-rest array-buffer)    
	(multiple-value-bind (value remainder) (bson-type-extract type rest)
	  (setf array-buffer remainder)
	  (push (list type key value) accum))))
    (let ((ht (make-hash-table :test 'equal)))
      (mapcar (lambda (x) (setf (gethash (car x) ht) (cadr x)))  (mapcar #'cdr accum))
      (values (ht->document ht) rest))))

(defmethod bson-type-extract ( (code (eql +bson-data-array+)) array)
  (let* ((accum ())
	 (array-size   (octet-to-int32 (subseq array 0 4) ))
	 (array-buffer (subseq array 4 array-size))
	 (rest         (subseq array array-size)))
    (do () ((zerop (array@end array-buffer) ))
      (multiple-value-bind (type key rest) (code-key-rest array-buffer)    
	(declare (ignore key))
	(multiple-value-bind (value remainder) (bson-type-extract type rest)
	  (setf array-buffer remainder)
	  ;(push (list key value) accum))))
	  (push value accum))))
    (values (reverse accum) rest)))






  

