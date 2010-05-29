(in-package :cl-mongo)

(defconstant +default-array-size+ 100   "size of default array in the encoder")

(defconstant +bson-data-number+   1   "bson number encoding")
(defconstant +bson-data-string+   2   "bson string encoding")
(defconstant +bson-data-object+   3   "bson data array; bson object")
(defconstant +bson-data-array+    4   "bson array")
(defconstant +bson-data-binary+   5   "bson binary")
(defconstant +bson-data-oid+      7   "bson oid encoding")
(defconstant +bson-data-boolean+  8   "bson boolean encoding")
(defconstant +bson-data-date+     9   "bson date encoding")
(defconstant +bson-data-null+     10  "bson null encoding")
(defconstant +bson-data-regex+    11  "bson regex encoding")
(defconstant +bson-data-code+     13  "bson code encoding")
(defconstant +bson-data-int32+    16  "bson 32 bit int encoding")
(defconstant +bson-data-long+     18  "bson 64 bit int encoding")

;function my_test() {
;    db.foo.find().forEach(function (obj) {delete obj.k;db.foo.save(obj);});

#|
support for data-symbol was removed b/c in the current implementation it
clashed with the encoding for booleans..
|#
(defconstant +bson-data-symbol+   14  "bson symbol encoding")

#|
  bson-encode encodes a complete bson object.
  It includes the obj_size at the start and the terminating null at the end.
  When creating composites these need to be skipped or removed..
|#


(defun set-array-length(array &key (start 0 start-supplied-p))
  (let* ((head  (if start-supplied-p start (fill-pointer array))))      ; save the stack pointer
    (set-octets head (int32-to-octet (- (length array) head) ) array))) ; set length    

(defun null-terminate-array(array)
  (add-octets (byte-to-octet 0) array))                       ; ending nul

(defgeneric bson-encode(key value &key )
  (:documentation "encode a bson data element"))

(defmethod bson-encode( (key string) (value t) &key array type encoder)
  ;(format t "89here~%")
  (let* ((head  (fill-pointer array)))                         ; save the stack pointer
    (add-octets (int32-to-octet 0)    array)                   ; length, set to zero
    (add-octets (byte-to-octet  type) array)                   ; data element code
    (add-octets (string-to-null-terminated-octet key) array)   ; key
    (funcall encoder array)                                    ; call type specific encoder
    (add-octets (byte-to-octet 0) array)                       ; ending nul
    (set-octets head (int32-to-octet (- (length array) head) ) array)      ; set length    
    array))

		  
;;; empty object
(defmethod bson-encode( (key (eql nil)) (value (eql nil)) &key (array nil))
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (add-octets (int32-to-octet 0)    array)                   ; length, set to zero
    (add-octets (byte-to-octet 0)     array)                  ; ending nul
    (set-octets 0 (int32-to-octet (length array)  ) array)      ; set length    
    array))

;;used in arrays, so you can just say (bson-encode 1 ...)
;;
(defmethod bson-encode ( (key integer) value &key ) 
  (bson-encode (format nil "~A" key) value)) 
	       
(defmethod bson-encode ( (key string) (value string) &key (array nil) (type +bson-data-string+) )
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (let ((enc-val (string-to-null-terminated-octet value)))
		 ;; length of the value string
		 (add-octets (int32-to-octet (length enc-val)) array)   
		 ;; value string, null terminated
		 (add-octets enc-val array)))) 
      (call-next-method key value :array array :type type :encoder #'encode-value))))

(defmethod bson-encode ( (key string) (value bson-binary-base) &key (array nil) (type +bson-data-binary+) )
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       ;; length of the data array
	       (add-octets (int32-to-octet (length (data value))) array)
	       (add-octets (byte-to-octet (type-id value)) array)
	       (add-octets (data value) array))) 
      (call-next-method key value :array array :type type :encoder #'encode-value))))

(defmethod bson-encode ( (key string) (value bson-binary) &key (array nil) (type +bson-data-binary+) )
  "the ordinary binary type as some structure in that the length of the binary array is encoded"
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (add-octets (int32-to-octet (+ 4 (length (data value)))) array)
	       (add-octets (byte-to-octet (type-id value)) array)
	       (add-octets (int32-to-octet (length (data value))) array)
	       (add-octets (data value) array))) 
      (bson-encode key value :array array :type type :encoder #'encode-value))))

(defmethod bson-encode ( (key string) (value bson-code) &key (array nil))
  (bson-encode key (code value) :array array :type +bson-data-code+))

;(defmethod bson-encode ( (key string) (value symbol) &key (array nil) )
;  (bson-encode key (string value) :array array :type +bson-data-symbol+ ))

(defmethod bson-encode( (key string) (value integer) &key (array nil)) 
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value32(array)
	       (add-octets (int32-to-octet value ) array)) ; value converted to 32 bits
	     (encode-value64(array)
      	       (add-octets (int64-to-octet value ) array))) ; value converted to 64 bits
      (if (> 32 (integer-length value)) 
	  (call-next-method key value :array array :type +bson-data-int32+ :encoder #'encode-value32)
	  (call-next-method key value :array array :type +bson-data-long+ :encoder #'encode-value64)))))
      
(defmethod bson-encode( (key string) (value float) &key (array nil) )
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value(array) 
	       (add-octets (int64-to-octet (encode-double-float-bits value)) array))) ;convert float to octet
      (call-next-method key value :array array :type +bson-data-number+ :encoder #'encode-value))))

(defmethod bson-encode( (key string) (value bson-time) &key (array nil)) 
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-date(array)
      	       (add-octets (int64-to-octet (raw value) ) array))) ; value converted to 64 bits
      (call-next-method key value :array array :type +bson-data-date+ :encoder #'encode-date))))

      

(defmethod bson-encode-boolean( (key string) value  &key (array nil) )
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (add-octets (byte-to-octet (bool-to-byte value)) array)))    ; add value
      (bson-encode key value :array array :type +bson-data-boolean+ :encoder #'encode-value))))

(defmethod bson-encode ( (key string) (value (eql t))  &key (array nil) )
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (add-octets (byte-to-octet (bool-to-byte value)) array)))    ; add value
      (call-next-method key value :array array :type +bson-data-boolean+ :encoder #'encode-value))))


;(defmethod bson-encode ( (key string) (value (eql nil))  &key (array nil) )
;  (let ((array (or array (make-octet-vector +default-array-size+))))
;    (labels ((encode-value (array)
;	       (add-octets (byte-to-octet (bool-to-byte value)) array)))    ; add value
;      (call-next-method key value :array array :type +bson-data-boolean+ :encoder #'encode-value))))

;
; nil is the opposite of t, and is already mapped as a boolean. Also, there a seperate encoder
; for symbols, so something like the below won't work (and propably doesn't need to )

;(defmethod bson-encode ( (key string) (value (eql 'void))  &key (array nil) )

(defmethod bson-encode ( (key string) (value (eql nil))  &key (array nil) )
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       array))
      (call-next-method key value :array array :type +bson-data-null+ :encoder #'encode-value))))

(defmethod bson-encode ( (key string) (value bson-regex) &key (array nil) (type +bson-data-regex+) )
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       ;; regex string, null terminated
	       (add-octets (string-to-null-terminated-octet (regex value)) array)
               ;; options string, null terminated
	       (add-octets (string-to-null-terminated-octet (options value) ) array)
	       ))
      (call-next-method key value :array array :type +bson-data-regex+ :encoder #'encode-value))))

;
; The array type is the parent class of other types like string. So see if a type and encoder is
; passed in and pass it along..
(defmethod bson-encode( (key string) (value array) &key (array nil) (type nil) (encoder nil) )
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (add-octets value array)))    ; add value
      (call-next-method key value :array array :type (or type +bson-data-object+) 
			:encoder (or encoder #'encode-value)))))


(defmethod bson-encode ( (key string) (value bson-oid) &key (array nil) )
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (add-octets (_id value) array))) ; twelf byte oid
      (call-next-method key value :array array :type +bson-data-oid+ :encoder #'encode-value))))



(defgeneric bson-decode (code array) 
  (:documentation "return data and the remaining array"))

(defmethod bson-decode ( (code t) (array t) )
;  (format t "~% code : ~A ~%" code)
;  (format t "~% array : ~A ~% " array)
  (values array (make-octet-vector 1 :init-fill 1)))

(defmethod bson-decode ( (code (eql +bson-data-number+)) array)
  (values (decode-double-float-bits (octet-to-int64 (subseq array 0 8))) (subseq array 8)))

(defmethod to-cstring (array)
  (let* ((size (octet-to-int32 (subseq array 0 4)))
	 (str  (null-terminated-octet-to-string (subseq array 4 (+ 4 size)) size))
	 (rest (subseq array (+ 4 size))))
    (values str rest)))

(defmethod bson-decode ( (code (eql +bson-data-string+)) array)
  (to-cstring array))

;  (let* ((size (octet-to-int32 (subseq array 0 4)))
;	 (str  (null-terminated-octet-to-string (subseq array 4 (+ 4 size)) size))
;	 (rest (subseq array (+ 4 size))))
 ;   (values str rest)))

(defmethod bson-decode ( (code (eql +bson-data-code+)) array)
  (to-cstring array))
 


(defmethod bson-decode ( (code (eql +bson-data-binary+)) array)
  (let* ((type (octet-to-byte  (subseq array 4 5)))
	 (size (if (eql type #x02) (octet-to-int32 (subseq array 5 9)) (octet-to-int32 (subseq array 0 4))))
	 (offset (if (eql type #x02) 9 5))
	 (binary  (bson-binary type (subseq array offset (+ offset size))))
	 (rest (subseq array (+ offset size))))
    (values binary rest)))

(defmethod bson-decode ( (code (eql +bson-data-symbol+)) array)
  (multiple-value-bind (str rest) (bson-decode +bson-data-string+ array)
    (values (intern str) rest)))

(defmethod bson-decode ( (code (eql +bson-data-oid+)) array)
  (values (make-bson-oid :oid (subseq array 0 12)) (subseq array 12)))

(defmethod bson-decode ( (code (eql +bson-data-boolean+)) array)
  (values (byte-to-bool (octet-to-byte (subseq array 0 1))) (subseq array 1)))

(defmethod bson-decode ( (code (eql +bson-data-int32+)) array)
  (values (octet-to-int32 (subseq array 0 4)) (subseq array 4)))

(defmethod bson-decode ( (code (eql +bson-data-long+)) array)
  (values (octet-to-int64 (subseq array 0 8)) (subseq array 8)))

(defmethod bson-decode ( (code (eql +bson-data-null+)) array)
  (values nil array))

(defmethod bson-decode ( (code (eql +bson-data-date+)) array)
  (values (make-bson-time (octet-to-uint64 (subseq array 0 8))) (subseq array 8)))

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
	  (multiple-value-bind (value rest)  (bson-decode obj-type buffer) 
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
(defmethod bson-decode ( (code (eql +bson-data-object+)) array)
  (let*  ((accum ())
	  (array-size   (octet-to-int32 (subseq array 0 4) ))
	  (array-buffer (subseq array 4 array-size))
	  (rest         (subseq array array-size)))
    (do () ((zerop (array@end array-buffer) ))
      (multiple-value-bind (type key rest) (code-key-rest array-buffer)    
	(multiple-value-bind (value remainder) (bson-decode type rest)
	  (setf array-buffer remainder)
	  (push (list type key value) accum))))
    (let ((ht (make-hash-table :test 'equal)))
      (mapcar (lambda (x) (setf (gethash (car x) ht) (cadr x)))  (mapcar #'cdr accum))
      (values (ht->document ht) rest))))

(defmethod bson-decode ( (code (eql +bson-data-array+)) array)
  (let* ((accum ())
	 (array-size   (octet-to-int32 (subseq array 0 4) ))
	 (array-buffer (subseq array 4 array-size))
	 (rest         (subseq array array-size)))
    (do () ((zerop (array@end array-buffer) ))
      (multiple-value-bind (type key rest) (code-key-rest array-buffer)    
	(declare (ignore key))
	(multiple-value-bind (value remainder) (bson-decode type rest)
	  (setf array-buffer remainder)
	  ;(push (list key value) accum))))
	  (push value accum))))
    (values (reverse accum) rest)))
