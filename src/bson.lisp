(in-package :cl-mongo)

(defconstant +default-array-size+ 100   "size of default array in the encoder")

(defconstant +bson-data-number+       1   "bson number encoding")
(defconstant +bson-data-string+       2   "bson string encoding")
(defconstant +bson-data-object+       3   "bson data array; bson object")
(defconstant +bson-data-array+        4   "bson array")
(defconstant +bson-data-binary+       5   "bson binary")
(defconstant +bson-data-undefined+    6   "undefined/deprecated")
(defconstant +bson-data-oid+          7   "bson oid encoding")
(defconstant +bson-data-boolean+      8   "bson boolean encoding")
(defconstant +bson-data-date+         9   "bson date encoding")
(defconstant +bson-data-null+         10  "bson null encoding")
(defconstant +bson-data-regex+        11  "bson regex encoding")
(defconstant +bson-data-dbpointer+    12  "bson db pointer encoding/deprecated")
(defconstant +bson-data-code+         13  "bson code encoding")
(defconstant +bson-data-symbol+       14  "bson symbol encoding")
(defconstant +bson-data-code_w_s+     15  "bson javascript with scope")
(defconstant +bson-data-int32+        16  "bson 32 bit int encoding")
(defconstant +bson-data-timestamp+    17  "bson 32 bit int encoding")
(defconstant +bson-data-long+         18  "bson 64 bit int encoding")
(defconstant +bson-data-min-key+      255 "bson data ultimate minimum")
(defconstant +bson-data-max-key+      127 "bson data ultimate maximum")

#|
  bson-encode encodes a complete bson object.
  It includes the obj_size at the start and the terminating null at the end.
  When creating composites these need to be skipped or removed..
|#


(defun set-array-length(array &key (start 0 start-supplied-p))
  (let* ((head  (if start-supplied-p start (fill-pointer array))))      ; save the stack pointer
    (set-octets head (int32-to-octet (- (length array) head)) array))) ; set length    

(defun null-terminate-array(array)
  (add-octets (byte-to-octet 0) array))                       ; ending nul

(defgeneric bson-encode(key value &key)
  (:documentation "encode a bson data element"))

(defun bson-encode-array (key &key array type encoder)
  (let* ((head  (fill-pointer array)))                         ; save the stack pointer
    (add-octets (int32-to-octet 0)    array)                   ; length, set to zero
    (add-octets (byte-to-octet  type) array)                   ; data element code
    (add-octets (string-to-null-terminated-octet key) array)   ; key
    (funcall encoder array)                                    ; call type specific encoder
    (add-octets (byte-to-octet 0) array)                       ; ending nul
    (set-octets head (int32-to-octet (- (length array) head)) array)      ; set length
    array))

(defmethod bson-encode ((key string) (value t) &key array type encoder)
  (bson-encode-array key :array array :type type :encoder encoder))

;;; empty object
(defmethod bson-encode((key (eql nil)) (value (eql nil)) &key (array nil))
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (add-octets (int32-to-octet 0) array)                   ; length, set to zero
    (add-octets (byte-to-octet 0) array)                  ; ending nul
    (set-octets 0 (int32-to-octet (length array)) array)      ; set length
    array))

;;used in arrays, so you can just say (bson-encode 1 ...)
;;
(defmethod bson-encode ((key integer) value &key) 
  (bson-encode (format nil "~A" key) value)) 

(defmethod bson-encode ((key string) (value bson-binary-base) &key (array nil) (type +bson-data-binary+))
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       ;; length of the data array
	       (add-octets (int32-to-octet (length (data value))) array)
	       (add-octets (byte-to-octet (type-id value)) array)
	       (add-octets (data value) array))) 
      (call-next-method key value :array array :type type :encoder #'encode-value))))

(defmethod bson-encode ((key string) (value bson-binary) &key (array nil) (type +bson-data-binary+))
  "the ordinary binary type as some structure in that the length of the binary array is encoded"
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (add-octets (int32-to-octet (+ 4 (length (data value)))) array)
	       (add-octets (byte-to-octet (type-id value)) array)
	       (add-octets (int32-to-octet (length (data value))) array)
	       (add-octets (data value) array))) 
      (bson-encode key value :array array :type type :encoder #'encode-value))))

;
; The array type is the parent class of other types like string. So see if a type and encoder is
; passed in and pass it along..
(defmethod bson-encode((key string) (value array) &key (array nil) (type nil) (encoder nil))
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (add-octets value array)))    ; add value
      (call-next-method key value :array array :type (or type +bson-data-object+) :encoder (or encoder #'encode-value)))))

;;-------------------------------------------------------------------------------------------------------

(defmethod bson-encode ((key string) (value (eql t))  &key (array nil))
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (add-octets (byte-to-octet (bool-to-byte value)) array)))    ; add value
      (bson-encode-array key :array array :type +bson-data-boolean+ :encoder #'encode-value))))

(defmethod bson-encode ((key string) (value (eql nil))  &key (array nil))
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       array))
      (bson-encode-array key :array array :type +bson-data-null+ :encoder #'encode-value))))

(defmethod bson-encode((key string) (value bson-time) &key (array nil)) 
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-date(array)
      	       (add-octets (int64-to-octet (raw value)) array))) ; value converted to 64 bits
      (bson-encode-array key :array array :type +bson-data-date+ :encoder #'encode-date))))


(defmethod bson-encode ((key string) (value bson-code) &key (array nil))
  (bson-encode key (code value) :array array :type +bson-data-code+))

(defmethod bson-encode ((key string) (value symbol) &key (array nil))
  (bson-encode key (string value) :array array :type +bson-data-symbol+))

(defmethod bson-encode ((key string) (value bson-oid) &key (array nil))
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (add-octets (_id value) array))) ; twelf byte oid
      (bson-encode-array key :array array :type +bson-data-oid+ :encoder #'encode-value))))

(defmethod bson-encode ((key string) (value string) &key (array nil) (type +bson-data-string+))
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       (let ((enc-val (string-to-null-terminated-octet value)))
		 ;; length of the value string
		 (add-octets (int32-to-octet (length enc-val)) array)   
		 ;; value string, null terminated
		 (add-octets enc-val array)))) 
      (bson-encode-array key :array array :type type :encoder #'encode-value))))

(defmethod bson-encode((key string) (value integer) &key (array nil)) 
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value32(array)
	       (add-octets (int32-to-octet value) array)) ; value converted to 32 bits
	     (encode-value64(array)
      	       (add-octets (int64-to-octet value) array))) ; value converted to 64 bits
      (if (> 32 (integer-length value)) 
	  (bson-encode-array key :array array :type +bson-data-int32+ :encoder #'encode-value32)
	  (bson-encode-array key :array array :type +bson-data-long+ :encoder #'encode-value64)))))

(defmethod bson-encode ((key string) (value float) &key (array nil))
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value(array) 
	       (add-octets (int64-to-octet (encode-double-float-bits value)) array))) ;convert float to octet
      (bson-encode-array key :array array :type +bson-data-number+ :encoder #'encode-value))))

(defmethod bson-encode ((key string) (value bson-regex) &key (array nil))
  (let ((array (or array (make-octet-vector +default-array-size+))))
    (labels ((encode-value (array)
	       ;; regex string, null terminated
	       (add-octets (string-to-null-terminated-octet (regex value)) array)
               ;; options string, null terminated
	       (add-octets (string-to-null-terminated-octet (options value)) array)))
      (bson-encode-array key :array array :type +bson-data-regex+ :encoder #'encode-value))))
