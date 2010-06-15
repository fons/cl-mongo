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


;;
;; Reads the byte stream and constructs a list of triples of (type key buffer) for each 
;; distinct bson object on the stream. The elements
;;
(defconstant --ee-get-type--        0 "initialize and read the type from the byte stream")
(defconstant --ee-get-key--         1 "read the key from the byte stream")
(defconstant --ee-get-buffer-size-- 2 "read the buffer size for variable sized data")
(defconstant --ee-get-element--     3 "read the element data")

(defun extract-elements(base-array)
  (let ((array  (subseq base-array 4)) ;; skip the size indicator
	(key    (make-octet-vector 50))
	(szbuf  (make-octet-vector 4))
	(buffer (make-octet-vector 10000))
	(type   nil)
	(state  --ee-get-type--)
	(elsize 0)
	(lst () ))
    (labels ((init-vars()
	       (setf (fill-pointer key)    0)
	       (setf (fill-pointer szbuf)  0)
	       (setf (fill-pointer buffer) 0)
	       (setf elsize 0)
	       (setf type   nil)
	       (setf state  --ee-get-type--))
	     ; store triple on the triple list
	     (store-triple ()
	       (push (list type (babel:octets-to-string key) (copy-seq buffer)) lst))
	     ; extract type
	     ; since this is the start initialization happens as well
	     ;; transition to the key-read state
	     (get-type (c)
	       (init-vars)
	       (setf type  c)
	       (setf state --ee-get-key--))
	     ;; for variable types we need to get the size first
	     (nxt-state (type)
	       (cond ( (eql type +bson-data-string+)   --ee-get-buffer-size--) 
		     ( (eql type +bson-data-object+)   --ee-get-buffer-size--)
		     ( (eql type +bson-data-array+ )   --ee-get-buffer-size--)
		     ( (eql type +bson-data-binary+)   --ee-get-buffer-size--)
		     ( (eql type +bson-data-code+)     --ee-get-buffer-size--)
		     ( t                               --ee-get-element--)))
	     ;; Return the size based on the type.
	     ;; For variable size types we need to get the size first. That's usually
	     ;; stored in an integer array.
	     (nxt-size (type)
	       (cond ( (eql type +bson-data-number+)   8) 
		     ( (eql type +bson-data-oid+)     12)
		     ( (eql type +bson-data-boolean+)  1)
		     ( (eql type +bson-data-long+)     8)
		     ( (eql type +bson-data-int32+)    4)
		     ( (eql type +bson-data-date+)     8)
		     ( (eql type +bson-data-null+)     0)
		     ( t                               4)))
	     ;; add chars to the key buffer, incl 0; then transition to the buffer reader
	     (get-key (c)
	       (if (eql 0 c) (progn
			       (setf state (nxt-state type))
			       (setf elsize (nxt-size type)))
		   (add-to-array c key)))
	     ;; for arrays and such we 've already read 4 bytes; we include the
	     ;; eoo as well
	     (normalize-size (size)
	       (cond ((eql type +bson-data-string+) size)
		     ((eql type +bson-data-code+)   size)
		     ((eql type +bson-data-binary+) (+ 1 size)) ;include type byte
		     (t        (- size 4))))
	     ;; variable elements are preceded by an integer stating their size.
	     ;; that needs to be decoded first..
	     (get-size (c)
	       (add-to-array c buffer)
	       (add-to-array c szbuf)
	       (decf elsize)
	       (when (zerop elsize) 
		 (progn 
		   (setf elsize (normalize-size (octet-to-int32 szbuf)))
		   (setf state  --ee-get-element--))))
	     ;;read element data
	     (get-element(c)
	       (add-to-array c buffer)
	       (decf elsize)
	       (when (zerop elsize) 
		 (progn
		   (setf state --ee-get-type--)
		   (store-triple))))
	     ;; this is where the state machine is executed
	     (parse-stream (c)
	       (cond ((eql state --ee-get-type--)        (get-type c))
		     ((eql state --ee-get-key--)         (get-key  c))
		     ((eql state --ee-get-buffer-size--) (get-size c))
		     ((eql state --ee-get-element--)     (get-element c))
		     ( t            (format t "state not recognized...~%"))))
	     ;;apply the decoders to the bson elements..
	     (codecs (triple)
	       (let ((type   (car triple))
		     (key    (cadr triple))
		     (buffer (caddr triple)))
		 (multiple-value-bind (value rest) (bson-decode type buffer)
		   (declare (ignore rest))
		   (list key value)))))
      (progn
	(map nil #'parse-stream  array)
	(mapcar #'codecs (nreverse lst))) )))

;;
;; This is the finalizer used to create documents from replies
;;

(defun to-document (array)
  ;; split the decoded bson element array into the "_id" and the rest.
  ;; Assumes the the first element is in fact the doc id. If not, cdrs down
  ;; the list until it's found..
  (labels ((split-on-_id (lst)
	     (if (string= "_id" (car (car lst)) )
		 (values (car lst) (cdr lst))
		 (progn 
		   (let ((l () )
			 (_id nil))
		     (dolist (vals lst)
		       (if (string= (car vals) "_id") 
			   (setf _id vals)
			   (push vals l)))
		     (values _id l))))))
    (multiple-value-bind (_id rest)  (split-on-_id (extract-elements array))
      (let ((doc (make-document :oid (cadr _id) :size (length rest))))
	(mapcar (lambda (x) (add-element (car x) (cadr x) doc)) rest)
	doc))))


(defmethod bson-decode ( (code (eql +bson-data-object+)) array)
  (extract-elements array))

(defmethod bson-decode ( (code (eql +bson-data-array+)) array)
  (mapcar (lambda (p) (cadr p)) (extract-elements array)))

