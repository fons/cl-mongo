(asdf:oos 'asdf:load-op 'uuid)
(require 'babel)

(defun make-octet-vector(sz &key (init-fill 0) )
  (make-array sz :element-type '(unsigned-byte 8)  :initial-element 0 :fill-pointer init-fill :adjustable t))

(defun make-fixed-size-octet-vector(sz)
  (make-array sz :element-type '(unsigned-byte 8)  :fill-pointer 0 :initial-element 0))

;;(append-to-octet-vector (vector-pop source) (vector-push-extend (vector-pop source) target)))
(defun append-to-octet-vector(source target)
  (assert (adjustable-array-p target))
  ;;(format t "~% (~A : ~A)" source target)
  (cond ( (< 0 (length source) ) (append-to-octet-vector (vector-pop source) target))
	( t target)))

(defun to-octet(val size)
  (let ((ov (make-fixed-size-octet-vector size)))
    (dotimes (position size)
      (let ((pos (* 8 position)))
	(vector-push (ldb (byte 8 pos) val) ov)))
    ov))


(defun int32-to-octet(val)
  (to-octet val 4))

(defun int64-to-octet(val)
  (to-octet val 8))

(defun byte-to-octet(val)
  (to-octet val 1))

(defun bool-to-byte(val)
  (if (eql t val) 1 0))

(defconstant +max-uint32+ 4294967296           "max + 1 value of unsigned 32 bit integer")
(defconstant +max-uint64+ 18446744073709551616 "max + 1 value of unsigned 64 bit integer")

(defun to-signed-value (value &key size max-uint )
  (cond ( (and (< 0 value) (logbitp size value) ) (- value max-uint) )
	( (<= max-uint value) 0)
	( t                    value)))

(defun to-val(vec size)
  (let ((value 0))
    (dotimes (position size)
      (let ((pos (* 8 position)))
	(setf (ldb (byte 8 pos) value) (aref vec position))))
    value))

(defun octet-to-int32 (vec)
  (to-signed-value (to-val vec 4) :size 31 :max-uint +max-uint32+)) 

(defun octet-to-int64 (vec)
  (to-signed-value (to-val vec 8) :size 63 :max-uint +max-uint64+)) 


(defun octet-to-byte (vec)
  (to-val vec 1))

(defun byte-to-bool (val)
  (if (eql 1 val) t nil))

(defun gtb ( loc )
  (let ((res 255))
    (dotimes (var loc)
      (setf res (* res 256)))
    res ))

(defun gti (size) 
  (let ((value 0))
    (dotimes (i size)
      (setf value (+ value (gtb i))))
    value))

(defun test-int32-code-decode()
  (let ((value (gti 4)))
    (format t "value ~A ~%" value)
    (octet-to-int32 (int32-to-octet value))))

    
(defun test-int64-code-decode()
  (let ((value (gti 8)))
    (format t "value ~A ~%" value)
    (octet-to-int64 (int64-to-octet value))))
	
;; s/b macro
(defun string-to-null-terminated-octet(str)
  (babel:string-to-octets (concatenate 'string str (string #\null))))

(defun null-terminated-octet-to-string (arr length)
  (babel:octets-to-string (subseq arr 0 (- length 1))))

(defun add-integer(int32 array) 
  (assert (integerp int32))
  (let ((oct (int32-to-octet int32)))
    (append-to-octet-vector oct array)))

(defun add-to-array(elem arr)
  (vector-push-extend elem arr)
  arr)
;;sb macro
(defun positive(val)
  (if (< 0 val) t nil))

(defun pop-from-array(arr)
  (when (positive (fill-pointer arr)) (vector-pop arr)))

(defun pop-to(src tar)
  (let ((elem (pop-from-array src)))
    (cond ((null elem)  tar)
	  (t (pop-to src (add-to-array elem tar))))))

(defun add-octets*(source target)
  (pop-to (nreverse source) target))

(defun add-octets(source target &key (start 0) (from-end 0))
  (let ((to (- (length source) start from-end)))
    (dotimes (index to)
      (vector-push-extend (aref source (+ start index) ) target))
  target))

(defun set-octets(start source target)
  (dotimes (ind (length source) )
    (let ((index (+ start ind)))
      (setf (aref target index) (aref source ind))))
  target)

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

#|
  
|#
;;:initform (uuid:uuid-to-byte-array (uuid:make-v4-uuid)))))

(defclass bson-oid ()
  ((id :reader id :initarg :oid))
  (:default-initargs
   :oid (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))

(defun make-bson-oid( &key (oid nil oid-supplied-p))
  (if oid-supplied-p
      (make-instance 'bson-oid :oid oid)
      (make-instance 'bson-oid)))

(defmethod print-object ((bson-oid bson-oid) stream)
  (format stream "~S [~A] ~%" (type-of bson-oid) 
	  (if (slot-boundp bson-oid 'id) 
	      (id bson-oid)
	      "no id set..")))

(defgeneric _id (bson-oid) 
  (:documentation 
   "return a 12 byte array, irrespective of the internals of bson-oid, which may have a larger  array")) 

(defmethod _id ((bson-oid bson-oid))
  (subseq (id bson-oid) 0 12))

(defmethod bson-encode ( (key string) (value bson-oid) &key (array nil array-supplied-p) (size 10 size-supplied-p) 
			(type +bson-data-oid+) (encoder nil) )
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value (array)
	       (add-octets (_id value) array))) ; twelf byte oid
      (call-next-method key value :array array :type type :encoder (if encoder encoder #'encode-value)))))


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
(defclass bson-array()
  ((array :initarg :array :accessor array)
   (index-array :initarg :index-array :accessor index-array)))

;; terminate the bson array with a nul and
;; encode the length in the first for bytes..

(defun normalize-array(array)
    (null-terminate-array array)
    (set-array-length array :start 0))

(defun normalize-bson-array(array)
  (normalize-array (array array)))
  
(defun make-bson-array(&key (size 10))
  (let ((array (make-instance 'bson-array 
			      :array (add-octets (int32-to-octet 0) (make-octet-vector (+ size 4)))
			      :index-array (make-octet-vector 5))))
    (normalize-bson-array array)
    array))

(defgeneric bson-array-push (element array)
  (:documentation "push element to arrray"))

(defmethod bson-array-push (element (array bson-array))
  (let ((key (format nil "~D" (fill-pointer (index-array array)))))
    (pop-from-array (array array)) ; remove terminating null
    (vector-push-extend (fill-pointer (array array)) (index-array array))
    ; this skips the first four bytes returned from the encoding which
    ; has the size of encoded object as well as the terminating nul 
    ; (which does get added on by normalize !)
    (add-octets (bson-encode key element) (array array)  :start 4 :from-end 1)
    (normalize-bson-array array)
    array))
 
(defgeneric bson-array-pop (array)
  (:documentation "pop element from arrray"))

(defmethod bson-array-pop-element ((array bson-array))
  (let* ((retval  (copy-seq (subseq (array array) (vector-pop (index-array array)))))
	 (newfill (- (fill-pointer (array array)) (length retval))))
    (setf (fill-pointer (array array)) newfill)
    (normalize-bson-array array)
    retval))

(defmethod bson-array-pop ((array bson-array))
  (if (zerop (fill-pointer (index-array array)))
      (values nil nil)
      (values (bson-array-pop-element array) (fill-pointer (index-array array)))))

(defgeneric bson-array-reset (array))

(defmethod  bson-array-reset ((array bson-array))
  (setf (fill-pointer (array array)) 0) 
  (setf (fill-pointer (index-array array)) 0) )

(defmethod print-object ((arr bson-array) stream)
  (format stream "~% ~S [~A] ~A" (type-of arr) 
	  (if (slot-boundp arr 'index-array) 
	      (index-array arr)
	      "no index array set..")
	  (if (slot-boundp arr 'array) 
	      (array arr)
	      "no array set..")))


(defmethod bson-encode( (key string) (value bson-array) &key (array nil array-supplied-p) 
		       (size 10 size-supplied-p) 
		       (type +bson-data-array+) (encoder nil))
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value (array)
	       (add-octets (array value) array)))    ; add value
      (call-next-method key value :array array :type type :encoder (if encoder encoder #'encode-value)))))


(defmethod bson-encode( (key string) (value array) &key (array nil array-supplied-p) 
		       (size 10 size-supplied-p) 
		       (type +bson-data-object+) (encoder nil))
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
    (labels ((encode-value (array)
	       (add-octets value array)))    ; add value
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
    (cond ( (encode-cons-done list stack) (car bson-array-stack))
	  ( (zerop (length list)) (bson-encode-cons (car stack) (cdr stack) 
						    (encode-cons-helper-2 bson-array-stack)))
	  ( (consp (car list)) (bson-encode-cons 
				(car list) (cons (cdr list) stack) (cons (make-bson-array) bson-array-stack)))
	  ( t  (bson-encode-cons (cdr list) stack (encode-cons-helper-1 (car list) bson-array-stack))))))

(defmethod bson-encode ( (key string) (value cons) &key (array nil array-supplied-p) 
			(size 10 size-supplied-p) 
			(type +bson-data-array+) (encoder nil))
  (declare (ignore encoder) (ignore array) (ignore type) (ignore size) 
	   (ignore size-supplied-p) (ignore array-supplied-p) )
  (bson-encode key (bson-encode-cons value () (list (make-bson-array :size (* (length value) 12))))))

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

;;  (bson-encode key (bson-encode-ht value (list (make-bson-array :size (* (hash-table-count value) 12))))))
	      

#|
  Document is a collection of key/value pairs
|#

(defclass document()
  ((elements  :initform (make-hash-table :test #'equal) :accessor elements)
   (_local_id :initarg :local :reader _local)
   (_id       :initarg :oid   :reader _id))
  (:default-initargs
   :local t
   :oid (make-bson-oid)))


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

(defun ht->document (ht) 
  (multiple-value-bind (oid oid-supplied) (gethash "_id" ht)
    (let ((doc (make-document :oid (if oid-supplied oid nil))))
      (when oid-supplied (remhash "_id" ht))
      (with-hash-table-iterator (iterator ht)
	(dotimes (repeat (hash-table-count ht))
	  (multiple-value-bind (exists-p key value) (iterator)
	    (if exists-p (add-element key value doc)))))
      doc)))

;;
;; When the to-hash-able finalizer is used, embedded docs/tables in the response aren't converted
;; to hash tables but to documents. When print-hash is used we want to see hash table like output
;; so that's what this tries to do..
;;
(defun print-hash (ht stream)
  (labels ((prdocs (docs) 
	     (dolist (doc docs)
	       (if (typep doc 'document)
		   (print-hash (elements doc) stream)
		   (format stream "    ~A ~%" doc))))
	   (vpr (v)
	     (cond ( (typep v 'cons)     (prdocs v)        )
		   ( (typep v 'document) (prdocs (list v)) )
		   (  t                  v))))
    (format stream "~%{~%") 
    (with-hash-table-iterator (iterator ht)
      (dotimes (repeat (hash-table-count ht))
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p (format stream "  ~A  -> ~A ~%" key (vpr value) )))))
    (format stream "}~%")))
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


#|
  containers are collections of key-value pairs. These are either cons's, hash tables
  or documents..
|#

(defgeneric bson-encode-container ( container &key )
  (:documentation "encode a container of key-value pairs.."))

;;(defmethod bson-encode-container ( (container array) )
;;  container)

(defmethod bson-encode-container ( (container document)
  				  &key (array nil) (size 10) )
  (setf (gethash "_id" (elements container)) (_id container))
  (bson-encode-container (elements container) :array array :size size))

(defmethod bson-encode-container ( (container hash-table) 
				  &key (array nil array-supplied-p) (size 10 size-supplied-p) )
  (let* ((size (if size-supplied-p size 10))
	 (array (if array-supplied-p array (make-octet-vector size))))
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

(defmethod bson-type-extract ( (code (eql +bson-data-oid+)) array)
  (values (make-bson-oid :oid (subseq array 0 12)) (subseq array 12)))

(defmethod bson-type-extract ( (code (eql +bson-data-boolean+)) array)
  (values (byte-to-bool (octet-to-byte (subseq array 0 1))) (subseq array 1)))

(defmethod bson-type-extract ( (code (eql +bson-data-int32+)) array)
  (values (octet-to-int32 (subseq array 0 4)) (subseq array 4)))

(defmethod bson-type-extract ( (code (eql +bson-data-long+)) array)
  (values (octet-to-int64 (subseq array 0 8)) (subseq array 8)))

#|
  Special operators
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

  

