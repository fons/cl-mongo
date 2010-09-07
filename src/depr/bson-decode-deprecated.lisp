
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
(let* ((size (octet-to-int32 (subseq array 0 4)))

	 (str  (null-terminated-octet-to-string (subseq array 4 (+ 4 size)) size))
	 (rest (subseq array (+ 4 size))))
    (values str rest)))

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
	;(format t "start parser ~%")
	(map nil #'parse-stream  array)
	;(format t "parser done ; codecs started ~%")
	(prog1 
	    (mapcar #'codecs (nreverse lst))
	  ;(format t "codecs done..~%")
	  )))))
	  
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

;;----------------------------------------------------version 2 

(defgeneric bson-decode.2 (code array startpos) 
  (:documentation "return data and the remaining array"))


(defun perf.to-cstring.2 (array startpos)
  (let* ((size (octet-to-int32.1 array startpos ))
	 (npos (+ 4 startpos))
	 (eos  (+ 3 startpos size)) ;;do not include null
	 (str  (babel:octets-to-string array :start npos :end eos )))
    ;; add 1 to skip over null
    (values str (+ 1 eos) )))

(defun perf.to-long.2 (array startpos)
  (values (octet-to-int64.1 array startpos) (+ 8 startpos)))
    

(defmethod bson-decode.2 ( (code (eql +bson-data-string+)) array startpos)
  (to-cstring.2 array startpos))

(defmethod bson-decode.2 ( (code (eql +bson-data-long+)) array startpos)
  (let ((npos (+ 8 startpos)))
    (values (octet-to-int64.1 array startpos) npos)))

;;----------------------------
(defmethod bson-decode ( (code t) (array t) )
;  (format t "~% code : ~A ~%" code)
;  (format t "~% array : ~A ~% " array)
  (values array (make-octet-vector 1 :init-fill 1)))

(defmethod bson-decode ( (code (eql +bson-data-number+)) array)
  (values (decode-double-float-bits (octet-to-int64 (subseq array 0 8))) (subseq array 8)))


;;
;; Not everything is going to be a complete document
;;

#|

(defmethod mongo-reply ( (array array) &key (finalize (lambda (x) x) ) )
  (labels ((header (array)
	     (let ((lst ()))
	       (push (octet-to-int32 (subseq array 0 4))   lst)   ; length
	       (push (octet-to-int32 (subseq array 4 8))   lst)   ; request id
	       (push (octet-to-int32 (subseq array 8 12))  lst)   ; response to
	       (push (octet-to-int32 (subseq array 12 16)) lst)   ; opcode
	       (push (octet-to-int32 (subseq array 16 20)) lst)   ; response flag
	       (push (octet-to-int64 (subseq array 20 28)) lst)   ; cursor id
	       (push (octet-to-int32 (subseq array 28 32)) lst)   ; starting from
	       (push (octet-to-int32 (subseq array 32 36)) lst)   ; returned
	       (nreverse lst)))
	   (to-obj+ ( array  accum)
	     (let* ((len   (octet-to-int32 (subseq array 0 4)))
		    (head  (subseq array 0 len) )
		    (rest  (subseq array len) ))
	       (if (zerop (length rest) )
		   (nreverse (cons head  accum))
		   (to-obj+ rest (cons head accum) ))))
	   (to-obj (array)
	     (if (zerop (length array))
		 nil
		 (to-obj+ array ()))))
    (values (header array) (mapcar finalize (to-obj (subseq array 36))))))
|#

(defun chunk+(pos array)
  (let* ((len   (octet-to-int32 (subseq array pos (+ 4 pos)))))
    (values (+ pos len) (subseq array pos (+ pos len)))))

(defun to-obj+ (array  accum)
  (let ((array-length (length array))
	(pos 0))
    (loop
       (multiple-value-bind (newpos doc) (chunk+ pos array)
	 (setf pos newpos)
	 (push doc accum))
       (when (= pos array-length) (return  (nreverse accum))))))


(defmethod mongo-reply ( (array array) &key (finalize (lambda (x) x) ) )
  (labels ((header (array)
	     (let ((lst ()))
	       (push (octet-to-int32 (subseq array 0 4))   lst)   ; length
	       (push (octet-to-int32 (subseq array 4 8))   lst)   ; request id
	       (push (octet-to-int32 (subseq array 8 12))  lst)   ; response to
	       (push (octet-to-int32 (subseq array 12 16)) lst)   ; opcode
	       (push (octet-to-int32 (subseq array 16 20)) lst)   ; response flag
	       (push (octet-to-int64 (subseq array 20 28)) lst)   ; cursor id
	       (push (octet-to-int32 (subseq array 28 32)) lst)   ; starting from
	       (push (octet-to-int32 (subseq array 32 36)) lst)   ; returned
	       (nreverse lst)))
	   (to-obj (array)
	     (if (zerop (length array))
		 nil
		 (to-obj+ array ()))))
    (values (header array) (mapcar finalize (to-obj (subseq array 36))))))

