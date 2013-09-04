(in-package :cl-mongo)

(defconstant +mongo-update+       2001 "update/insert command")
(defconstant +mongo-insert+       2002 "insert command")
(defconstant +mongo-query+        2004 "query command")
(defconstant +mongo-get-more+     2005 "request for more docs/next cursor iterator")
(defconstant +mongo-delete+       2006 "delete command")
(defconstant +mongo-kill-cursors+ 2007 "kill the cursor/reclaim db resources")

(defgeneric mongo-update (collection selector document &key options)
  (:documentation "insert function for mongo"))

(defun update-options (&key (upsert nil) (multi-update nil))
  (let ((value 0))
    (when upsert
      (setf (ldb (byte 1 0) value) 1))
    (when multi-update
      (setf (ldb (byte 1 1) value) 1))
    value))

(defmethod mongo-update ((collection string) (selector array) (document array) &key (options 0))
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr)               ; length
    (add-octets (int32-to-octet 0) arr)               ; request id
    (add-octets (int32-to-octet 0) arr)               ; response to
    (add-octets (int32-to-octet +mongo-update+) arr)  ; op code 
    (add-octets (int32-to-octet 0) arr)               ; zero
    (add-octets (string-to-null-terminated-octet collection) arr) ; full collection name
    (add-octets (int32-to-octet options) arr)          ; flags
    (add-octets selector arr) ; selector
    (add-octets document arr) ; document
    (set-octets 0 (int32-to-octet (length arr)) arr) ; set length
    arr))
  
(defgeneric mongo-insert (collection documents)
  (:documentation "insert function for mongo"))

(defmethod mongo-insert ((collection string) (document array))
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr) 
    (add-octets (int32-to-octet 0) arr)
    (add-octets (int32-to-octet 0) arr)
    (add-octets (int32-to-octet +mongo-insert+) arr)
    (add-octets (int32-to-octet 0) arr)
    (add-octets (string-to-null-terminated-octet collection) arr)
    (add-octets document arr)
    (set-octets 0 (int32-to-octet (length arr)) arr)
    arr))

(defmethod mongo-insert((collection string) (document document))
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr)
    (add-octets (int32-to-octet 0) arr)
    (add-octets (int32-to-octet 0) arr)
    (add-octets (int32-to-octet +mongo-insert+) arr)
    (add-octets (int32-to-octet 0) arr)
    (add-octets (string-to-null-terminated-octet collection) arr)
    (add-octets (bson-encode-container document) arr)
    (set-octets 0 (int32-to-octet (length arr)) arr)
    arr))

(defmethod mongo-insert((collection string) (documents cons))
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr)
    (add-octets (int32-to-octet 0) arr)
    (add-octets (int32-to-octet 0) arr)
    (add-octets (int32-to-octet +mongo-insert+) arr)
    (add-octets (int32-to-octet 0) arr)
    (add-octets (string-to-null-terminated-octet collection) arr)
    (dolist (document documents)
      (add-octets (bson-encode-container document) arr))
    (set-octets 0 (int32-to-octet (length arr)) arr)
    arr))


(defgeneric mongo-query (collection query &key options skip limit selector)
  (:documentation "query a bson database"))

(defmethod mongo-query ((collection string) (query t) &key (options 0) (skip 0) (limit 0) (selector nil))
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr)             ; length
    (add-octets (int32-to-octet 0) arr)             ; request id
    (add-octets (int32-to-octet 0) arr)             ; response to
    (add-octets (int32-to-octet +mongo-query+) arr) ; opcode
    (add-octets (int32-to-octet options) arr)       ; options
    (add-octets (string-to-null-terminated-octet collection) arr) ; collection name
    (add-octets (int32-to-octet skip) arr)          ; skip; numberToSkip
    (add-octets (int32-to-octet limit) arr)         ; limit; numberToReturn
    (add-octets query arr)                          ; query
    (when selector (add-octets selector arr))        ; field selector
    (set-octets 0 (int32-to-octet (length arr)) arr) ; set message length
    arr))

(defmethod mongo-query ((collection string) (query hash-table) 
			&key (options 0) (skip 0) (limit 0) (selector nil))
  (call-next-method collection (bson-encode-container query) 
		    :options options :skip skip :limit limit :selector selector))

(defgeneric mongo-get-more (collection cursor &key limit)
  (:documentation "iterate the db cursor.."))

(defmethod mongo-get-more ((collection string) (cursor array) &key (limit 0))
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr)                           ; length
    (add-octets (int32-to-octet 0) arr)                           ; request id
    (add-octets (int32-to-octet 0) arr)                           ; response to
    (add-octets (int32-to-octet +mongo-get-more+) arr)            ; opcode
    (add-octets (int32-to-octet 0) arr)                           ; zero
    (add-octets (string-to-null-terminated-octet collection) arr) ; collection name
    (add-octets (int32-to-octet limit) arr)                       ; limit; numberToReturn
    (add-octets cursor arr)                                       ; cursor id
    (set-octets 0 (int32-to-octet (length arr)) arr)             ; set message length
    arr))

(defgeneric mongo-kill-cursors (cursor count)  
  (:documentation "tell the db you're done with the cursors"))

(defmethod mongo-kill-cursors ((cursor array) (count (eql 1)))
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr)                    ; length
    (add-octets (int32-to-octet 0) arr)                    ; request id
    (add-octets (int32-to-octet 0) arr)                    ; response to
    (add-octets (int32-to-octet +mongo-kill-cursors+) arr)  ; opcode
    (add-octets (int32-to-octet 0) arr)                    ; zero
    (add-octets (int32-to-octet count) arr)                ; number of cursors in the message
    (add-octets cursor arr)                                ; cursor id
    (set-octets 0 (int32-to-octet (length arr)) arr)      ; set message length
    arr))

#+nil(defgeneric mongo-reply (array &key finalize) 
  (:documentation "extract reply parameters"))

#+nil(defmethod mongo-reply ((array (eql nil)) &key finalize)
  (declare (ignore finalize))
 )

(defun mongo-reply (array) ;;FIXME defmethod?
  "Extract reply parameters"
  (labels ((header (array)
             #+nil(loop
                for i from 0 to 32 by 4
                when (/= i 24)
                collect (octet-to-int32.1 array i))                  
	     (let ((lst ()))
	       (push (octet-to-int32.1 array 0)   lst)   ; length
	       (push (octet-to-int32.1 array 4)   lst)   ; request id
	       (push (octet-to-int32.1 array 8)   lst)   ; response to
	       (push (octet-to-int32.1 array 12)  lst)   ; opcode
	       (push (octet-to-int32.1 array 16)  lst)   ; response flag
	       (push (octet-to-int64.1 array 20)  lst)   ; cursor id
	       (push (octet-to-int32.1 array 28)  lst)   ; starting from
	       (push (octet-to-int32.1 array 32)  lst)   ; returned
	       (nreverse lst))))
    (let ((head (header array)))
      (values head (bson-decode (car head) 36 (nth 7 head) array)))))

(defgeneric mongo-delete (collection object) 
  (:documentation "delete a mongo object"))

(defmethod mongo-delete ((collection string) (selector array))
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr)              ; length
    (add-octets (int32-to-octet 0) arr)              ; request id
    (add-octets (int32-to-octet 0) arr)              ; response to
    (add-octets (int32-to-octet +mongo-delete+) arr) ; opcode
    (add-octets (int32-to-octet 0) arr)              ; zero
    (add-octets (string-to-null-terminated-octet collection) arr) ; collection name
    (add-octets (int32-to-octet 0) arr)                 ; zero
    (add-octets selector arr)                           ; selector
    (set-octets 0 (int32-to-octet (length arr)) arr)   ; set length
    arr))

(defmethod mongo-delete ((collection string) (object document))
  (mongo-delete collection (bson-encode "_id" (_id object)))) ; encode unique id 

