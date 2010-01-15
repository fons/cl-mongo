(require 'usocket)

(defconstant +mongo-update+       2001 "update/insert command")
(defconstant +mongo-insert+       2002 "insert command")
(defconstant +mongo-query+        2004 "query command")
(defconstant +mongo-get-more+     2005 "request for more docs/next cursor iterator")
(defconstant +mongo-delete+       2006 "delete command")
(defconstant +mongo-kill-cursors+ 2007 "kill the cursor/reclaim db resources")

(defgeneric mongo-update (collection selector document &key options)
  (:documentation "insert function for mongo"))

(defun update-options ( &key (upsert 0 upsert-p) (multi-update 0 multi-update-p) )
  (let ((value 0))
    (when upsert-p       (setf (ldb (byte 1 0) value) upsert))
    (when multi-update-p (setf (ldb (byte 1 1) value) multi-update))
    value))

(defmethod mongo-update ( (collection string) (selector array) (document array) &key (options 0) )
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
    (set-octets 0 (int32-to-octet (length arr) ) arr) ; set length
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
    (set-octets 0 (int32-to-octet (length arr) ) arr)
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
    (set-octets 0 (int32-to-octet (length arr) ) arr)
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
    (set-octets 0 (int32-to-octet (length arr) ) arr)
    arr))


(defgeneric mongo-query ( collection query &key options skip limit selector)
  (:documentation "query a bson database"))

(defmethod mongo-query ( (collection string) (query t) &key (options 0) (skip 0) (limit 0) (selector nil) )
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
    (set-octets 0 (int32-to-octet (length arr) ) arr) ; set message length
    arr))

(defmethod mongo-query ( (collection string) (query hash-table) 
			&key (options 0) (skip 0) (limit 0) (selector nil) )
  (call-next-method collection (bson-encode-container query) 
		    :options options :skip skip :limit limit :selector selector))

(defgeneric mongo-get-more (collection cursor &key limit)
  (:documentation "iterate the db cursor.."))

(defmethod mongo-get-more ( (collection string) (cursor array) &key (limit 0) )
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr)                           ; length
    (add-octets (int32-to-octet 0) arr)                           ; request id
    (add-octets (int32-to-octet 0) arr)                           ; response to
    (add-octets (int32-to-octet +mongo-get-more+) arr)            ; opcode
    (add-octets (int32-to-octet 0) arr)                           ; zero
    (add-octets (string-to-null-terminated-octet collection) arr) ; collection name
    (add-octets (int32-to-octet limit) arr)                       ; limit; numberToReturn
    (add-octets cursor arr)                                       ; cursor id
    (set-octets 0 (int32-to-octet (length arr) ) arr)             ; set message length
    arr))

(defgeneric mongo-kill-cursors (cursor count)  
  (:documentation "tell the db you're done with the cursors"))

(defmethod mongo-kill-cursors ( (cursor array) (count (eql 1) ) )
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr)                    ; length
    (add-octets (int32-to-octet 0) arr)                    ; request id
    (add-octets (int32-to-octet 0) arr)                    ; response to
    (add-octets (int32-to-octet +mongo-kill-cursors+) arr)  ; opcode
    (add-octets (int32-to-octet 0) arr)                    ; zero
    (add-octets (int32-to-octet count) arr)                ; number of cursors in the message
    (add-octets cursor arr)                                ; cursor id
    (set-octets 0 (int32-to-octet (length arr) ) arr)      ; set message length
    arr))
  

(defgeneric mongo-reply (array &key finalize) 
  (:documentation "extract reply parameters"))

(defmethod mongo-reply ( (array (eql nil) ) &key finalize)
  (declare (ignore finalize))
  )

;;
;; Not everything is going to be a complete document
;;
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


(defgeneric mongo-delete (collection object) 
  (:documentation "delete a mongo object"))

(defmethod mongo-delete ( (collection string) (selector array) )
  (let ((arr (make-octet-vector 50)))
    (add-octets (int32-to-octet 0) arr)              ; length
    (add-octets (int32-to-octet 0) arr)              ; request id
    (add-octets (int32-to-octet 0) arr)              ; response to
    (add-octets (int32-to-octet +mongo-delete+) arr) ; opcode
    (add-octets (int32-to-octet 0) arr)              ; zero
    (add-octets (string-to-null-terminated-octet collection) arr) ; collection name
    (add-octets (int32-to-octet 0) arr)                 ; zero
    (add-octets selector arr)                           ; selector
    (set-octets 0 (int32-to-octet (length arr) ) arr)   ; set length
    arr))

(defmethod mongo-delete ( (collection string) (object document) )
  (mongo-delete collection (bson-encode "_id" (_id object)))) ; encode unique id 

#|
  Connection...
|#

(defconstant +MONGO-PORT+ 27017)
(defconstant +TEST-PORT+  9999)
(defvar      *mongo-registry* () "list of all open mongo connections")

(defclass mongo ()
  ((port   :reader   port             :initarg :port)
   (host   :reader   host             :initarg :host)
;   (stream :accessor stream           :initarg :stream) 
   (socket :accessor socket           :initarg :socket)
   (db     :accessor db               :initarg :db)))
;	     
(defmethod initialize-instance :after ( (mongo mongo) &key)
  (labels ((socket* (host port) 
	     (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
    (setf (socket mongo) (socket* (host mongo)  (port mongo)))))
 
(defmethod shared-initialize :after ( (mongo mongo) slots &key)
  (push mongo *mongo-registry*))


(defun make-mongo ( &key (host "localhost") (port +MONGO-PORT+) (db nil) )
   (make-instance 'mongo :host host :port port  :db db  :socket nil))

(defmethod print-object ((mongo mongo) stream)
  (format stream "~S [socket : ~A] [port : ~A] [host : ~A] [db : ~A] ~%" (type-of mongo) 
	  (if (slot-boundp mongo 'socket) 
	      (socket mongo)
	      "socket not set")
	  (if (slot-boundp mongo 'port) 
	      (port mongo)
	      "port not set")
	  (if (slot-boundp mongo 'host) 
	      (host mongo)
	      "host not set")
	  (if (slot-boundp mongo 'db) 
	      (db mongo)
	      "db not set")))

(defgeneric stream (mongo)
  (:documentation "mongo stream socket.."))

(defmethod stream ( (mongo mongo) )
  (usocket:socket-stream (socket mongo)))

(defgeneric mongo (&key)
  (:documentation "mongo connection"))

;;
;; If the registry is empty open a connection so that (mongo :host .. :port) or (mongo)
;; gets a live connection.
;; If the caller supplies an index, use it (unless the index is 0 and the registry is empty)
;; This may return nil..
;; Open a connection a subsequent connection only if host and port are provided..

(defmethod mongo ( &key (index 0 index-p) (host "localhost" host-p) (port +MONGO-PORT+ port-p) )
  (cond  ( (and (eql 0 index) (null *mongo-registry*) ) (make-mongo :host host :port port) )
	 ( index-p                                      (nth index *mongo-registry*))
	 ( (and host-p port-p)                          (make-mongo :host host :port port) )
	 ( t                                            (car *mongo-registry*) )))


(defun close-all-connections ()
  (dolist (mongo *mongo-registry*)
    (pop *mongo-registry*)
    (close (stream mongo))))

(defgeneric mongo-message (mongo message &key ) 
  (:documentation "message to/from mongo.."))

(defmethod mongo-message ( (mongo mongo) (message array) &key (timeout 5) )
  (write-sequence message (stream mongo))
  (force-output (stream mongo))
  (usocket:wait-for-input (list (socket mongo) ) :timeout timeout)
  (if (listen (stream mongo))
      (progn 
	(let* ((reply  (make-octet-vector 1000 :init-fill 4 )) 
	       (cursor (read-sequence reply (stream mongo) :start 0 :end 4))
	       (rsz    (octet-to-int32 (subseq reply 0 4))))
	  (unless (array-in-bounds-p reply rsz) (adjust-array reply rsz))
	  (setf (fill-pointer reply) rsz) 
	  (read-sequence reply (stream mongo) :start cursor)
	  reply))
      nil))

(defgeneric full-collection-name (mongo collection)
  (:documentation "generate the full collection name"))

(defmethod full-collection-name ( (mongo mongo) (collection string) )
  (concatenate 'string (db mongo) "." collection))

#|
  db shell functions
|#
;; special commands + : up -:down
(defvar *db.use-history* () "an attempt to record the history of the shell" )

(defgeneric db.use ( db &optional mongo )
  (:documentation "use a specific db"))

(defmethod db.use ( (db string) &optional (mongo nil) )
  (push db *db.use-history*)
  (setf (db (or mongo (mongo))) db))

(defmethod db.use ( (db cons)  &optional (mongo nil) )
  (when (eql db -)
    (progn
      (push (cadr *db.use-history*) *db.use-history*)
      (setf (db (or mongo (mongo))) (car *db.use-history*)))))


;;
;; This is intended to be a helper function for key-value pairs..
;; Here's a caveat : (kv "nested" (kv "hello" "again" (kv "678" 678)))
;; will generate ("nested" ("hello" "again")) because kv 
;; (kv "hello" "again" (kv "678" 678))) the third argument is ignored.
;;
;; Basically don't use kv to construct nested conses; use list instead.
;;
;; pair  -> (string value)
;; value -> atom | cons 
;; kv    -> pair | list
;; list  -> (elem list) | nil
;;
(defstruct (pair
	     (:conc-name pair-)
	     (:constructor pair (key value)))
  key value)

(defgeneric kv (a b &rest rest) )

(defmethod kv ( (a (eql nil) ) (b (eql nil)) &rest rest)
  (declare (ignore rest))
  (pair nil nil))

;; basically b can be anything with
;; an encoder..

(defmethod kv ( (a string) b &rest rest)
  (declare (ignore rest))
  (pair a b))

(defmethod kv ( (a symbol) b &rest rest)
  (declare (ignore rest))
  (pair (string-downcase a) b))

;
;; TODO -> don't return a cons as that's basically an array,
;; a compound query is a document/hash table.
;; when returning a cons --> need to call kv->ht on the cons !!  
;; so you might as well incorporate that stright in here
(defmethod kv ( (a pair) (b pair) &rest rest)
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash (pair-key a) ht) (pair-value a))
    (setf (gethash (pair-key b) ht) (pair-value b))
    (dolist (el rest)
      (setf (gethash (pair-key el) ht) (pair-value el)))
    ht))

(defmethod kv-alt ( (a pair) (b pair) &rest rest)
  (let ((lst (list b a)))
    (dolist (el rest)
      (push el lst))
    (nreverse lst)))

    
(defun bson-encode-pair ( kv )
  (bson-encode (pair-key kv) (pair-value kv)))

(defmethod bson-encode ( (key string) (value pair) &key (array nil array-supplied-p) 
			(size 10 size-supplied-p) 
			(type nil) (encoder nil))
  (declare (ignore encoder) (ignore array) (ignore type) (ignore size) 
	   (ignore size-supplied-p) (ignore array-supplied-p) )
  (bson-encode key (bson-encode-pair value)))


(defgeneric kv->doc ( kv )
  (:documentation "turn a pair of key/value pairs into a document"))

(defmethod kv->doc ( (kv pair) ) 
  (let ((doc (make-document)))
    (add-element (pair-key kv) (pair-value kv) doc)))

(defmethod kv->doc ( (kv cons) ) 
  (let ((doc (make-document)))
    (dolist (pair kv)
      (add-element (pair-key pair) (pair-value pair) doc))
    doc))


(defmethod kv->ht ( (kv cons) )
  (elements (kv->doc kv)))

(defgeneric db.insert ( collection document &key )
  (:documentation "inserting into a mongo database"))

(defmethod db.insert ( (collection string) (document t) &key (mongo nil) )
  (let ((mongo (or mongo (mongo))))
    (mongo-message mongo (mongo-insert (full-collection-name mongo collection) document) :timeout 0)))

(defmethod db.insert ( (collection string) (document hash-table) &key (mongo nil) )
  (call-next-method collection (bson-encode-container document) :mongo mongo))

(defmethod db.insert ( (collection string) (document document) &key (mongo nil) )
  (call-next-method collection (bson-encode-container document) :mongo mongo))

(defmethod db.insert ( (collection string) (kv pair) &key (mongo nil) )
  (call-next-method collection (bson-encode-container (kv->doc kv)  ) :mongo mongo))

(defmethod db.insert ( (collection string) (kv hash-table) &key (mongo nil) )
  (call-next-method collection (bson-encode-container kv) :mongo mongo))

(defgeneric db.find (collection  kv &key)
  (:documentation "find a document in the db collection"))

(defmethod db.find ( (collection symbol) (kv t) 
		    &key (mongo nil) (options 0) (skip 0) (limit 1) (selector nil) )
  (db.find (string-downcase collection) kv 
		    :mongo mongo :options options :skip skip :limit limit :selector selector ))

(defmethod db.find ( (collection string) (kv t) 
		    &key (mongo nil) (options 0) (skip 0) (limit 1) (selector nil) )
  (let ((mongo (or mongo (mongo))))
    (labels ((query ()
	       (mongo-message mongo (mongo-query 
				     (full-collection-name mongo collection) kv
				     :limit limit :skip skip :selector selector :options options))))
      (multiple-value-bind (header docs) (mongo-reply (query) :finalize 'to-document)
	(list (append header (list collection)) docs)))))

(defmethod db.find ( (collection string) (kv (eql 'all)) 
		    &key (mongo nil) (options 0) (skip 0) (selector nil) )
  (call-next-method collection (bson-encode nil nil)
		    :mongo mongo :options options :skip skip :limit 0 :selector selector ))
  
(defmethod db.find ( (collection string) (kv integer) 
		    &key (mongo nil) (options 0) (skip 0) (selector nil) )
  (call-next-method collection (bson-encode nil nil)
		    :mongo mongo :options options :skip skip :limit kv :selector selector ))

(defmethod db.find ( (collection string) (kv pair) 
		    &key (mongo nil) (options 0) (skip 0) (limit 1) (selector nil) )
  (call-next-method collection (bson-encode (pair-key kv) (pair-value kv))
		    :mongo mongo :options options :skip skip :limit limit :selector selector ))

(defmethod db.find ( (collection string) (kv hash-table) 
		    &key (mongo nil) (options 0) (skip 0) (limit 1) (selector nil) )
  (call-next-method collection (bson-encode-container kv)
		    :mongo mongo :options options :skip skip :limit limit :selector selector ))

(defun headerp (val)
  (and (consp val) (= 1 (length val))))

(defun header+docsp (val)
  (and (consp val)  (= 2 (length val)) (consp (car val)) (consp (cadr val))))

(defun db.iterator ( result )
    (cond ( (headerp result)      (values (nth 5 result)       (car (last result))       nil)          )
	  ( (header+docsp result) (values (nth 5 (car result)) (car (last (car result))) (cadr result)) )
	  ( t                     (values 0 nil nil)))) ;stop iteration

(defgeneric db.next ( collection cursor-id &key ) 
  (:documentation "next iteration of the mongo db cursor"))

(defmethod db.next ( (collection (eql nil)) (cursor-id (eql nil)) &key mongo limit)
  (declare (ignore mongo) (ignore limit))
  (list nil nil))

(defmethod db.next ( (collection (eql nil)) (cursor-id (eql 0)) &key mongo limit)
  (declare (ignore mongo) (ignore limit))
  (list nil nil))

(defmethod db.next ( (collection string) (cursor-id integer) &key (mongo nil) (limit 0) )
  (let ((mongo (or mongo (mongo))))
    (labels ((get-more ()
	       (mongo-message mongo (mongo-get-more 
				     (full-collection-name mongo collection) 
				     (int64-to-octet cursor-id) :limit limit))))
      (multiple-value-bind (header docs) (mongo-reply (get-more) :finalize 'to-document)
	(list (append header (list collection)) docs)))))

(defgeneric db.iter ( result &key )
  (:documentation "next document iteration"))

(defmethod db.iter ( (result (eql nil) ) &key ) )

(defmethod db.iter ( (result cons) &key (mongo nil) (limit 0) )
    (let ((mongo (or mongo (mongo))))
      (multiple-value-bind (iterator collection docs) (db.iterator result) 
	(multiple-value-bind (header docs*) (values-list (db.next collection iterator :mongo mongo :limit limit))
	  (list header (append docs docs*))))))

(defgeneric db.stop ( cursor &key mongo )
  (:documentation "stop and clean up the cursor"))

(defmethod db.stop ( (cursor (eql nil) ) &key mongo) 
  (declare (ignore mongo))
  )

(defmethod db.stop ( (cursor (eql 0) ) &key mongo) 
  (declare (ignore mongo))
  )

(defmethod db.stop ( (cursor integer) &key (mongo nil) )
    (mongo-message (or mongo (mongo)) (mongo-kill-cursors (int64-to-octet cursor) 1) :timeout 0))

(defmethod db.stop ( (result cons) &key (mongo nil) )
  (labels ((decapitate (result)
	     (cond ( (headerp result)      result)
		   ( (header+docsp result) (car result) )
		   ( t                     nil))))
    (let* ((header    (decapitate result))
	   (cursor-id (nth 5 header))
	   (docs   (cadr result)))
      (db.stop cursor-id :mongo mongo)
      (list header docs))))

(defgeneric db.delete ( collection object &key )
  (:documentation "delete one or more objects from the db"))

(defmethod db.delete ( (collection (eql nil)) (document (eql nil)) &key))

(defmethod db.delete ( (collection string) (document document) &key (mongo nil))
  (let ((mongo (or mongo (mongo) )))
    (mongo-message mongo (mongo-delete 
			  (full-collection-name mongo collection)  
			  document ) :timeout 0)))

(defmethod db.delete ( (collection string) (documents cons ) &key (mongo nil))
  (dolist (doc documents)
    (db.delete collection doc :mongo mongo)))


;
; key -> (string asc)
; asc -> t | nil
; keys -> (list key)
;
(defmethod db.ensure-index ( (collection string) (keys cons) &key (mongo nil)  (unique nil) )
  (assert (typep unique 'boolean))
  (let ((mongo (or mongo (mongo))))
    (labels ((keys->ht (keys) 
	       (if (null (cdr keys) ) ;list of length 1
		   (kv->ht (car keys))
		   (kv->ht keys)))
	     (keys->name (keys)
	       (format nil "~{~{~a~^_~}~^_~}" keys)))
    (db.insert "system.indexes" 
	       (kv->ht (kv (kv "ns"   (full-collection-name mongo collection) ) 
			   (kv "key"  (keys->ht keys) )
			   (when unique (kv "unique" unique))
			   (kv "name" (keys->name keys) ) ))))))

(defmethod db.ensure-index ( (collection string) (key string) &key (mongo nil)  (unique nil) (asc t) )
  (let ((mongo    (or mongo (mongo)))
	(order-id (if asc 1.0 -1.0)))
    (db.ensure-index collection (list (list key order-id) ) :mongo mongo :unique unique)))



(defgeneric db.run-command ( cmd &key )
  (:documentation "run a command on the db.."))

;;
;; This converts a symbol to a down/lower case string
;; This will work for most commands (which are lower case) 
;; but a few are camel-case so I have overides for those..
;;

(defmethod db.run-command ( (cmd symbol) &key (mongo nil) (arg 1) )
  (db.run-command (string-downcase cmd) :mongo mongo :arg arg))

(defmethod db.run-command ( (cmd (eql 'listdatabases) ) &key (mongo nil) )
  (db.run-command "listDatabases" :mongo mongo))

(defmethod db.run-command ( (cmd (eql 'serverstatus) ) &key (mongo nil) )
  (db.run-command "serverStatus" :mongo mongo))

(defmethod db.run-command ( (cmd string) &key (mongo nil) (arg 1) )
  (db.find "$cmd" (kv cmd arg) :limit 1 :mongo mongo))

#|
   

;; special commands

|#

(defmethod db.indexes (&key (mongo nil) )
  (db.find "system.indexes" 'all :mongo mongo))

(defmethod db.collections (&key (mongo nil) )
  (db.find "system.namespaces" 1000 :mongo mongo))

(defmethod db.run-command ( (cmd (eql 'deleteindexes) ) &key (mongo nil) (collection nil) (index "*") )
  (assert (not (null collection)))
  (db.find "$cmd" (kv->ht (kv (kv "deleteIndexes" collection) (kv "index" index))) :mongo mongo))


(defmethod db.count ( (collection t) (selector t) &key (mongo nil) )
  (db.find "$cmd" (kv (kv "count" collection) (kv "query" selector) (kv "fields" nil)) :mongo mongo :limit 1))

(defmethod db.count ( (collection t) (selector (eql 'all) ) &key (mongo nil) )
  (call-next-method collection nil :mongo mongo))

(defmethod db.count ( (collection t) (selector pair ) &key (mongo nil) )
  (call-next-method collection (kv->ht selector :mongo mongo))


#|
 shell commands
|# 

(defun cwd ( &key (mongo nil) )
  (db (or mongo (mongo))))

(defun nwd ()
  (cadr *db.use-history*))

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

;(defgeneric index ( collection name action &key )
;  (:documentation "shell index managment"))

;(defmethod (index (collection string) (name string) (action (eql 'create)) &key (unique nil) (asc t) (mongo nil) )
    
#|

|#
(defun write-array(port array)
  (let (( socket (usocket:socket-connect "localhost" port :element-type '(unsigned-byte 8))))
    (write-sequence array (usocket:socket-stream socket))
    (usocket:socket-close socket)))

(defun ping-connect(port)
  (let (( socket (usocket:socket-connect "localhost" port)))
    (format (usocket:socket-stream socket) "ping") 
    (usocket:socket-close socket)))

(defun read-array (socket timeout)
  (usocket:wait-for-input (list socket) :timeout timeout)
  (if (listen (usocket:socket-stream socket))
      (progn 
	(let* ((reply  (make-octet-vector 1000 :init-fill 4 )) 
	       (cursor (read-sequence reply (usocket:socket-stream socket) :start 0 :end 4))
	     (rsz    (octet-to-int32 (subseq reply 0 4))))
	  (unless (array-in-bounds-p reply rsz) (adjust-array reply rsz))
	  (setf (fill-pointer reply) rsz) 
	  (read-sequence reply (usocket:socket-stream socket) :start cursor)
	  reply))
      nil))
  
(defun write-read-array(port array &key (timeout 5) )
  (let (( socket (usocket:socket-connect "localhost" port :element-type '(unsigned-byte 8))))
    (write-sequence array (usocket:socket-stream socket))
    (force-output (usocket:socket-stream socket))
    (let (( reply (read-array socket timeout)))
      (usocket:socket-close socket)
      reply)))

#|
  tests go here.
|#

(defun test-insert(key value &key (port +TEST-PORT+) )
  (write-array port (mongo-insert "test.foo" (bson-encode key value))))

(defun test-insert-doc(document &key (port +TEST-PORT+) )
  (write-array port (mongo-insert "test.foo" document)))

(defun test-update (id key value &key (options 0) (port +TEST-PORT+) )
  (write-read-array port 
		    (mongo-update "test.foo" 
				  (bson-encode "_id" id) 
				  (bson-encode key value)  
				  :options options) :timeout 0))

(defun test-update-kv (k v  key value &key (options 0) (port +TEST-PORT+) )
  (write-read-array port (mongo-update 
			  "test.foo" 
			  (bson-encode k v) 
			  (bson-encode key value) :options options) :timeout 0))

(defun test-query (&key (port +TEST-PORT+) )
  (write-read-array port (mongo-query "test.foo" (bson-encode nil nil))))

(defun test-query-kv(key value &key (port +TEST-PORT+) )
  (write-read-array port (mongo-query "test.foo" (bson-encode key value))))

(defun test-query-ht(ht &key (port +TEST-PORT+) )
  (write-read-array port (mongo-query "test.system.indexes" (bson-encode-container ht))))

(defun test-query-kv-selector(key value &key (port +TEST-PORT+) )
  (write-read-array port (mongo-query "test.foo" (bson-encode key value) :selector (bson-encode key 1) )))

(defun test-insert-alt(key value &key (port +TEST-PORT+) )
  (write-read-array port (mongo-insert "test.foo" (bson-encode key value)) :timeout 0))


(defun test-delete( selector &key (port +TEST-PORT+) )
  (write-read-array port (mongo-delete "test.foo" selector) :timeout 0))

;
; Note : limit == 1 ---> no cursor is returned !
;
(defun get-all-docs ()
  (let ((docs ())
	(cursor-id 0))
    (labels ((query ()
	       (write-read-array +MONGO-PORT+ (mongo-query "test.foo" (bson-encode nil nil))))
	     (get-more (cursor-id)
	       (write-read-array +MONGO-PORT+ (mongo-get-more "test.foo" (int64-to-octet cursor-id)))))
      (multiple-value-bind (header docs+) (mongo-reply (query) :finalize 'to-document)
	(progn 
	  (setf docs (append docs docs+))
	  (setf cursor-id (nth 5 header) )
	  ;(format t "~% ~A cursor id : ~A~%" header cursor-id)
	  (do () ( (zerop cursor-id) )
	    (multiple-value-bind (header docs+) (mongo-reply (get-more cursor-id) :finalize 'to-document)
	      (progn 
		(setf docs (append docs docs+))
		(setf cursor-id (nth 5 header)))))))
      docs)))

(defun test-kill-cursor ()
  (let ((docs ())
	(cursor-id 0))
    (labels ((query ()
	       (write-read-array +MONGO-PORT+ (mongo-query "test.foo" (bson-encode nil nil))))
	     (kill-cursor (cursor-id)
	       (write-read-array +MONGO-PORT+ (mongo-kill-cursors (int64-to-octet cursor-id) 1) :timeout 0))
	     (get-more (cursor-id)
	       (write-read-array +MONGO-PORT+ (mongo-get-more "test.foo" (int64-to-octet cursor-id) :limit 10))))
      (multiple-value-bind (header docs+) (mongo-reply (query) :finalize 'to-document)
	(progn 
	  (setf docs (append docs+ docs))
	  (setf cursor-id (nth 5 header) )
	  (format t "~% ~A cursor id : ~A~%" header cursor-id)
	  (do () ( (zerop cursor-id) )
	    (multiple-value-bind (header docs+) (mongo-reply (get-more cursor-id) :finalize 'to-document)
	      (progn 
		(setf docs (append docs+ docs))
		(setf cursor-id (nth 5 header) )
		(format t "~% ~A cursor id : ~A~%" header cursor-id)
		(format t "--> ~A ~%" (length docs))
		(when (> (length docs) 500)
		  (progn
		    (format t "kill off the cursor ~A (~A) ~%" cursor-id (length docs))
		    (kill-cursor cursor-id)))))))))))
	    
	    
(defun query-to-cons ()
  (multiple-value-bind (header docs) (mongo-reply (test-query :port +MONGO-PORT+) :finalize 'to-document)
    (declare (ignore header))
    docs))

(defun query-1 ()
  (multiple-value-bind (header docs) (mongo-reply (test-query :port +MONGO-PORT+) :finalize 'to-document)
    (list header (length docs))))

(defun drop-all-docs()
  (let ((docs (get-all-docs)))
    (dolist (doc docs)
      (write-read-array +MONGO-PORT+ (mongo-delete "test.foo" doc) :timeout 0))))

(defun total-length (docs)
  (let ((val 0))
    (dolist (element docs)
      (setf val (+ val (octet-to-int32 (subseq element 0 4)))))
    val))

(defun mongo-reply-check ( lst )
  (let* ((header   (car lst) )
	 (elements (cadr lst))
	 (tot-size (first header))
	 (offset   (* 4 9))
	 (docs     (nth 7 header)))
    (format t "header ~A ~%" header)
    (format t "# docs   ~A -- elements ~A ~%" docs (length elements))
    (format t "# length ~A -- doc length ~A ~%" (- tot-size offset) (total-length elements) )
    (assert (eql docs (length elements))) 
    (assert (eql (total-length elements) (- tot-size offset)))))

(defun send-test-document()
  (let ((doc (make-document)))
    (add-element "string" "hello world generated by test-document" doc)
    (add-element "number89" 89 doc)
    (add-element "numberbig" 89988787777887 doc)
    (add-element "float 89.67" 89.67 doc)
    (add-element "list-1" (list 1 2 3 4) doc)
    (add-element "list-2" (list 1 (list "inside" 3 4 "list" ) "the end") doc)
    (test-insert-doc doc :port +MONGO-PORT+)))

(defun send-doc-in-doc()
  (let ((doc (make-document)))
    (add-element "string" "hello world generated by test-document" doc)
    (add-element "number89" 89 doc)
    (add-element "numberbig" 89988787777887 doc)
    (add-element "float 89.67" 89.67 doc)
    (add-element "list-1" (list 1 2 3 4) doc)
    (add-element "list-2" (list 1 (list "inside" 3 4 "list" ) "the end") doc)
    (let ((doc2 (make-document)))
      (add-element "key-1" "doc in doc !!" doc2)
      (add-element "key-2" 56.89 doc2)
      (add-element "doc2" doc2 doc)
      (test-insert-doc doc :port +MONGO-PORT+))))

(defun send-doc-in-doc-in-doc()
  (let ((doc (make-document)))
    (add-element "string" "hello world generated by test-document" doc)
    (add-element "number89" 89 doc)
    (add-element "numberbig" 89988787777887 doc)
    (add-element "float 89.67" 89.67 doc)
    (add-element "list-1" (list 1 2 3 4) doc)
    (add-element "list-2" (list 1 (list "inside" 3 4 "list" ) "the end") doc)
    (let ((doc2 (make-document)))
      (add-element "key-1" "doc in doc !!" doc2)
      (add-element "key-2" 56.89 doc2)
      (let ((doc3 (make-document)))
	(add-element "array" (list (list (list "inside a nest!!"))) doc3)
	(add-element "key56" 56 doc3)
	(add-element "doc3" doc3 doc2)
	(add-element "doc2" doc2 doc)
	(test-insert-doc doc :port +MONGO-PORT+)))))


(defun test-doc-simple()
    (let ((doc (make-document)))
      (add-element "string" "hello world generated by test-document" doc)
      (add-element "1" 1 doc)
      (add-element "2" 2 doc)
      (add-element "3" 3 doc)
      (add-element "4" 40 doc)
      (add-element "5" 30 doc)
      (test-insert-doc doc :port +MONGO-PORT+)))

(defun insert-lots (n)
  (dotimes (i n)
    (let ((doc (make-document)))
      (add-element (format nil "~D" i) i doc)
      (add-element (format nil "k") (+ 56.00 i) doc)
      (add-element (format nil "l") (- i 9.00)  doc)
      (add-element (format nil "index-this") i doc)
      (add-element (format nil "value-1") (* 78 i) doc)
      (test-insert-doc doc :port +MONGO-PORT+))))
  
