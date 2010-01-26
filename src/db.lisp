(in-package :cl-mongo)

(defgeneric full-collection-name (mongo collection)
  (:documentation "generate the full collection name"))

(defmethod full-collection-name ( (mongo mongo) (collection string) )
  (concatenate 'string (db mongo) "." collection))

(defgeneric db.insert ( collection document &key )
  (:documentation "inserting into a mongo database"))

(defmethod db.insert ( (collection string) (document t) &key (mongo nil) )
  (let ((mongo (or mongo (mongo))))
    (mongo-message mongo (mongo-insert (full-collection-name mongo collection) 
				       (bson-encode-container document)) :timeout 0)))


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

(defgeneric db.update ( collection selector new-document &key )
  (:documentation "find a document and replace it with a new version"))

(defmethod db.update ( (collection string) (selector t) (new-document t) 
		      &key (mongo nil) (upsert nil) (multi nil) )
  (let ((mongo (or mongo (mongo))))
    (mongo-message mongo (mongo-update 
			  (full-collection-name mongo collection) 
			  (bson-encode-container selector) 
			  (bson-encode-container new-document) 
			  :options (update-options :upsert upsert :multi-update multi))
		   :timeout 0)))


(defgeneric db.save ( collection document &key) 
  (:documentation "save a document; to insert if no _id is found"))

(defmethod db.save ( (collection string) (document document) &key (mongo nil) )
  (db.update collection (kv "_id" (_id document) ) document :mongo (or mongo (mongo) ) :upsert t))

(defmethod db.save ( (collection string) (document hash-table) &key (mongo nil) )
  (db.insert collection document :mongo (or mongo (mongo)) ))

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
  (call-next-method collection (kv->ht selector) :mongo mongo))

