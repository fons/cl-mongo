(in-package :cl-mongo)

(defgeneric full-collection-name (mongo collection)
  (:documentation "generate the full collection name"))

(defmethod full-collection-name ((mongo mongo) (collection string))
  (concatenate 'string (db mongo) "." collection))

(defgeneric db.insert (collection document &key)
  (:documentation "Insert a document in a collection. A document is typically generated by `(make-document)`, 
but it can also be a hash table, a key-value pair or kv list (see the kv functions)."))

(defmethod db.insert ((collection string) (document t) &key (mongo (mongo)))
  (mongo-message mongo (mongo-insert (full-collection-name mongo collection)
                                     (bson-encode-container document)) :timeout 0))

(defgeneric db.find (collection kv &key)
  (:documentation "
Find documents in the collection using the selector specified by kv.  
Methods take two keywords. ':limit' sets the maximum number of documents returned. The default is 1.
':skip' sets the number of documents to skip in this query. It's default is 0.
Since the default value of the limit is one, db.find by default is the equivalant of *findOne* in the
mongo documentation.
"))

(defmethod db.find ((collection string) (kv t)
                    &key (mongo (mongo)) (options 0) (skip 0) (limit 1) (selector nil))
  (labels ((query ()
             (mongo-message mongo (mongo-query 
                                   (full-collection-name mongo collection) kv
                                   :limit limit 
                                   :skip skip 
                                   :selector (bson-encode-container (expand-selector selector))
                                   :options options))))
    (multiple-value-bind (header docs) (mongo-reply (query))
      (list (append header (list collection)) docs))))

(defmethod db.find ((collection symbol) (kv t)
		    &key (mongo (mongo)) (options 0) (skip 0) (limit 1) (selector nil))
  (db.find (string-downcase collection) kv 
		    :mongo mongo :options options :skip skip :limit limit :selector selector))


(defmethod db.find ((collection string) (kv (eql :all))
		    &key (mongo (mongo)) (options 0) (skip 0) (limit 0) (selector nil))
  (db.find collection (bson-encode "query" (kv nil nil))
	   :mongo mongo :options options :skip skip :limit limit :selector selector))
  
(defmethod db.find ((collection string) (kv integer)
		    &key (mongo (mongo)) (options 0) (skip 0) (selector nil))
  (db.find collection (bson-encode nil nil)
	   :mongo mongo :options options :skip skip :limit kv :selector selector))

(defmethod db.find ((collection string) (kv pair)
		    &key (mongo (mongo)) (options 0) (skip 0) (limit 1) (selector nil))
  (db.find collection (bson-encode (pair-key kv) (pair-value kv))
	   :mongo mongo :options options :skip skip :limit limit :selector selector))

(defmethod db.find ((collection string) (kv hash-table)
		    &key (mongo (mongo)) (options 0) (skip 0) (limit 1) (selector nil))
  (db.find collection (bson-encode-container kv)
		    :mongo mongo :options options :skip skip :limit limit :selector selector))

(defmethod db.find ((collection string) (kv kv-container)
		    &key (mongo (mongo)) (options 0) (skip 0) (limit 1) (selector nil))
  (db.find collection (bson-encode-container kv)
		    :mongo mongo :options options :skip skip :limit limit :selector selector))

(defmacro db.sort (collection query &rest args)
  "sort macro : Takes the same arguments and keywords as db.find but converts the query 
   so it works as a sort. use the :field keyword to select the field to sort on.
   Set :asc to nil to reverse the sort order"
  (let ((kv-query  (gensym)))
    `(destructuring-bind (&key (selector nil) (mongo (mongo))
			       (limit 0) (skip 0) (options 0) (field nil) (asc t))
	 (list ,@args)
       (let ((kv-query  (or (and (eql ,query :all) (kv nil nil)) ,query)))
	 (db.find ,collection (kv (kv "query" kv-query) (kv "orderby" (kv field (if asc 1 -1))))
		  :limit limit :mongo mongo :skip skip :selector selector :options options)))))


(defgeneric db.update (collection selector new-document &key)
  (:documentation "In a collection update the document(s) identified by the selector statement.  
This method has two keywords. ':upsert' : If t insert the document if the document cannot be 
found in the collection. ':multi'  : Update all documents identified by the selector.
"))

(defmethod db.update ((collection string) (selector t) (new-document t)
		      &key (mongo (mongo)) (upsert nil) (multi nil))
  ;;(format t "[---> db.update selector ~A~% new doc ~A]~%" selector new-document)
  (mongo-message mongo (mongo-update 
                        (full-collection-name mongo collection)
                        (bson-encode-container selector)
                        (bson-encode-container new-document)
                        :options (update-options :upsert upsert :multi-update multi))
                 :timeout 0))

;(defgeneric db.find-and-modify (collection query 

(defgeneric db.save (collection document &key)
  (:documentation "
Save a document to the collection. If the document has a unique `_id` value (i.e. if it's generated
by `(make-document)`) it will be 'upserted' (that is: it will be inserted if the document
doesn't exist). If the document a hash table or a kv set, it will be inserted.  
In other words this a a helper-function build around *db.insert* and *db.update*.
"))

(defmethod db.save ((collection string) (document document) &key (mongo (mongo)))
  ;;(format t "---> in db.save ~A~%" document)
  ;;(format t "---> in db.save : id ~A~%" (_id document))
  (db.update collection (kv "_id" (_id document)) document :mongo mongo :upsert t))

(defmethod db.save ((collection string) (document hash-table) &key (mongo (mongo)))
  (let ((_id (gethash "_id" document)))
    (db.update collection (kv "_id" _id) document :mongo mongo :upsert t)))

(defmethod db.save ((collection string) (document kv-container) &key (mongo (mongo)))
  (db.save collection (kv->ht document) :mongo mongo))

(defun headerp (val)
  (and (consp val) (= 1 (length val))))

(defun header+docsp (val)
  (and (consp val) (= 2 (length val)) (consp (car val)) (consp (cadr val))))

(defun db.iterator (result)
  "Returns the iterator from the result set."
  (cond ((headerp result) (values (nth 5 result) (car (last result)) nil))
	((header+docsp result) (values (nth 5 (car result)) (car (last (car result))) (cadr result)))
	(t (values 0 nil nil)))) ;stop iteration


(defgeneric db.next (collection cursor-id &key)
  (:documentation "Executes the next call on the iterator identified by cursor-id."))

(defmethod db.next ((collection (eql nil)) (cursor-id (eql nil)) &key mongo limit)
  (declare (ignore mongo) (ignore limit))
  (list nil nil))

(defmethod db.next ((collection (eql nil)) (cursor-id (eql 0)) &key mongo limit)
  (declare (ignore mongo) (ignore limit))
  (list nil nil))

(defmethod db.next ((collection string) (cursor-id integer) &key (mongo (mongo)) (limit 0))
  (labels ((get-more ()
             (mongo-message mongo (mongo-get-more 
                                   (full-collection-name mongo collection)
                                   (int64-to-octet cursor-id) :limit limit))))
    (multiple-value-bind (header docs) (mongo-reply (get-more))
      (list (append header (list collection)) docs))))

(defgeneric db.iter (result &key)
  (:documentation "next document iteration"))

(defmethod db.iter ((result (eql nil)) &key))

(defmethod db.iter ((result cons) &key (mongo (mongo)) (limit 0))
  (multiple-value-bind (iterator collection docs) (db.iterator result)
    (multiple-value-bind (header docs*) (values-list (db.next collection iterator :mongo mongo :limit limit))
      (list header (append docs docs*)))))

(defgeneric db.stop (cursor &key mongo)
  (:documentation "
Stop iterating and clean up the iterator on the server by making a server call.
"))

(defmethod db.stop ((cursor (eql nil)) &key mongo)
  (declare (ignore mongo)))

(defmethod db.stop ((cursor (eql 0)) &key mongo)
  (declare (ignore mongo)))

(defmethod db.stop ((cursor integer) &key (mongo (mongo)))
  (mongo-message mongo (mongo-kill-cursors (int64-to-octet cursor) 1) :timeout 0))

(defmethod db.stop ((result cons) &key (mongo (mongo)))
  (labels ((decapitate (result)
	     (cond ((headerp result)      result)
		   ((header+docsp result) (car result))
		   (t                     nil))))
    (let* ((header    (decapitate result))
	   (cursor-id (nth 5 header))
	   (docs   (cadr result)))
      (db.stop cursor-id :mongo mongo)
      (list header docs))))

(defgeneric db.delete (collection object &key)
  (:documentation "
Delete a document from a collection. The *document* field is used to identify the document to
be deleted.  
You can enter a list of documents. In that the server will be contacted to delete each one of these.
It may be more efficient to run a delete script on the server side.
"))

(defmethod db.delete ((collection t) (document (eql nil)) &key (mongo (mongo)))
 )


(defmethod db.delete ((collection string) (document document) &key (mongo (mongo)))
  (mongo-message mongo (mongo-delete 
                        (full-collection-name mongo collection)
                        document) :timeout 0))

(defmethod db.delete ((collection string) (documents cons) &key (mongo (mongo)))
  (dolist (doc documents)
    (db.delete collection doc :mongo mongo)))

(defmethod db.delete ((collection string) (document kv-container) &key (mongo (mongo)))
  (mongo-message mongo (mongo-delete (full-collection-name mongo collection)
                                     (bson-encode-container document)) :timeout 0))

(defmethod db.delete ((collection string) (kv pair) &key (mongo (mongo)))
  (mongo-message mongo (mongo-delete (full-collection-name mongo collection)
                                     (bson-encode (pair-key kv) (pair-value kv))) :timeout 0))

;
; key -> (string asc)
; asc -> t | nil
; keys -> (list key)
;

(defgeneric db.ensure-index (collection keys &key)
  (:documentation "
Create an index specified by the keys in a collection
"))

;;
;; A lot of this 'formatting' has to do with the way the indexes are set up
;; through the java script client. Apperently 1/-1 -> coneverted to float, rather
;; than an integer. This may or may not matter..
;;
	    
(defun asc/desc->+1/-1 (ht)
  (let ((new-ht (make-hash-table :test 'equal)))
    (labels ((conv (value)
	       (cond ((eql value :asc)  1)
		     ((eql value :desc) -1)
		     (t                  value))))
      (with-hash-table-iterator (iterator ht)
	(dotimes (repeat (hash-table-count ht))
	  (multiple-value-bind (exists-p key value) (iterator)
	    (when exists-p (setf (gethash key new-ht) (conv value)))))))
    new-ht))
	    

(defmethod db.ensure-index ((collection string) (index hash-table)
			    &key (mongo (mongo)) (unique nil) (drop-duplicates nil))
  (assert (typep unique 'boolean))
  (let ((index (asc/desc->+1/-1 index)))
    (labels ((ht->list (ht)
	       (let ((lst ()))
		 (with-hash-table-iterator (iterator ht)
		   (dotimes (repeat (hash-table-count ht))
		     (multiple-value-bind (exists-p key value) (iterator)
		       (if exists-p (push (list key (floor value)) lst)))))
		 lst))
	       ;------------------------------------------------------------------
	     (force-float (ht)
	       (let ((new-ht (make-hash-table :test 'equal)))
		 (with-hash-table-iterator (iterator ht)
		   (dotimes (repeat (hash-table-count ht))
		     (multiple-value-bind (exists-p key value) (iterator)
		       (when exists-p (setf (gethash key new-ht) (float value))))))
		 new-ht))
	     ;----------------------------------------------------------------------
	     (spec-gen (unique drop-dups)
	       (let* ((ukv  (when unique    (kv "unique"   unique)))
		      (dkv  (when drop-dups (kv "dropDups" drop-dups)))
		      (both (when (and ukv dkv) (kv ukv dkv))))
		 (cond (both both)
		       (ukv  ukv)
		       (dkv  dkv)
		       (t    nil))))
	     ;----------------------------------------------------------------------
	     (keys->name (k)
	       (format nil "~{~{~a~^_~}~^_~}" k)))
      (db.insert "system.indexes" 
		 (kv (kv "ns"   (full-collection-name mongo collection))
		     (kv "key"  (force-float index))
		     (spec-gen unique drop-duplicates)
		     (kv "name" (keys->name (ht->list index))))))))

(defmethod db.ensure-index ((collection string) (index kv-container)
			    &key (mongo (mongo)) (unique nil) (drop-duplicates nil))
  (db.ensure-index collection (kv->ht index) :mongo mongo :unique unique 
		   :drop-duplicates drop-duplicates))
		   
(defmethod db.ensure-index ((collection string) (key string)
			    &key (mongo (mongo))  (unique nil) (asc t) (drop-duplicates nil))
  (let ((order-id (if asc 1 -1)))
    (db.ensure-index collection (kv->ht (kv key order-id))
		     :mongo mongo :unique unique :drop-duplicates drop-duplicates)))

(defmethod db.ensure-index ((collection string) (key pair)
			    &key (mongo (mongo)) (unique nil) (drop-duplicates nil))
  (let ((asc (if (eql -1 (pair-value key)) nil t)))
    (db.ensure-index collection (pair-key key)
		     :mongo mongo :unique unique :asc asc :drop-duplicates drop-duplicates)))

(defgeneric db.run-command (cmd &key)
  (:documentation "
Run a database command on the server. See the mongo documentation for a list of commands.  
For most commands you can just uses the key-value shown in the mongo documentation.
"))

;;
;; This converts a symbol to a down/lower case string
;; This will work for most commands (which are lower case)
;; but a few are camel-case so I have overides for those..
;;

(defmethod db.run-command ((cmd symbol) &key (mongo (mongo)) (arg 1))
  (db.run-command (string-downcase cmd) :mongo mongo :arg arg))

(defmethod db.run-command ((cmd string) &key (mongo (mongo)) (arg 1))
  (db.find "$cmd" (kv cmd arg) :limit 1 :mongo mongo))

;does this actually work ? s/b there according to the documentation...
(defmethod db.run-command ((cmd (eql :querytracelevel)) &key (mongo (mongo)) (arg 1))
  (db.run-command "queryTraceLevel" :mongo mongo :arg arg))

(defmethod db.run-command ((cmd (eql :listdatabases)) &key (mongo (mongo)))
  (db.run-command "listDatabases" :mongo mongo))

(defmethod db.run-command ((cmd (eql :serverstatus)) &key (mongo (mongo)))
  (db.run-command "serverStatus" :mongo mongo))

(defmethod db.run-command ((cmd (eql :deleteindexes)) &key (mongo (mongo)) (collection nil) (index "*"))
  (assert (not (null collection)))
  (db.find "$cmd" (kv (kv "deleteIndexes" collection) (kv "index" index)) :mongo mongo))

(defmethod db.run-command ((cmd (eql :dropindexes)) &key (mongo (mongo)) (collection nil) (index "*"))
  (assert (not (null collection)))
  (db.find "$cmd" (kv (kv "dropIndexes" collection) (kv "index" index)) :mongo mongo))

(defmethod db.run-command ((cmd (eql :drop)) &key (mongo (mongo)) (collection nil) (index "*"))
  (assert (not (null collection)))
  (db.find "$cmd" (kv->ht (kv "drop" collection)) :mongo mongo))


#|
   

;; special commands

|#


(defmethod db.indexes (&key (mongo (mongo)))
  "Return all indexes in the database."
  (db.find "system.indexes" :all :mongo mongo))

(defmethod db.collections (&key (mongo (mongo)))
  "Show all the collections in the current database."
  (db.find "system.namespaces" 0 :mongo mongo))

(defgeneric db.distinct (collection key &key)
  (:documentation "Return all the distinct values of this key in the collection "))

(defmethod db.distinct ((collection string) (key string) &key (mongo (mongo)))
  (db.find "$cmd" (kv (kv "distinct" collection) (kv "key" key)) :limit 1 :mongo mongo))
	   

(defun count-it(collection key)
  (db.find "$cmd" (kv (kv "distinct" collection) (kv "key" key))))

(defgeneric db.count (collection selector &key)
  (:documentation "
Count all the collections satifying the criterion set by the selector. 
:all can be used to return a count of
all the documents in the collection.
"))

(defmethod db.count ((collection t) (selector t) &key (mongo (mongo)))
  (db.find "$cmd" (kv (kv "count" collection) (kv "query" selector) (kv "fields" nil)) :mongo mongo :limit 1))

(defmethod db.count ((collection t) (selector (eql :all)) &key (mongo (mongo)))
  (db.count collection nil :mongo mongo))

(defmethod db.count ((collection t) (selector pair) &key (mongo (mongo)))
  (db.count collection (kv->ht selector) :mongo mongo))

(defgeneric db.eval (code &rest rest)
  (:documentation "run javascript code server side"))

(defmethod db.eval ((code string) &rest args)
  (db.find "$cmd" (kv (kv "$eval" (make-bson-code code)) (kv "args" args))))

(defun hex-md5 (str)
  (ironclad:digest-sequence :md5 (babel:string-to-octets str)))

(defgeneric db.create-collection (collection &key)
  (:documentation "create a collection"))

(defgeneric db.add-user (username password &key)
  (:documentation " Add a user to the database. "))

(defmethod db.add-user ((username string) (password string) &key (mongo (mongo)) (readonly nil))
  (let* ((pwd (concatenate 'string username ":mongo:" password))
	 (md5-pwd (hex-md5 pwd))
	 (md5-pwd-str (ironclad:byte-array-to-hex-string md5-pwd)))
    ;(format t "user: ~A ~% pwd : ~A ~% read only : ~A ~%" username md5-pwd-str readonly)
    (db.save "system.users" (kv (kv "user" username) (kv "pwd" md5-pwd-str)) :mongo mongo)))

(defgeneric db.auth (username password &key)
  (:documentation "authenticate a user with a password"))

(defmethod db.auth ((username string) (password string) &key (mongo (mongo)))
  (let* ((nonce (get-element "nonce" (car (docs (db.run-command 'getnonce :mongo mongo)))))
	 (pwd (concatenate 'string username ":mongo:" password))
	 (md5-pwd (hex-md5 pwd))
	 (md5-pwd-str (ironclad:byte-array-to-hex-string md5-pwd))
	 (md5-key (hex-md5 (concatenate 'string nonce username md5-pwd-str)))
	 (md5-key-str (ironclad:byte-array-to-hex-string md5-key))
	 (request (kv (kv "authenticate" 1) (kv "user" username) (kv "nonce" nonce) (kv "key" md5-key-str)))
	 (retval (get-element "ok" (car (docs (db.find "$cmd" request :limit 1 :mongo mongo))))))
    (if retval t nil)))

;;(db.find "$cmd" (kv (kv "count" "foo")  (kv "query" (kv nil nil)) (kv "fields" (kv nil nil))))
;;(db.find "foo" (kv (kv "query" (kv nil nil)) (kv "orderby" (kv "k" 1))) :limit 0)

