(in-package :cl-mongo)

(defun perf.mongo-reply (array)
  (labels ((header (array)
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
      (values head  (list (car head) 36 (nth 7 head)  array)))))


(defun perf.db.find (collection qm  &key (query (bson-encode "query" (kv nil nil))) (mongo (mongo)) (options 0) (skip 0) (limit 0) (selector nil))
  (let ((doccount 0)
	(next-cursor-id 0))
    (labels ((query ()
	       (mongo-message mongo (mongo-query 
				     (full-collection-name mongo collection) query
				     :limit limit
				     :skip skip 
				     :selector (bson-encode-container (expand-selector selector))
				     :options options)))
	     (get-more (cursor-id)
	       (mongo-message mongo (mongo-get-more 
				     (full-collection-name mongo collection) 
				     (int64-to-octet cursor-id) :limit limit)))
	     (decode-it (elem)
	       (bson-decode (nth 0 elem) (nth 1 elem) (nth 2 elem)  (nth 3 elem))))
      (multiple-value-bind (header docs) (perf.mongo-reply (query))
	(incf doccount (nth 7 header))
	(setf next-cursor-id (nth 5 header)) 
	(funcall qm (decode-it docs))
	(loop 
	   (multiple-value-bind (h1 d1) (perf.mongo-reply (get-more next-cursor-id))
	     (incf doccount (nth 7 h1))
	     (setf next-cursor-id (nth 5 h1)) 
	     (funcall qm (decode-it d1)))
	   (when (zerop next-cursor-id) (return nil)))))
    doccount))

(defvar *Q*    nil)
(defvar *R*    nil)
(defvar *RUNNERS* 0)

#|
(defun mongodb-reader (coll lock cv &key (query (bson-encode "query" (kv nil nil))) (mongo (mongo)) (limit 0) (selector nil))
  (lambda ()
    (labels ((queue-data (item)
	       (push item *Q*))
	     (set-runners (val)
	       (bordeaux-threads:acquire-lock lock)
	       (setf *RUNNERS* val)
	       (bordeaux-threads:condition-notify cv)
	       (bordeaux-threads:release-lock lock)))
      (set-runners 1)
      (handler-case 
	  (perf.db.find coll #'queue-data :query query :mongo mongo :limit limit :selector selector)
	(error (c)
	  (format t "~% error ~A" c)))
      (set-runners 0))))
|#
(defun mongodb-reader (coll lock cv &key (query (bson-encode "query" (kv nil nil))) (mongo (mongo)) (limit 0) (selector nil))
  (lambda ()
    (labels ((queue-data (item)
	       (push item *Q*))
	     (set-runners (val)
	       (bordeaux-threads:acquire-lock lock)
	       (setf *RUNNERS* val)
	       (bordeaux-threads:condition-notify cv)
	       (bordeaux-threads:release-lock lock)))
      (set-runners 1)
      (perf.db.find coll #'queue-data :query query :mongo mongo :limit limit :selector selector)
      (set-runners 0))))


(defun th-writer (coll lock cv &key (query (bson-encode "query" (kv nil nil))) (mongo (mongo)) (limit 0) (selector nil))
  (bordeaux-threads:make-thread (mongodb-reader coll lock cv :query query :mongo mongo :limit limit :selector selector) :name "th-writer"))


(defun toR (fn)
  (lambda (lst)
    (do ()
	((null lst) 'done)
      (push (funcall fn (pop lst)) *R*))))

(defun queue-reader (fn lock cv lock2 cv2)
  (lambda ()
    (progn
      (bordeaux-threads:acquire-lock lock2)
      (bordeaux-threads:acquire-lock lock)
      (when (= -1 *RUNNERS*)
	(bordeaux-threads:condition-wait cv lock))
      (block top
	(loop
	   (progn
	     (do ()
		 ((null *Q*) 'done)
	       (funcall fn (pop *Q*)))
	     (when (zerop *RUNNERS*)
	       (progn
		 (bordeaux-threads:release-lock lock)
		 (return-from top 'done)))
	     (bordeaux-threads:condition-wait cv lock)))) 
      (bordeaux-threads:condition-notify cv2)
      (bordeaux-threads:release-lock lock2))))
      

(defun th-reader (fun lock-q cv-q lock-r cv-r)
  (bordeaux-threads:make-thread (queue-reader fun lock-q cv-q lock-r cv-r) :name "th-reader"))

  
(defun do-reduce (fn &key (initial-value nil))
  (let ((lvalue (or initial-value (pop *R*))))
    (do ()
	((null *R*) 'done)
      (setf lvalue (funcall fn lvalue (pop *R*))))
    lvalue))

(defun collect-to-hash (ht doc)
  (setf (gethash (doc-id doc) ht) doc)
  ht)

(defun do-query (coll &key (map-fn #'identity) (reduce-fn #'collect-to-hash) (initial-value (make-hash-table :test 'equal :size 100))
		(query (bson-encode "query" (kv nil nil))) (mongo (mongo)) (limit 0) (selector nil))
" Performs a multi-threaded query on a mongo database.  coll is the collection name.  The reduce-fn keyword is used to specify a function which 
will be called for each member of a batch of data returned from mongodb.  
The reduce-fn function is executed while the query for the next batch is in progress. The default for reduce-fn is the identity function. 
The reduction keyword is used to specify a function which is executed when the database queries have finished. 
It's default implementation is to return a hash table, with the mongodb id as the hash key.  The query, mongo, limit and selector keywords are used in the same way as for db.find. 
"
  (let ((lock-q  (bordeaux-threads:make-lock "lock-q"))
	(lock-r  (bordeaux-threads:make-lock "lock-q"))
	(cv-q    (bordeaux-threads:make-condition-variable))
	(cv-r    (bordeaux-threads:make-condition-variable)))
    (setf *Q* nil)
    (setf *R* nil)
    (setf *RUNNERS* -1)
    (th-writer coll lock-q cv-q :mongo mongo :selector selector :query query :limit limit)
    (bordeaux-threads:acquire-lock lock-r t)
    (th-reader (toR map-fn) lock-q cv-q lock-r cv-r)
    (bordeaux-threads:condition-wait cv-r lock-r)
    (do-reduce reduce-fn :initial-value initial-value)))


(defun th-status ()
  (bordeaux-threads:all-threads))
