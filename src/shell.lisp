(in-package :cl-mongo)

#|
 shell commands
|# 

(defun docs ( result )
"
Stop the iterator (if any) and return the list of documents returned by the query. 
Typical ue would be in conjunction with db.find like so (docs (iter (db.find 'foo' 'll)))
"
  (cadr (db.stop result)))
  
(defun iter ( result &key (mongo nil) (max-per-call 0) )
"
Exhaustively iterate through a query. The maximum number of responses 
per query can be specified using the max-per-call keyword.

"
    (loop 
       (setf result (db.iter result :mongo mongo :limit max-per-call ) )
       (when (zerop (db.iterator result) ) (return result) )))

(defun rm (result &key (mongo nil) )
"
Delete all the documents returned by a query. This is not an efficient 
way of deleting documents as it invloves multiple trips to the server.
Mongo allows for execution of java-script on the server side, which provides
an alternative. Typical use would be (rm (iter (db.find 'foo' (kv 'key' 1)))),
which deletes all documents in foo, with field key equal to 1.
"
(multiple-value-bind (iterator collection docs) (db.iterator result) 
  (db.stop iterator :mongo mongo)
  (db.delete collection docs)))

(defgeneric pp (result &key)
  (:documentation "
Pretty-print the results returned from a query. To be used on the repl. 
This will format each server reply as if it were a document. This is obviously
ok in mosty cases. See nd for an alternative.
"))

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
"
Pretty-print for non-document responses, like the response to a database command.
"
  (pp result :stream stream :nd t))

(defun now()
"
Return the current date and time in bson format.  
"
(make-bson-time))

(defun date-time (second minute hour day month year &optional (time-zone *bt-time-zone*) )
  "Generate a time stamp the mongo/bson protocol understands."
  (make-bson-time (gmt-to-bson-time (encode-universal-time second minute hour day month year time-zone))))

