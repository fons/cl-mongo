(in-package :cl-mongo)

#|
 shell commands
|# 

(defun docs (result)
  "Stop the iterator (if any) and return the list of documents returned by the query. 
Typical ue would be in conjunction with db.find like so (docs (iter (db.find 'foo' 'll)))"
  (cadr (db.stop result)))
  
(defun iter (result &key (mongo (mongo)) (max-per-call 0))
  "Exhaustively iterate through a query. The maximum number of responses 
   per query can be specified using the max-per-call keyword."
    (loop 
       ;; (format t "length of result set : ~A~%" (length result))
       (setf result (db.iter result :mongo mongo :limit max-per-call))
       (when (zerop (db.iterator result)) (return result))))

(defun rm* (result &key (mongo (mongo)))
  "Delete all the documents returned by a query. This is not an efficient 
way of deleting documents as it invloves multiple trips to the server.
Mongo allows for execution of java-script on the server side, which provides
an alternative. Typical use would be (rm (iter (db.find 'foo' (kv 'key' 1)))),
which deletes all documents in foo, with field key equal to 1."
(multiple-value-bind (iterator collection docs) (db.iterator result)
  (db.stop iterator :mongo mongo)
  (db.delete collection docs)))
;;
;; rm is is set up to handle insert/delete race conditions which may appear..
;;
(defun rm (collection query &key (mongo (mongo)))
  "Delete all the documents returned by a query. This is not an efficient 
way of deleting documents as it invloves multiple trips to the server.
Mongo allows for execution of java-script on the server side, which provides
an alternative. Typical use would be (rm (iter (db.find 'foo' (kv 'key' 1)))),
which deletes all documents in foo, with field key equal to 1."
  (labels ((server-count()
	     (get-element "n" (car (docs (db.count collection query :mongo mongo)))))
	   (delete-docs ()
	     (db.delete collection (docs (iter (db.find collection query :selector "_id" :mongo mongo :limit 0))) :mongo mongo)))
    (do ((line (server-count)
	       (server-count)))
	((zerop line))
      (delete-docs))))

(defvar *indent*)

(defun new-line (stream)
  (if (zerop *indent*)
      (format stream "~%")
      (format stream "~%~VT" *indent*)))

(defmacro with-indent (&body body)
  `(let ((*indent* (+ 2 *indent*)))
     ,@body))

(defgeneric pp (result &key)
  (:documentation "
Pretty-print the results returned from a query. To be used on the repl. 
This will format each server reply as if it were a document. This is obviously
ok in mosty cases. See nd for an alternative.
"))

(defmethod pp ((result (eql nil)) &key)
  nil)

(defmethod pp ((result cons) &key (stream t) (nd nil))
  (let ((str 
	 (with-output-to-string (stream)
	   (pp* result :stream stream :nd nd))))
    (format stream "~A~%" str)))

(defmethod pp* ((result cons) &key (stream t) (nd nil))
  (labels ((br+ ()
	     (unless nd
               (new-line stream)
               (format stream "{")))
	   (br- ()
	     (unless nd
               (new-line stream)
               (format stream "}")))
	   (pp-oid (value)
             (with-indent
               (new-line stream)
               (format stream "\"_id\" -> objectid(")
               (let* ((arr  (id value))
                      (size (length arr)))
                 (dotimes (index size)
                   (let ((x (aref arr index)))
                     (if (< x 10)
                         (format stream "0~1X" x)
                         (format stream "~2X" x)))))
               (format stream ")")))
	   (pp* (value)
             (typecase value
               (document   (pp-doc  value))
               (hash-table (pp-ht   value))
               (cons       (pp-cons value))
               (bson-oid   (pp-oid  value))
               (otherwise  (format stream "~A" value))))
	   (pp-cons (list)
             (loop
                initially (format stream "[")
                for (el . next?) on list
                do
                  (pp* el)
                  (when next?
                    (format stream ", "))
                finally (format stream "]")))
	   (pp-doc (d)
             (with-indent
               (br+)
               (unless (_local d)
                 (pp* (_id d)))
               (pp* (elements d))
               (br-)))
	   (pp-ht (ht)
             (with-indent
               (maphash (lambda (key value)
                          (new-line stream)
                          (format stream "\"~A\"  -> " key)
                          (pp* value)) ht))))
    (let ((*indent* 0))
      (dolist (d (docs result))
        (pp-doc d)))))

(defun nd (result &key (stream t))
  "Pretty-print for non-document responses, like the response to a database command."
  (pp result :stream stream :nd t))

(defun bind-return (val result)
  "return value bound to val in a return document. Used for count etc.."
  (get-element val (car (docs result))))

(defun ret (result)
  "return value bound to retval in a return document. Used for db.count, db.distinct, functions etc.."
  (let ((inspect (list "n" "values" "retval")))
    (dolist (key inspect)
      (multiple-value-bind (value found) (bind-return key result)
        (when found
          (return-from ret value))))))


(defun now ()
  "Return the current date and time in bson format.  "
  (make-bson-time))

(defun date-time (second minute hour day month year &optional (time-zone *bt-time-zone*))
  "Generate a time stamp the mongo/bson protocol understands."
  (make-bson-time (gmt-to-bson-time (encode-universal-time second minute hour day month year time-zone))))

(defgeneric elp (element))

(defmethod elp (element)
  (format nil "~A" element))

(defmethod elp ((doc document))
  (let ((str ""))
    (dolist (el (doc-elements doc))
      (setf str (concatenate 'string (format nil "(~A : ~A) " el (elp (get-element el doc))) str)))
    str))

(defmethod elp ((element cons))
  (mapcar #'elp element))

(defun print-doc (doc &key (order (list :rest)) (nl nil) (msg ""))
  (let ((lst)
	(elements (doc-elements doc)))
    (labels ((pr-_id()
	       (when (and (not (_local doc))
                          (or (find "_id" order)
                              (find :rest order)))
		 (format t "{_id : ~A} " (_id doc))
		 (when nl (format t "~%"))))
	     (gen-traversal ()
	       (dolist (el (remove "_id" order :test #'equal))
		 (if (eql el :rest)
		     (setf lst (concatenate 'cons  elements lst))
		     (progn
		       (push el lst)
		       (setf elements (remove el elements :test #'equal)))))
	       (nreverse lst)))
      (when msg (format t "~A" msg))
      (unless nl (format t "["))
      (pr-_id)
      (dolist (el (gen-traversal))
	(progn
	  (format t "{~A : ~A} " el (elp (get-element el doc)))
	  (when nl (format t "~%"))))
      (unless nl (format t "]~%")))))

(defgeneric show (things &key)
  (:documentation "Print a list of things. Things can be users, databases, 
collections in the current database,
the profile and more. Things is a keyword so (show :users) will show all users."))

(defmethod show ((things (eql :connections)) &key)
  (mongo-show))
  
(defmethod show ((func function) &key (order '(:rest)) (nl nil) (msg nil))
  (mapcar (lambda (doc) (print-doc doc :order order :nl nl :msg msg)) (docs (iter (funcall func))))
  nil)

(defmethod show ((things (eql :collections)) &key (order '("name" :rest)))
  (show #'db.collections :order order))

(defmethod show ((things (eql :indexes)) &key (order '("name" :rest)))
  (show #'db.indexes :order order))

(defmethod show ((things (eql :databases)) &key (order '("totalSize" "databases")))
  (show (lambda () (db.run-command :listdatabases)) :order order))

(defmethod show :before ((things (eql :databases)) &key)
  (db.use "admin"))

(defmethod show :after ((things (eql :databases)) &key)
  (db.use -))

(defmethod show ((things (eql :buildinfo)) &key (order '("version" "sysInfo" "gitVersion" :rest)))
  (show (lambda () (db.run-command :buildinfo)) :order order :nl t))

(defmethod show :before ((things (eql :buildinfo)) &key)
  (db.use "admin"))

(defmethod show :after ((things (eql :buildinfo)) &key)
  (db.use -))

(defmethod show ((things (eql :users)) &key (order '("user" "pwd")))
  (show (lambda () (db.find "system.users" 'all)) :order order))

(defmethod show ((things (eql :status)) &key (order '(:rest)))
  (show (lambda () (db.run-command :serverstatus)) :order order :nl t))

(defmethod show ((things (eql :assertinfo)) &key (order '(:rest)))
  (show (lambda () (db.run-command things)) :order order :nl t))


(defmethod show ((things (eql :lasterror)) &key (order '(:rest)))
  (show (lambda () (db.run-command :getlasterror)) :order order :msg "last error : "))

(defmethod show ((things (eql :preverror)) &key (order '(:rest)))
  (show (lambda () (db.run-command :getpreverror)) :order order :msg "last error : "))

(defmethod show ((things (eql :errors)) &key (order '(:rest)))
  (declare (ignore order))
  (show :lasterror)
  (show :preverror))

;;---------------------

