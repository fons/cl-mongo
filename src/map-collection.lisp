(in-package :cl-mongo)

;;(defun mapcoll (fun collection query limit)
  
(defmethod mapcoll ((fun function) (collection string) (kv t)
                    &key (mongo (mongo)) (options 0) (skip 0) (limit 1) (selector nil))
  (labels ((query ()
             (mongo-message mongo (mongo-query 
                                   (full-collection-name mongo collection) kv
                                   :limit limit 
                                   :skip skip 
                                   :selector (bson-encode-container (expand-selector selector))
                                   :options options))))
    (multiple-value-bind (header docs) (mongo-reply (query))
      (declare (ignore header))
      (let ((lst ()))
        (dolist (doc docs lst) 
          (push (funcall fun doc) lst))))))

(defmethod mapcoll ((fun function) (collection string) (kv (eql :all)) 
		    &key (mongo (mongo)) (options 0) (skip 0) (limit 0) (selector nil))
  (call-next-method fun collection (bson-encode "query" (kv nil nil))
	   :mongo mongo :options options :skip skip :limit limit :selector selector))

#|
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

(defmethod db.next ((collection string) (cursor-id integer) &key (mongo (mongo)) (limit 0))
  (labels ((get-more ()
             (mongo-message mongo (mongo-get-more 
                                   (full-collection-name mongo collection) 
                                   (int64-to-octet cursor-id) :limit limit))))
    (multiple-value-bind (header docs) (mongo-reply (get-more))
      (list (append header (list collection)) docs))))



(defmethod db.iter ((result cons) &key (mongo (mongo)) (limit 0))
  (multiple-value-bind (iterator collection docs) (db.iterator result) 
    (multiple-value-bind (header docs*) (values-list (db.next collection iterator :mongo mongo :limit limit))
      (list header (append docs docs*)))))

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

(defun iter (result &key (mongo (mongo)) (max-per-call 0))
  "Exhaustively iterate through a query. The maximum number of responses 
   per query can be specified using the max-per-call keyword."
    (loop 
       ;;(format t "length of result set : ~A~%" (length result))
       (setf result (db.iter result :mongo mongo :limit max-per-call))
       (when (zerop (db.iterator result)) (return result))))
|#
