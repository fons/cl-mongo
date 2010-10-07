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


(defun perf.db.find ( collection qm  &key (query (bson-encode "query" (kv nil nil)) ) (mongo nil) (options 0) (skip 0) (limit 0) (selector nil) )
  (let ((mongo (or mongo (mongo)))
	(doccount 0)
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
      (multiple-value-bind (header docs) (perf.mongo-reply (query) )
	(incf doccount (nth 7 header))
	(setf next-cursor-id (nth 5 header)) 
	(funcall qm (decode-it docs))
	(loop 
	   (multiple-value-bind (h1 d1) (perf.mongo-reply (get-more next-cursor-id))
	     (incf doccount (nth 7 h1))
	     (setf next-cursor-id (nth 5 h1)) 
	     (funcall qm (decode-it d1))
	     )
	   (when (zerop next-cursor-id) (return nil)))))
    doccount))


(defun print-elem ( elem stream &key (nd nil) )
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
    (pp-doc elem)))

(defun print-str (elem)
  (with-output-to-string (stream)
    (print-elem elem stream)))
    


