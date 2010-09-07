(in-package :cl-mongo)


;;----------------------------------------------------------------------------------------

;; perf.extract-elements rewrite

(defun end-of-key (start array)
  (let ((eol start)) 
    (do ( (pos start (+ pos 1) ) )
	( (= (elt array pos) 0) ) 
      (incf eol)
      )
    eol))


(defun perf.extract-elements (totlen pos docs array &key (container #'ht->document.1 ) ) 
  (block nil
    (let ((lst () ) )
      (when (zerop docs) (return lst))
      (tagbody
       start-document
	 (progn
	   (let* ((ht      (make-hash-table :test #'equal :size 10))
		  (end (+ (- pos 1) (octet-to-int32.1 array pos)))) 
	     (incf pos 4)
	     (tagbody
	      get-key-value
		(let* ((type  (elt array pos))
		       (spos  (+ pos 1))
		       (epos  (end-of-key pos array) )
		       (key   (babel:octets-to-string array :start spos :end epos)))
		  (setf pos (+ 1 epos))

		  (cond 
		    ( (= type +bson-data-number+) (progn
						    (setf (gethash key ht) (decode-double-float-bits (octet-to-uint64.1 array pos)))
						    (incf pos 8)
						    ))
		    
		    
		    ((= type +bson-data-string+) (progn
						   (let* ((size (octet-to-int32.1 array pos ))
							  (npos (+ 4 pos))
							  (eos  (+ 3 pos size)) ;;do not include null
							  (value  (babel:octets-to-string array :start npos :end eos )))
							    (setf (gethash key ht) value)
							    (setf pos (+ 1 eos))
							    )
						   ) )
		    
		    ( (= type +bson-data-object+) (progn
						    (let* ((size (octet-to-int32.1 array pos ))
							   (eos  (- (+ pos size) 1) )) 
						      (setf (gethash key ht) (car (perf.extract-elements eos pos 1 array )))
							  (setf pos (+ 1 eos))
							  )
						    ))
		    
		    ( (= type +bson-data-array+) (progn
						   (let* ((size (octet-to-int32.1 array pos ))
							  (eos  (- (+ pos size) 1) )) 
						     (setf (gethash key ht) (car (perf.extract-elements eos pos 1 array :container #'ht->list.1)))
						     (setf pos (+ 1 eos))
						     )
						   ))

		    ( (= type +bson-data-binary+) (progn
						    (let* ((binarysize (octet-to-int32.1 array pos))
							   (totalsize  (+ 5 binarysize))
							   (type       (octet-to-byte.1  array (+  4 pos)))
							   (size (if (eql type #x02) (octet-to-int32.1 array (+ pos 5) ) (octet-to-int32.1 array pos)))
							   (offset (if (eql type #x02) 9 5))
							   (binary  (bson-binary type (subseq array offset (+ offset size)))))
						      (setf (gethash key ht) binary)
						      (incf pos totalsize)
						    )
						    ))

		    ( (= type +bson-data-undefined+   ) (progn
							  (setf (gethash key ht) nil)
							  ))
		    
		    ( (= type +bson-data-oid+   ) (progn
						    (let ((npos  (+ pos 12)))
						      (setf (gethash key ht) (make-bson-oid :oid (subseq array pos npos)))
						      (setf pos npos) 
							     )
						    ) )

		    ( (= type +bson-data-boolean+) (progn
						     ;;(values 
						     (setf (gethash key ht) (byte-to-bool (octet-to-byte.1 array pos)))
						     (incf pos 1)
						     ))

		    ( (= type +bson-data-date+   ) (progn
						     (setf (gethash key ht) (make-bson-time (octet-to-uint64.1 array pos)))
						     (incf pos 8)
						     ))
		    
		    ( (= type +bson-data-null+   ) (progn
						     (setf (gethash key ht) nil)
						     ))

		    ( (= type +bson-data-regex+   ) (progn
						      (let* ((eregex    (end-of-key pos array) )
							     (regex     (babel:octets-to-string array :start pos :end eregex))
							     (npos      (+ 1 eregex))
							     (eopt      (end-of-key npos array))
							     (options   (babel:octets-to-string array :start npos :end eopt)) )
							(setf (gethash key ht) (make-bson-regex regex options))
							(setf pos (+ 1 eopt)) 
							)
						      ))
		    
	
		    ( (= type +bson-data-dbpointer+ ) (progn
							(let ((npos  (+ pos 12)))
							  (setf (gethash key ht) (subseq array pos npos))
							  (setf pos npos) 
							  )
							) )


		    ((= type +bson-data-code+) (progn
						   (let* ((size (octet-to-int32.1 array pos ))
							  (npos (+ 4 pos))
							  (eos  (+ 3 pos size)) ;;do not include null
							  (value  (babel:octets-to-string array :start npos :end eos )))
							    (setf (gethash key ht) value)
							    (setf pos (+ 1 eos))
							    )
						   ) )

		    ((= type +bson-data-symbol+) (progn
						   (let* ((size (octet-to-int32.1 array pos ))
							  (npos (+ 4 pos))
							  (eos  (+ 3 pos size)) ;;do not include null
							  (value  (babel:octets-to-string array :start npos :end eos )))
						     (setf (gethash key ht) value)
						     (setf pos (+ 1 eos))
						     (intern value :cl-user)
						     )
						   ))

		    ((= type +bson-data-code_w_s+) (progn
						   (let* ((total-size (octet-to-int32.1 array pos ))
							  (npos (+ 4 pos))
							  (string-size (octet-to-int32.1 array npos ) )
							  (start-of-string (+ 4 npos))
							  (end-of-string   (+ 3 npos string-size)) ;;do not include null
							  (javascript      (babel:octets-to-string array :start start-of-string :end end-of-string ))
							  (eojs            (+ 1 end-of-string))
							  (env             (car (perf.extract-elements (+ pos total-size) eojs 1 array ))) )
						     (setf (gethash key ht) (list javascript env))
						     (incf pos total-size)
						     )
						   ))
		      
		    ( (= type +bson-data-int32+  ) (progn
						     (setf (gethash key ht) (octet-to-int32.1 array pos))
						     (incf pos 4)
						     ))
		    
		    ( (= type +bson-data-timestamp+  ) (progn
							 (setf (gethash key ht) (octet-to-int64.1 array pos))
							 (incf pos 8)
							 ))
		    
		    ( (= type +bson-data-long+  ) (progn
						    (setf (gethash key ht) (octet-to-int64.1 array pos))
						    (incf pos 8)
						    ))
		    

		    ( (= type +bson-data-min-key+ ) (progn
						      (setf (gethash key ht) nil)
						      ))

		    ( (= type +bson-data-max-key+ ) (progn
						      (setf (gethash key ht) nil)
						      ))

		    ( t          (error "error : unable to process this type : ~A " type))

		    ) ;; end of condition on type
		(when (< pos end) (go get-key-value))) )
	     (incf pos)
	     (push (funcall container ht) lst) )) 
	 (decf docs)
	 (when (= totlen pos) (return lst))
	 (when (zerop docs)   (return lst))
	 (go start-document)) )))


;;(print-hash (car (cadr (perf.db.find "test" :all :limit 1) )) t)
;;--------------------------------------------------------------------------

(defun insert-test-docs (collection n &key (host "localhost" ) (port *mongo-default-port*) (db "test"))
  (with-mongo-connection (:host host  :port port :db db)
    (dotimes (i n)
      (let ((doc (make-document)))
	(add-element (format nil "index-field") i doc)
	(add-element "float" (+ 0.98 (* i 0.7656443497654332122)) doc) 
	(add-element "date-time" (date-time 12 34 16 5 4 2010) doc)
	(db.insert collection doc )))))
							  
(defun insert-test-array (collection n &key (host "localhost" ) (port *mongo-default-port*) (db "test"))
  (with-mongo-connection (:host host  :port port :db db)
    (let ((lst ()))
      (dotimes (i n)
	(push i lst))
      (db.insert collection (kv "array" lst )))))
							  
(defun insert-doc-in-doc (collection n &key (host "localhost" ) (port *mongo-default-port*) (db "test"))
  (with-mongo-connection (:host host  :port port :db db)
    (dotimes (i n)
      (let ((doc (make-document)))
	(add-element (format nil "index-field") i doc)
	(add-element "float" (+ 0.98 (* i 0.7656443497654332122)) doc) 
	(add-element "date-time" (date-time 12 34 16 5 4 2010) doc)
	(let ((doc2 (make-document)))
	  (add-element "doc2 text"   "hello world" doc2)
	  (add-element "doc2 number" 56.89776      doc2)
	  (add-element "doc2" doc2 doc) 
	  (db.insert collection doc ))))))

(defun idd ()
  (with-mongo-connection (:host "localhost"  :port *mongo-default-port* :db "test")
    (let ((doc (make-document)))
      ;;(add-element "doc1" "document 1" doc)
      ;;(add-element "float" (+ 0.98 (* 2 0.7656443497654332122)) doc) 
      (let ((doc2 (make-document)))
	(add-element "doc2 text"   "hello world" doc2)
	(add-element "doc2 number" 56.89776      doc2)
	(add-element "doc2" doc2 doc) 
	(db.insert "foo" doc )))))


;;;---------------- perf.mongo-reply ----------------------------------------------------------------

(defun perf.mongo-reply ( array )
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
      (values head  (extract-elements (car head) 36 (nth 7 head)  array)))))

;;-----------perf.db.find ......................................



#|
(defun insert-test-docs (collection n &key (host "localhost" ) (port +mongo-port+) (db "test"))
  (with-mongo-connection (:host host  :port port :db db)
    (dotimes (i n)
      (let ((doc (make-document)))
	(add-element (format nil "index-field") i doc)
	(add-element "float" (+ 0.98 (* i 0.7656443497654332122)) doc) 
	(add-element "date-time" (date-time 12 34 16 5 4 2010) doc)
	(db.insert collection doc )))))
							  
(defun insert-test-array (collection n &key (host "localhost" ) (port +mongo-port+) (db "test"))
  (with-mongo-connection (:host host  :port port :db db)
    (let ((lst ()))
      (dotimes (i n)
	(push i lst))
      (db.insert collection (kv "array" lst )))))
							  
(defun insert-doc-in-doc (collection n &key (host "localhost" ) (port +mongo-port+) (db "test"))
  (with-mongo-connection (:host host  :port port :db db)
    (dotimes (i n)
      (let ((doc (make-document)))
	(add-element (format nil "index-field") i doc)
	(add-element "float" (+ 0.98 (* i 0.7656443497654332122)) doc) 
	(add-element "date-time" (date-time 12 34 16 5 4 2010) doc)
	(let ((doc2 (make-document)))
	  (add-element "doc2 text"   "hello world" doc2)
	  (add-element "doc2 number" 56.89776      doc2)
	  (add-element "doc2" doc2 doc) 
	  (db.insert collection doc ))))))

(defun idd ()
  (with-mongo-connection (:host "localhost"  :port +mongo-port+ :db "test")
    (let ((doc (make-document)))
      ;;(add-element "doc1" "document 1" doc)
      ;;(add-element "float" (+ 0.98 (* 2 0.7656443497654332122)) doc) 
      (let ((doc2 (make-document)))
	(add-element "doc2 text"   "hello world" doc2)
	(add-element "doc2 number" 56.89776      doc2)
	(add-element "doc2" doc2 doc) 
	(db.insert "foo" doc )))))

|#