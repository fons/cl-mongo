(in-package :cl-mongo-test)

#|
(defun gtb ( loc )
  (let ((res 255))
    (dotimes (var loc)
      (setf res (* res 256)))
    res ))

(defun gti (size) 
  (let ((value 0))
    (dotimes (i size)
      (setf value (+ value (gtb i))))
    value))

(defun test-int32-code-decode()
  (let ((value (gti 4)))
    (format t "value ~A ~%" value)
    (octet-to-int32 (int32-to-octet value))))

(defun test-int64-code-decode()
  (let ((value (gti 8)))
    (format t "value ~A ~%" value)
    (octet-to-int64 (int64-to-octet value))))
    

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


(defun send-doc-in-doc ( collection )
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
      (db.insert collection doc ))))


|#

(defun send-doc-in-doc-in-doc (collection)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
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
			       (db.insert collection doc ))))))


(defun test-doc-simple (collection)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
			 (let ((doc (make-document)))
			   (add-element "string" "hello world generated by test-document" doc)
			   (add-element "1" 1 doc)
			   (add-element "2" 2 doc)
			   (add-element "3" 3 doc)
			   (add-element "4" 40 doc)
			   (add-element "5" 30 doc)
			   (db.insert collection doc ))))

(defun send-test-document ( collection )
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
			 (let ((doc (make-document)))
			   (add-element "string" "hello world generated by test-document" doc)
			   (add-element "number89" 89 doc)
			   (add-element "numberbig" 89988787777887 doc)
			   (add-element "float 89.67" 89.67 doc)
			   (add-element "list-1" (list 1 2 3 4) doc)
			   (add-element "list-2" (list 1 (list "inside" 3 4 "list" ) "the end") doc)
			   (db.insert collection doc ))))

(defun insert-lots (collection n)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
			 (dotimes (i n)
			   (let ((doc (make-document)))
			     (add-element (format  nil "name") "simple" doc)
			     (add-element (format nil "~D" i) i doc)
			     (add-element (format nil "k") (+ 56.00 i) doc)
			     (add-element (format nil "l") (- i 9.00)  doc)
			     (add-element (format nil "index-this") i doc)
			     (add-element (format nil "value-1") (* 78 i) doc)
			     (db.insert collection doc )))))


(defun count-documents ( collection )
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
    (let ((client-count (* 1.0 (length (docs (iter (db.find collection :all))))))
	  (server-count (* 1.0 (get-element "n" (car (docs (db.count collection :all)))))))
      (format t "client count : ~A -- server count ~A ~%" client-count server-count)
      (eql client-count server-count))))
			

(defun delete-docs (collection &key (size 5) )
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
    (let ((cnt (* 1.0 (get-element "n" (car (docs (db.count collection :all)))))))
      (unless (> cnt 0) (insert-lots collection size)))
    (rm collection :all)
    (let ((cnt (* 1.0 (get-element "n" (car (docs (db.count collection :all)))))))
      (format t "count : ~A ~%" cnt)
      (eql 0.0 cnt))))


(defun find-docs (collection &key (size 5))
  (rm collection :all)
  (insert-lots collection size)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
    (let ((found (length (docs (iter (db.find collection (kv "name" "simple")))))))
      (format t "findOne is default : ~A ~%" found)
      (eql found 1)))
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
    (let ((found (length (docs (iter (db.find collection (kv "name" "simple") :limit 0))))))
      (format t "limit == 0 returns all : ~A ~%" found)
      (eql found size))))

