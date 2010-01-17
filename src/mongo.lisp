(in-package :cl-mongo)

#|
  Connection...
|#

(defconstant +MONGO-PORT+ 27017)

(defvar *mongo-registry* () "list of all open mongo connections")
(defvar *db.use-history* () "an attempt to record the history of the shell" )

(defclass mongo ()
  ((port   :reader   port             :initarg :port)
   (host   :reader   host             :initarg :host)
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

(defgeneric mongo-stream (mongo)
  (:documentation "mongo stream socket.."))

(defmethod mongo-stream ( (mongo mongo) )
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
    (close (mongo-stream mongo))))

(defgeneric mongo-message (mongo message &key ) 
  (:documentation "message to/from mongo.."))

(defmethod mongo-message ( (mongo mongo) (message array) &key (timeout 5) )
  (write-sequence message (mongo-stream mongo))
  (force-output (mongo-stream mongo))
  (usocket:wait-for-input (list (socket mongo) ) :timeout timeout)
  (if (listen (mongo-stream mongo))
      (progn 
	(let* ((reply  (make-octet-vector 1000 :init-fill 4 )) 
	       (cursor (read-sequence reply (mongo-stream mongo) :start 0 :end 4))
	       (rsz    (octet-to-int32 (subseq reply 0 4))))
	  (unless (array-in-bounds-p reply rsz) (adjust-array reply rsz))
	  (setf (fill-pointer reply) rsz) 
	  (read-sequence reply (mongo-stream mongo) :start cursor)
	  reply))
      nil))


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



#|
  db shell functions
|#

;; special commands + : up -:down

(defun cwd ( &key (mongo nil) )
  (db (or mongo (mongo))))

(defun nwd ()
  (cadr *db.use-history*))
