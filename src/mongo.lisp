(in-package :cl-mongo)

#|
  Connection...
|#

(defconstant +mongo-port+ 27017)

(defvar *mongo-registry* nil "hash table of all open mongo connections")
(defvar *db.use-history* () "an attempt to record the history of the shell")

(defvar *mongo-default-host* "localhost"  "host for the default connection.")
(defvar *mongo-default-port* +MONGO-PORT+ "port for the default connection.")
(defvar *mongo-default-db*   "admin"      "database opened by the default connection")


(defclass mongo ()
  ((port   :reader   port             :initarg  :port)
   (host   :reader   host             :initarg  :host)
   (name   :accessor name             :initarg  :name)
   (socket :accessor socket           :initarg  :socket)
   (id     :reader   id               :initform (uuid:make-v4-uuid))
   (db     :accessor db               :initarg  :db))
  (:documentation " Encapsulates the connection to the mongo database.
Each connection is a added to a global registry."))

(defun mongo-registry ()
  (or *mongo-registry* (setf *mongo-registry* (make-hash-table :test 'equalp))))

(defun error-on-key-exists (key)
  (if (null (gethash key (mongo-registry)))
      key
      (error "duplicate key registration for key ~A ~%" key)))

(defun add-to-registry (mongo)
  (let ((key (error-on-key-exists (name mongo))))
    (setf (gethash key (mongo-registry)) mongo)
    key))

(defmethod initialize-instance :after ((mongo mongo) &key)
  (labels ((socket* (host port)
             (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
    (setf (socket mongo) (socket* (host mongo) (port mongo)))))

(defmethod shared-initialize :after ((mongo mongo) slots &key)
  (add-to-registry mongo))

;;
;; name is setfable so I need to ovveride that version !
;;

(defun make-mongo (&key (host *mongo-default-host*) (port *mongo-default-port*)
                     (db *mongo-default-db*) (name (gensym)))
  (make-instance 'mongo :host host :port port :db db :socket nil :name name))

(defmethod print-object ((mongo mongo) stream)
  (format stream "(type-of ~S) [name : ~A ] ~% {[id : ~A] [port : ~A] [host : ~A] [db : ~A]} ~%"
          (type-of mongo)
	  (if (slot-boundp mongo 'name)
	      (name mongo)
	      "name not set")
	  (if (slot-boundp mongo 'id)
	      (id mongo)
	      "id not set")
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

(defmethod mongo-stream ((mongo mongo))
  (usocket:socket-stream (socket mongo)))

(defgeneric mongo (&key host port db name)
  (:documentation "This method returns the connection referred to by 
the name identifier from the connection registry. The connection name is unique. 
If no connection with that name exists, a new connection with the supplied or default 
host, port and db parameters will be created. The default host is localhost; 
the default port is 27017; the default db is admin."))

(defmethod mongo (&key (host *mongo-default-host*) (port *mongo-default-port*)
                    (db *mongo-default-db*) (name :default))
  (or (gethash name (mongo-registry)) (make-mongo :host host :port port :db db :name name)))

(defgeneric mongo-registered (name)
  (:documentation "Return a conection registered by this name or nil.."))

(defmethod mongo-registered (name)
  (gethash name (mongo-registry)))

(defun mongo-show ()
  " Show all registered connections and their session id"
  (with-hash-table-iterator (iterator (mongo-registry))
    (dotimes (repeat (hash-table-count (mongo-registry)))
      (multiple-value-bind (exists-p key value) (iterator)
	(when exists-p (format t "~% ~A -> ~A " key value))))))

(defgeneric mongo-close (name)
  (:documentation "Close the connection to the mongo database. 
The name should uniquely identify the connection to close.
This is either a mongo object or the name the object is bound to 
in the connection registry. To close all open connections use the special symbol 'all"))

(defmethod mongo-close ((mongo mongo))
  (let ((name (name mongo)))
    (handler-case 
	(socket-close (socket (mongo :name name)))
      (error(c)
	(format t "~% unable to close mongo connection for mongo connection ~A: error ~A" mongo c)))
    (remhash name (mongo-registry))))

; This is the base case. This means that we can be pretty lieberal with what is used as a 'name'.
; This can be either a string, symbol or bson-oid, or whatever else strikes your fancy..
(defmethod mongo-close (name)
  (mongo-close (mongo :name name))
  (remhash name (mongo-registry)))

(defmethod mongo-close ((name (eql :all)))
  (with-hash-table-iterator (iterator (mongo-registry))
    (dotimes (repeat (hash-table-count (mongo-registry)))
      (multiple-value-bind (exists-p key mongo) (iterator)
	(declare (ignore mongo))
	(when exists-p (mongo-close key))))))

(defgeneric mongo-swap (left right)
  (:documentation "Swap the names of the left and right connections. Typical use would be 
`(mongo-swap :default :alt)`. After the function call :default will refer to the connection
previously referred to as :alt. A connection named :default is returned by `(mongo)` and is the default used in the api. The connections are returned in the order they were passed in (but with the names
swapped between them). To re-open a connection you can say 
`(mongo-close (mongo-swap :default (mongo :host <newhost> :portid <portid> :name :temp)))` 
and a new default connection is registered."))

(defmethod mongo-swap ((left mongo) (right mongo))
  (labels ((tmp-save (mongo)
	     (let ((tmp (gensym)))
	       (setf (gethash tmp (mongo-registry)) (gethash (name mongo) (mongo-registry)))
	       tmp))
	   (rename (mongo new-name)
	     (setf (name mongo) new-name)
	     (add-to-registry mongo))
	   (swap-em (left right)
	     (let ((tmp-key  (tmp-save right))
		   (old-name (name left)))
	       (remhash (name right) (mongo-registry))
	       (rename left (name right))
	       (remhash old-name (mongo-registry))
	       (rename (mongo :name tmp-key) old-name)
	       (remhash tmp-key  (mongo-registry)))))
    (swap-em left right)
    (values  left right)))

(defmethod mongo-swap (left right)
  (mongo-swap (mongo :name left) (mongo :name right)))

(defmethod mongo-swap ((left (eql :default)) (right mongo))
  (mongo-swap (mongo :name :default) right))

(defgeneric mongo-message (mongo message &key)
  (:documentation "message to/from mongo.."))

(defun test-for-readback* (stream timeout)
  (declare (ignore timeout))
  (listen stream))

#+clisp
(defun test-for-readback-clisp (stream timeout)
  (multiple-value-bind (direction status) (socket:socket-status (cons stream  :INPUT)  timeout)
    (declare (ignore status))
    (when direction t)))
  
(defun test-for-readback (stream timeout)
  #-(or clisp) (test-for-readback* stream timeout)
  #+clisp (test-for-readback-clisp stream timeout))

(defun read-back (mongo)
  (let* ((reply (make-octet-vector 1000 :init-fill 4))
	 (cursor (read-sequence reply (mongo-stream mongo) :start 0 :end 4))
	 (rsz (octet-to-int32 (subseq reply 0 4))))
    (unless (array-in-bounds-p reply rsz)
      (adjust-array reply rsz))
    (setf (fill-pointer reply) rsz)
    (read-sequence reply (mongo-stream mongo) :start cursor)
    reply))

(defun read-buffer (mongo timeout state &optional (accum nil))
  (if state 
      (read-buffer mongo timeout (test-for-readback (mongo-stream mongo) timeout) (cons (read-back mongo) accum))
      (nreverse accum)))
;  (read-buffer mongo timeout (test-for-readback (mongo-stream mongo) timeout)))

(defun read-ready (mongo timeout)
  (multiple-value-bind (lst val) (usocket:wait-for-input (list (socket mongo)) :timeout timeout)
    (declare (ignore lst))
    (if val
	t
	(read-ready mongo timeout))))
;;
;; If the timeout is set > 0, then output is expected, and the reader will be polled
;; until out is received...
;;

(defmethod mongo-message* ((mongo mongo) (message array) &key (timeout 5))
  (write-sequence message (mongo-stream mongo))
  (force-output (mongo-stream mongo))
  (unless (zerop timeout)
    (read-ready mongo timeout)
    (test-for-readback (mongo-stream mongo) timeout)
    (read-back mongo)))

(defmethod mongo-message ((mongo mongo) (message array) &key (timeout 5))
  (handler-case 
      (mongo-message* mongo message :timeout timeout)
    (error (c)
      (progn
	(format t "error occured when sending message : ~A~%" c)
	(format t "closing connection ~A ~%" mongo)
	(mongo-close mongo)
	nil))))

(defgeneric db.use (db &key)
  (:documentation "Use a database on the mongo server. Opens a connection if one isn't already 
established. (db.use -) can be used to go to a previosuly visited database, 
similar to cd -. "))

(defmethod db.use ((db string) &key (mongo (mongo)))
  (push db *db.use-history*)
  (setf (db mongo) db))

(defmethod db.use ((db cons) &key (mongo (mongo)))
  (when (eql db -)
    (push (cadr *db.use-history*) *db.use-history*)
    (setf (db mongo) (car *db.use-history*))))

;;;
;;; db shell functions
;;;

;; special commands + : up -:down

(defun cwd (&key (mongo (mongo)))
  "Show the current database."
  (db mongo))

(defun nwd ()
  " Show the database set by the `(db.use -)` command"
  (cadr *db.use-history*))

(defun mongo-ids ()
  (let ((lst))
    (with-hash-table-iterator (iterator (mongo-registry))
      (dotimes (repeat (hash-table-count (mongo-registry)))
	(multiple-value-bind (exists-p key value) (iterator)
	  (declare (ignore value))
	  (when exists-p
            (push key lst)))))
    (nreverse lst)))

(defun connection? (name)
  (if (car (multiple-value-list (gethash name (mongo-registry)))) t nil))

(defun default? ()
  (connection? :default))

(defun switch-default-connection (name &key (host "localhost") (db "test") (port +MONGO-PORT+))
  (if (default?)
      (mongo-swap :default (make-mongo :host host :db db :port port :name name))
      (make-mongo :host host :db db :port port :name :default)))

(defun restore-default-connection (name)
  (if (connection? name)
      (mongo-close (mongo-swap :default name))
      (mongo-close :default)))

#|
(defmacro with-mongo-connection ((var . args) &rest body)
  "Creates a connection to a mongodb, binds it to var and evaluates the body form.
  args is passed on to make-mongo when the connection is created.
  Caller should reference var in the body form where appropriate.."
  (let ((connection-name (gensym)))
    `(let ((,connection-name (gensym)))
       (unwind-protect
	    (multiple-value-prog1 
		(let ((,var (switch-default-connection ,connection-name ,@args)))
		  ,@body))
	 (restore-default-connection ,connection-name)))))
|#

(defmacro with-mongo-connection (args &rest body)
  "Creates a connection to a mongodb, makes it the default connection 
  and evaluates the body form.
  args uses the same keyword set as mongo (:db :localhost :port)
  args is passed on to make-mongo when the connection is created."
  (let ((connection-name (gensym)))
    `(let ((,connection-name (gensym)))
       (unwind-protect
	    (multiple-value-prog1 
		(progn (switch-default-connection ,connection-name ,@args)
		  ,@body))
	 (restore-default-connection ,connection-name)))))

#|
(with-mongo-connection (mongo :db "test")
		       (pp (iter (db.find "foo" :all :mongo mongo))))


(with-mongo-connection (mongo)
  (format t "-> mongo ~A~%" mongo))

(with-mongo-connection (mongo)
  (show :connections))

(with-mongo-connection (mongo :db "test")
  (pp (iter (db.find "foo" :all :mongo mongo) :mongo mongo)))
		       
|#
