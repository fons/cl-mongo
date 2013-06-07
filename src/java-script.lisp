(in-package :cl-mongo)

;;;
;;;  Enable javascript in the lisp using parenscript
;;;

(defparameter *js-definitions* (make-hash-table :test 'equalp) "hash table containing client side javascript")

(defmacro jsdef (name)
  "Return the body of the javascript function; otherwise nill."
  `(multiple-value-bind (value found) (gethash ',name *js-definitions*)
     (when found value)))

(defmacro jssrvdef (name)
  "Return the body of the javascript function as installed on the server; otherwise nill."
  `(db.find "system.js" (kv "_id" (string-downcase ',name))))

(defun js-show()
  (maphash (lambda (k v) (format t "~A:~A~%" k v)) *js-definitions*))

;;     (if found
;;	 value
;;	 ',name)))

(defmacro defjs (name args &body body)
  "Define client side javascript. Works like defun; body is in lisp, with parenscript
   additions, like return. So (defjs hello (x y) (return (+ x y))) defines an adder. 
   macro creates a lisp function which sends the javascript function over to the mongo 
   server to be evaluated. Result is processed and returned to the reader. 
   This will execute 10 times on the server :
   (mapcar (lambda (x) (hello 10 x)) (list 1 2 3 4 5 6 7 8 9 10))"
  `(let* ((js-body (parenscript:ps (lambda ,args ,@body))))
     (setf (gethash ',name *js-definitions*) js-body)
     (defun ,name ,args
       (get-element "retval" (car (docs (db.eval js-body ,@args)))))))

(defmacro defsrvjs (name args &body body)
  "Creates a function which stores and executes javascript on the server. The first time 
   the function is called the javascript function is stored on the server. Subsequent calls will
   call out to the server. 
   Works like defun; the function body is defined in lisp, with parenscript additions. Since
   the body of the function already resides on the server this should have less impact on 
   the network. Use :install t to reinstall."
  `(let* ((js-body (make-bson-code (parenscript:ps (lambda ,args ,@body))))
	  (server nil)
	  (func-name (string-downcase ',name))
	  (arg-list (format nil "(~{~a~^, ~})" ',args))
	  (jslambda (format nil "function ~A {return ~A~A;}" arg-list func-name arg-list)))
     (flet ((install (name)
	      (setf server (mongo))
	      (db.save "system.js" (kv (kv "_id" name) (kv "value" js-body)))))
       (setf (gethash ',name *js-definitions*) js-body)
       (defun ,name ,(append args '(&key (install nil)))
	 (progn
	     (when (or install (not server)) (install func-name))
	     (get-element "retval" (car (docs (db.eval jslambda ,@args)))))))))


(defmacro install-js (name)
  "Allows server based javascripts the be installed without being run."
  `(db.save "system.js" (kv (kv "_id" (string-downcase ',name)) (kv "value" (jsdef ,name)))))

;; this may not be on the server; this is just to make sure..
(defmacro remove-js (name)
  `(progn
     (remhash ',name *js-definitions*)
     (db.delete "system.js" (kv "_id" (string-downcase ',name)))))

(ps:defpsmacro for-each (collection fun)
  "for-each is a parenscript macro which will map fun over every item in the mongodb collection."
  `((slot-value ((slot-value ,collection 'find)) 'for-each) #',fun))

(ps:defpsmacro transform (collection fun)
  "for-each is a parenscript macro which will map fun over every item in the mongodb collection.
   It will then save the item."
  `((slot-value ((slot-value ,collection 'find)) 'for-each)
    (lambda (obj)
      (,fun obj)
      ((slot-value ,collection 'save) obj))))




