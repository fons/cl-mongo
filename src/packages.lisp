(in-package #:cl-user)

(defpackage #:cl-mongo
  (:use #:common-lisp #:babel #:uuid #:usocket)
  (:export

   :mongo
   :db.use
   :db.insert
   :db.find
   :close-all-connections

   ;; shell commands
   :nwd
   :cwd
   :pp
   :iter
   :nd
   ))
