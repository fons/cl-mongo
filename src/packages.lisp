(in-package #:cl-user)

(defpackage #:cl-mongo
  (:use #:common-lisp #:babel #:uuid #:usocket)
  (:export

   ;;keywords
   :all
   :listdatabases
   :serverstatus
   :deleteindexes

   ;;commands
   :mongo
   :kv
   :db.use
   :db.insert
   :db.find
   :db.next
   :db.iter
   :db.stop
   :db.delete
   :db.ensure-index
   :db.run-command
   :db.indexes
   :db.collections
   :db.count
   :close-all-connections
   
   ;; shell commands
   :nwd
   :cwd
   :pp
   :iter
   :nd
   :rm
   :docs
   ))
