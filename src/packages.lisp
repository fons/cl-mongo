(in-package #:cl-user)

(defpackage #:cl-mongo
  (:use #:common-lisp #:babel #:uuid #:usocket)
  (:export

   ;;keywords
   :all
   :listdatabases
   :serverstatus
   :deleteindexes

   ;; document
   :document
   :make-document
   :add-element
   :get-element
   :rm-element
   :ht->document

   ;;commands
   :mongo
   :kv
   :db.use
   :db.insert
   :db.update
   :db.save
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
   :db.eval
   :close-all-connections
   :time-zone
   :date-time

   ;; shell commands
   :nwd
   :cwd
   :pp
   :iter
   :nd
   :rm
   :docs
   :now

   ;; documentation generator
   :generate-readme
   :*REPO-ROOT*
   ))
