(in-package #:cl-user)

(defpackage #:cl-mongo
  (:use #:common-lisp #:babel #:uuid #:usocket )
  (:export

   ;;
   :*mongo-default-port*
   :*mongo-default-host*
   :*mongo-default-db*

   ;; document
   :document
   :make-document
   :add-element
   :get-element
   :rm-element
   :ht->document

   ;;commands
   :mongo
   :mongo-show
   :mongo-close
   :mongo-swap
   :with-mongo-connection
   :kv
   
   :db.create-collection
   :db.use
   :db.insert
   :db.update
   :db.save
   :db.find
   :db.sort
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
   :db.add-user
   :db.auth
   :time-zone
   :date-time

   ;; shell commands
   :nwd
   #+(or sbcl clisp allegro abcl)    :cwd
   :pp
   :iter
   :nd
   :rm
   :docs
   :now
   :show
   ;; syntax expansion
   :$+
   :$-

   ;; documentation generator
   :generate-readme
   :*REPO-ROOT*
   ))
