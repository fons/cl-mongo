(in-package #:cl-user)

(defpackage #:cl-mongo
  (:use #:common-lisp #:babel #:uuid #:usocket )
  (:export

   ;;
   :*mongo-default-port*
   :*mongo-default-host*
   :*mongo-default-db*
   :document
   :make-document
   :add-element
   :get-element
   :collect-all-elements
   :rm-element
   :ht->document
   :mapdoc
   :doc-id
   :get-keys

   ;;commands
   :mongo
   :mongo-registered
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
   :db.distinct
   :time-zone
   :date-time
   :bson-time-to-ut

   ;; shell commands
   :nwd
   #+(or sbcl clisp allegro abcl)    :cwd
   :pp
   :ret
   :iter
   :nd
   :rm
   :docs
   :now
   :show

   ;; syntax expansion
   :$
   :$+
   :$-
   :$>
   :$>=
   :$<
   :$<=
   :$!=
   :$in
   :$!in
   :$mod
   :$all
   :$exists
   :$/
   :$not 
   :$em
   :$where
   :$index 

   :$inc
   :$set
   :$unset
   :$push
   :$push-all
   :$add-to-set
   :$pop-front
   :$pop-back
   :$pull
   :$pull-all
   :$where
   :$map-reduce
   :mr.p
   :mr.gc
   :mr.gc.all

   ;;javascript 
   :jsdef
   :defjs
   :defsrvjs
   :foreach
   :transform
   :install-js
   :remove-js

   ;; multi-threaded stuff 

   :do-query

   ;; documentation generator
   :generate-readme
   :*REPO-ROOT*
   ))
