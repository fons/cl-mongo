(in-package #:cl-user)

(defpackage #:cl-mongo-system (:use #:cl #:asdf))

(in-package #:cl-mongo-system)

(asdf:defsystem #:cl-mongo
  :name   "cl-mongo"
  :author "Fons Haffmans; fons.haffmans@gmail.com"
  :version "0.7"
  :licence "MIT"
  :description "lisp system to interact with mongodb, a non-sql db"
  :depends-on (:uuid
	       :babel
	       :bordeaux-threads
	       :documentation-template
	       :lisp-unit
	       :parenscript
	       :split-sequence
	       :usocket)
  :serial t
  :components 
  ((:module "src"
    :serial t
    :components ((:file "packages")
		 (:file "octets")
                 (:file "bson-oid")
                 (:file "document" :depends-on ("bson-oid"))
		 (:file "pair" :depends-on ("document"))
		 (:file "bson-array")
                 (:file "bson-encode-container" :depends-on ("bson-array"))
                 (:file "bson-binary")
                 (:file "bson-regex")
                 (:file "bson-decode" :depends-on ("bson-regex" "bson-binary"))
                 (:file "protocol" :depends-on ("octets" "bson-decode" "bson-array"))
                 (:file "encode-float")
		 (:file "bson-time")
		 (:file "bson-code")
                 (:file "bson" :depends-on ("octets" "bson-array" "bson-binary"))
		 (:file "mongo-syntax")
		 (:file "java-script")
		 (:file "mongo")
		 (:file "db" :depends-on ("pair" "mongo" "protocol" "bson-code" "bson-array"))
                 (:file "do-query" :depends-on ("mongo" "bson-decode" "bson-array"))
                 (:file "map-reduce" :depends-on ("bson-regex" "db" "mongo-syntax"))
                 ;(:file "map-collection" :depends-on ("db")) ???
                 (:file "doc")
		 (:file "shell" :depends-on ("bson-time" "db"))
                 #+sbcl(:file "mem")))
   (:static-file "README.md")
   (:static-file "COPYING")))

(asdf:defsystem #:cl-mongo-test
  :name   "cl-mongo"
  :author "Fons Haffmans; fons.haffmans@gmail.com"
  :version "0.7"
  :licence "MIT"
  :description "testing cl-mongo"
  :depends-on (:cl-mongo)
  :serial t
  :components 
  ((:module "test"
    :serial t
    :components ((:file "package")
		 (:file "test-utils")
		 (:file "regression")))))










