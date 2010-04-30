(in-package #:cl-user)

(defpackage #:cl-mongo-system (:use #:cl #:asdf))

(in-package #:cl-mongo-system)

(asdf:defsystem cl-mongo
  :name   "cl-mongo"
  :author "Fons Haffmans; fons.haffmans@gmail.com"
  :version "0.0.1"
  :licence "MIT"
  :description "lisp system to interact with mongo, a non-sql db"
  :depends-on (:uuid
	       :babel
	       :documentation-template
	       :lisp-unit
	       :parenscript
	       :usocket)
  :serial t
  :components 
  ((:module "src"
    :serial t
    :components ((:file "packages")
		 (:file "octets")
		 (:file "pair")
		 (:file "encode-float")
		 (:file "bson-oid")
		 (:file "bson-binary")
		 (:file "bson-time")
		 (:file "bson-regex")
		 (:file "bson-code")
		 (:file "bson")
		 (:file "bson-array")
		 (:file "document")
		 (:file "mongo-syntax")
		 (:file "js.lisp")
		 (:file "bson-encode-container")
		 (:file "protocol")
		 (:file "mongo")
		 (:file "db")
		 (:file "doc") 
		 (:file "shell")))
   (:static-file "README.md")
   (:static-file "COPYING")))

(asdf:defsystem cl-mongo-test
  :name   "cl-mongo"
  :author "Fons Haffmans; fons.haffmans@gmail.com"
  :version "0.0.1"
  :licence "MIT"
  :description "tesing cl-mongo"
  :depends-on (:cl-mongo)
  :serial t
  :components 
  ((:module "test"
    :serial t
    :components ((:file "package")
		 (:file "test-utils")
		 (:file "regression")))))










