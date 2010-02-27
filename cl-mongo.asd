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
	       :usocket)
  :serial t
  :components 
  ((:module "src"
    :serial t
    :components ((:file "packages")
		 (:file "octets")
		 (:file "encode-float")
		 (:file "bson-oid")
		 (:file "bson-binary")
		 (:file "bson-time")
		 (:file "bson-code")
		 (:file "pair")
		 (:file "bson")
		 (:file "bson-array")
		 (:file "document")
		 (:file "protocol")
		 (:file "mongo")
		 (:file "db")
		 (:file "doc") 
		 (:file "shell")))
   (:static-file "README.md")
   (:static-file "COPYING")))









