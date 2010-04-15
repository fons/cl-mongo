# cl-mongo


## Intro

[mongo is a scalable, high-performance, open source, schema-free, document-oriented database.](http://www.mongodb.org). 


This is a common lisp interface that allows you to work with a mongo document database.
This is not a driver (yet) as specified in the mongo documents; There is still some 
functionality I need to implement.


Various features are missing such as serialization of binary data and code, 
gridfs among other things.

In its current state cl-mongo provides the ability to insert, update and delete documents.
It also supports indexing.

I developed this using *sbcl*, *clozure common lisp*, *clisp* and *allegro common lisp*.


## Version

   Version 0.5.0

   This version is basically an alpha release. 
   I've added regression testing for all features currently supported by cl-mongo.
   
   
## Installation

Use asdf to install cl-mongo. 

## Testing

The cl-mongo-test package  contains regression tests for all the features currently supported.
cl-mongo-test is asdf-installable, but does not export it's tests. 
To run the quick test, do this :
    (use-package :cl-mongo-test)
    (quick-test)

Quick test will connected to a locally running mongodb instance. It uses the "foo" collection 
in the "test" database.

## A sample session
This connects to the test database on the local mongo server listening on its default port.

    (use-package :cl-mongo)
    (db.use "test")

Insert a key-value pair as the first document into collection "foo".

`(db.insert "foo" (kv "document" "one") )`

Pretty-print the documents returned by the find command. iter will ensure that the cursor is
fully iterated over.

    (pp (iter (db.find "foo" 'all)))`

     {
        "_id" -> objectid(4B5CF28970DFF196A75FE1F0)
        "document"  ->  one
     }

Create a document. A document is collection of key-value pairs and a unique identifier called "_id".

`(defvar *DOC* (make-document))`

Add various elements to the document.

       (add-element "tag" "key" *DOC*)`


       {  DOCUMENT 

       {
       tag  -> key 
       }
       }


    (add-element "array" (list 1 2 3 "hello") *DOC*)

    {  DOCUMENT 

    {
    tag  -> key 
    	 1 
    	 2 
    	 3 
    	 hello 
  	 array  -> NIL 
	 }
     }


Insert document into the database.

`(db.insert "foo" *DOC*)`

Print the current contents.

       (pp (iter (db.find "foo" 'all)))

       {
       "_id" -> objectid(4B5CF28970DFF196A75FE1F0)
       "document"  ->  one
       }

       {
       "_id" -> objectid(8B508D5CBB5D451D961F046D)
       "array"  -> [ 1, 2, 3, hello,]
       "tag"  ->  key
       }


Bind variable `*DOC*` to the second document returned by the find command, 
add an other element and save back to the collection.

      (defvar *DOC* (cadr (docs (db.find "foo" 'all))))`
      (add-element "tags" (list 'good 'bad 'ugly) *DOC*)
      (db.save "foo" *DOC*)
      (pp (db.find "foo" 'all))

      {
	"_id" -> objectid(4B5CF28970DFF196A75FE1F0)
      	"document"  ->  one
      }

      {
        "_id" -> objectid(8B508D5CBB5D451D961F046D)
      	"tags"  -> [ GOOD, BAD, UGLY,]
     	"tag"  ->  key
     	"array"  -> [ 1, 2, 3, hello,]
      }

Check the status of the server.

      (db.use "admin")
      (nd (db.run-command 'serverstatus))

        "ok"  ->  1.0d0
	  "mem"  -> 
	    "mapped"  ->  80
	    "virtual"  ->  177
            "resident"  ->  3
        "globalLock"  -> 
          "ratio"  ->  4.644753171959394d-5
          "lockTime"  ->  6493354.0d0
        "totalTime"  ->  1.39799764586d11
        "uptime"  ->  139799.0d0

## $ syntax

I've added various $.. macros which allow for a more declarative programming style. In stead of
doing something like :

    (db.find "foo" (kv "k" (kv "$lte" 3)))

you can use :

    (db.find "foo" ($<= "k" 3))

To declare a unique ascending index on "k" in collection "foo" , you can say :

    ($index "foo" :unique :asc "k")


## What's missing

At least the following is missing :  

* Request id/ Response id are left 0 in the header.
* Serialization of binary data.
* Serialization of code scope.
* Advanced queries like min/max queries, group by, snapshot support.
* Aggregation except for distinct and group by.
* GridFS
* ......

## API documentation

I use Edi Weitz's [documentation template](http://weitz.de/documentation-template/)
to generate an api description based on embedded comments. 



