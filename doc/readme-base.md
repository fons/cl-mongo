# cl-mongo


## Intro

[mongo is a scalable, high-performance, open source, schema-free, document-oriented database.](http://www.mongodb.org). 


This is a common lisp interface that allows you to work with a mongo document database.
This is not a driver (yet) as specified in the mongo documents; There is still some 
functionality I need to implement.


Except for gridfs most features have been implemented.

cl-mongo provides the ability to insert, update
and delete documents, indexing, searching using regexs etc.
In addition it supports javascript in the repl (using a commonly
available lisp to javascript compiler). This allows you to do use
mongodb's map-reduce from the lisp repl.

I developed this primarily using *sbcl* and to some extend *clozure
common lisp* on the linux and mac osx platforms.

This should also work on  *clisp* and *allegro common lisp* and windows.

### multi-threading
The *do-query* function uses multithreading to process the results of
a batch as a query is in progress. This obviously requires that
multi-threading is supported. I've found that as of now (2010-10-23)
sbcl on mac osx doesn't support multithreading out of the box. So
you'd need to compile a new sbcl with multithreading enabled, which is
easy to do.

## Version

Version 0.9.0

I've added regression testing for all features currently supported by cl-mongo.
   
   
## Installation

Use asdf to install cl-mongo.  cl-mongo depends on many other lisp
libraries (which in turn have their own dependecies).  asdf is not a
dependency manager, so you would need to install all dependencies as
they come up as unavailable when you try to install cl-mongo.

On the other hand, cl-mongo is included in quicklisp, which should make life easier.

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

    (pp (iter (db.find "foo" :all)))`

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

      (defvar *DOC* (cadr (docs (db.find "foo" :all))))`
      (add-element "tags" (list 'good 'bad 'ugly) *DOC*)
      (db.save "foo" *DOC*)
      (pp (db.find "foo" :all))

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
      (nd (db.run-command :serverstatus))

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
* GridFS
* ......



## API documentation

I use Edi Weitz's [documentation template](http://weitz.de/documentation-template/)
to generate an api description based on embedded comments. 


## News

2010-10-23
I've improved the read performance several orders of magnitude. This
also reduces the memory foot pront quite a bit.

In addition I've added a multi-threaded reader called do-query.
