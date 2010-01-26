# cl-mongo


## Intro

[mongo is a scalable, high-performance, open source, schema-free, document-oriented database.](http://www.mongodb.org). 


This is a common lisp interface that allows you to work with a mongo document database.
This is not a driver (yet) as specified in the mongo documents; There is still some 
functionality I need to implement.


Various features are missing such as serialization of binary data and code, authentication, 
gridfs among other things.

In its current state cl-mongo provides the ability to insert, update and delete documents.
It also supports indexing.

I developed this using sbcl. I didn't rely on any sbcl extenxions so I expect this to run under 
other lisps as well.

## Version

   Version 0.1.1

## Installation

Use asdf to install cl-mongo. 

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


## Commands

### Creating and maintaining documents. 

`(defclass document()`

Document class. 
A document consists of key/value pairs stored in a hash table.
Each document has a unique id.   
Accessors : 
 
* elements  : returns the hash table.
* _id       : returns the unique id.
* _local_id : true id the document is locally generated.

`(defun make-document ( &key (oid nil) )`

Constructor.

* :oid key : User supplied unique id. 
 
`(defgeneric add-element ( key value document)`

Add an element with key and value to the document.

`(defgeneric get-element ( key document)`

Get an element with key from the document.

`(defgeneric rm-element (key document)`

Remove anelement with key from the document.

`(defun ht->document (ht)`

Convert a hash-table to a document.

`(defgeneric kv (a b &rest rest) )`

This is intended to be a helper function for key-value pairs and sets of key-value pairs.  

pair  -> (string value)  
value -> atom | cons  
kv    -> pair | list  
list  -> (elem list) | nil  


### Connection managment
`(defclass mongo ()`

Encapsulates the connection to the mongo database.
Each connection is a added to a global list of connections.

`(defgeneric db.use ( db &key )`

Use a database on the mongo server. Opens a connection if one isn't already 
established. (db.use -) can be used to go to a previosuly visited database, 
similar to cd -.

`(defun close-all-connections ()`

Close all connections in the global connection list.

`(defun cwd ( &key (mongo nil) )`

Show the current database.

`(defun nwd ()`

Show the database set by the `(db.use -)` command

### Helper Functions

`(defgeneric pp (result &key)`

Pretty-print the results returned from a query.

`(defun nd (result &key (stream t) )`

Pretty-print for non-document responses, like the response to a database command.

`(defun iter ( result &key (mongo nil) (max-per-call 0) )`

Exhaustively iterate through a query. The maximum number of responses 
per query can be specified using the max-per-call keyword.

`(defun rm (result &key (mongo nil) )`

Delete all the documents returned by a query. This is not an efficient 
way of deleting documents as it invloves multiple trips to the server.
Mongo allows for execution of java-script on the server side, but support for
this hasn't been added to cl-mongo *yet*.

`(defun docs ( result )`

Stop iteration and return the list of documents returned by the query.

`(defun now()`

Return the current date and time in bson format.

`(defgeneric db.insert ( collection document &key )`

Insert a document in the collection. The collection is specified by a string.

The document can be one of :

* document class as generated by `(make-document)`
* hash table
* a single kv (key-value pair) or a kv list.


`(defgeneric db.find (collection  kv &key)`

Find documents in the collection using the selector specified by kv.

Keywords :

* :limit : Max number of documents returned in this query


   :db.update
   :db.save

   :db.delete

   :db.next
   :db.iter
   :db.stop


   :db.ensure-index
   :db.indexes

   :db.run-command

   :db.collections
   :db.count

   :time-zone
   :date-time

   ;; shell commands




## What's missing
