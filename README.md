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

I developed this using *sbcl*. I didn't rely on any sbcl extenxions so I expect this to run under 
other lisps as well.

## Version

   Version 0.1.1

   This version is basically an alpha release. Many features are and testing has not been
   tested extensively.

   
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

`(defmethod db.collections (&key (mongo nil) )`  

Show all the collections in the current database.

`(defgeneric db.count ( collection selector &key )`  

Count the number of documents satifying the selector. 'all can be used to return a count of
all the documents in the collection.

### CRUD Operations

`(defgeneric db.insert ( collection document &key )`

Insert a document in the collection. The collection is specified by a string.  
The document can be one of :

* document class as generated by `(make-document)`
* hash table
* a single kv (key-value pair) or a kv list.


`(defgeneric db.find (collection  kv &key)`

Find documents in the collection using the selector specified by kv.  
Keywords :

* :limit : Max number of documents returned in this query. The default is 1.
* :skip  : Number of documents to skip in this query.

The default value of the limit is one, so db.find by default is the equivalant of *findOne* in the
mongo documentation.

`(defgeneric db.update ( collection selector new-document &key )`

In a collection update the document(s) identified by the selector statement.  
This method has the following keywords :

* :upsert : If t insert the document if the document cannot be found in the collection.
* :multi  : Update all documents identified by the selector.

`(defgeneric db.save ( collection document &key)`

Save a document to the collection. If the document has a unique "_id" value (i.e. if it's generated
by `(make-document)` ) it will be *upserted*.  
If it's a hash table or a kv set, it will be inserted.  
In other words this a a helper-function build around *db.insert* and *db.update*.

`(defgeneric db.delete ( collection object &key )`

Delete a document from a collection. The *document* field is used to identify the document to
be deleted.  
You can enter a list of documents. In that the server will be contacted to delete each one of these.
It may be more efficient to run a delete script on he server side.

`(defun date-time (second minute hour day month year &optional (time-zone *bt-time-zone*) )`  

Generate a time stamp the mongo/bson protocol understands.

`(defun time-zone ()`  
Set the time zone appropriate for the current environment.

### Iteration Support

`(defun db.iterator ( result )`  
Returns the iterator from the result set.

`(defgeneric db.next ( collection cursor-id &key )`  
Executes the next call on the iterator identified by cursor-id.

`(defgeneric db.stop ( cursor &key mongo )`  
Stop iterating and clean up the iterator on the server by making a server call.

### Index Support

`(defgeneric db.ensure-index (collection keys &key)`  

Create an index for a collection.

`(defmethod db.indexes (&key (mongo nil) )`  

Return all indexes in the database.

### Database Commands

`(defgeneric db.run-command ( cmd &key )`  

Run a database command on the server. See the mongo documentation for a list of commands.  
For most commands you can just uses the key-value shown in the mongo documentation.

## What's missing

At least the following is missing :  

* Request id/ Response id are left 0 in the header.
* Serialization of binary data.
* Serialization of regular expressions.
* Serialization of code with and without scope (i.e support for server-side code execution) .
* Advanced queries like min/max queries, group by, snapshot support.
* Aggregation except for distinct and group by.
* Authentication
* GridFS
* ......

