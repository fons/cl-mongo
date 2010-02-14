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



## What's missing

At least the following is missing :  

* Request id/ Response id are left 0 in the header.
* Serialization of binary data.
* Serialization of regular expressions.
* Serialization of code scope.
* Advanced queries like min/max queries, group by, snapshot support.
* Aggregation except for distinct and group by.
* Authentication
* GridFS
* ......

## Commands



<body bgcolor=white>

<h2> CL-MONGO - cl-mongo</h2>

<blockquote>
<br>&nbsp;<br><h3><a name=abstract class=none>Abstract</a></h3>

The code comes with
a <a
href="http://www.opensource.org/licenses/bsd-license.php">MIT-style
license</a> so you can basically do with it whatever you want.

<p>
<font color=red>Download shortcut:</font> <a href="http://github.com/fons/cl-mongo">http://github.com/fons/cl-mongo</a>.
</blockquote>

<br>&nbsp;<br><h3><a class=none name="contents">Contents</a></h3>
<ol>
  <li><a href="#download">Download</a>
  <li><a href="#dictionary">The CL-MONGO dictionary</a>
    <ol>
      <li><a href="#add-element"><code>add-element</code></a>
      <li><a href="#close-all-connections"><code>close-all-connections</code></a>
      <li><a href="#cwd"><code>cwd</code></a>
      <li><a href="#date-time"><code>date-time</code></a>
      <li><a href="#db.collections"><code>db.collections</code></a>
      <li><a href="#db.collections"><code>db.collections</code></a>
      <li><a href="#db.count"><code>db.count</code></a>
      <li><a href="#db.delete"><code>db.delete</code></a>
      <li><a href="#db.ensure-index"><code>db.ensure-index</code></a>
      <li><a href="#db.eval"><code>db.eval</code></a>
      <li><a href="#db.find"><code>db.find</code></a>
      <li><a href="#db.indexes"><code>db.indexes</code></a>
      <li><a href="#db.indexes"><code>db.indexes</code></a>
      <li><a href="#db.insert"><code>db.insert</code></a>
      <li><a href="#db.iter"><code>db.iter</code></a>
      <li><a href="#db.next"><code>db.next</code></a>
      <li><a href="#db.run-command"><code>db.run-command</code></a>
      <li><a href="#db.save"><code>db.save</code></a>
      <li><a href="#db.stop"><code>db.stop</code></a>
      <li><a href="#db.update"><code>db.update</code></a>
      <li><a href="#db.use"><code>db.use</code></a>
      <li><a href="#docs"><code>docs</code></a>
      <li><a href="#document"><code>document</code></a>
      <li><a href="#gendoc"><code>gendoc</code></a>
      <li><a href="#get-element"><code>get-element</code></a>
      <li><a href="#ht->document"><code>ht->document</code></a>
      <li><a href="#iter"><code>iter</code></a>
      <li><a href="#kv"><code>kv</code></a>
      <li><a href="#make-document"><code>make-document</code></a>
      <li><a href="#mongo"><code>mongo</code></a>
      <li><a href="#mongo"><code>mongo</code></a>
      <li><a href="#nd"><code>nd</code></a>
      <li><a href="#now"><code>now</code></a>
      <li><a href="#nwd"><code>nwd</code></a>
      <li><a href="#pp"><code>pp</code></a>
      <li><a href="#rm"><code>rm</code></a>
      <li><a href="#rm-element"><code>rm-element</code></a>
      <li><a href="#time-zone"><code>time-zone</code></a>
    </ol>
  <li><a href="#ack">Acknowledgements</a>
</ol>

<br>&nbsp;<br><h3><a class=none name="download">Download</a></h3>

CL-MONGO together with this documentation can be downloaded from <a
href="http://github.com/fons/cl-mongo">http://github.com/fons/cl-mongo</a>. The
current version is 0.1.0.

<br>&nbsp;<br><h3><a class=none name="dictionary">The CL-MONGO dictionary</a></h3>





<p><br>[Generic function]<br><a class=none name='add-element'><b>add-element</b> <i>key value document</i> =&gt; <i>result</i></a>
<blockquote><br>

add element with key and value to a document

</blockquote>






<p><br>[Function]<br><a class=none name='close-all-connections'><b>close-all-connections</b> <i></i> =&gt; <i>result</i></a>
<blockquote><br>


Close all connections in the global connection list.


</blockquote>






<p><br>[Function]<br><a class=none name='cwd'><b>cwd</b> <i><tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Show the current database.


</blockquote>






<p><br>[Function]<br><a class=none name='date-time'><b>date-time</b> <i>second minute hour day month year <tt>&amp;optional</tt> time-zone</i> =&gt; <i>result</i></a>
<blockquote><br>

Generate a time stamp the mongo/bson protocol understands.

</blockquote>






<p><br>[Generic function]<br><a class=none name='db.collections'><b>db.collections</b> <i><tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Function]<br><a class=none name='db.collections'><b>db.collections</b> <i><tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Show all the collections in the current database.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.count'><b>db.count</b> <i>collection selector <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Count all the collections satifying the criterion set by the selector. 
&#039;all can be used to return a count of
all the documents in the collection.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.delete'><b>db.delete</b> <i>collection object <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Delete a document from a collection. The *document* field is used to identify the document to
be deleted.  
You can enter a list of documents. In that the server will be contacted to delete each one of these.
It may be more efficient to run a delete script on he server side.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.ensure-index'><b>db.ensure-index</b> <i>collection keys <tt>&amp;key</tt> unique mongo asc</i> =&gt; <i>result</i></a>
<blockquote><br>


Create an index specified by the keys in a collection


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.eval'><b>db.eval</b> <i>code <tt>&amp;rest</tt> rest</i> =&gt; <i>result</i></a>
<blockquote><br>

run javascript code server side

</blockquote>






<p><br>[Generic function]<br><a class=none name='db.find'><b>db.find</b> <i>collection kv <tt>&amp;key</tt> selector limit skip options mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Find documents in the collection using the selector specified by kv.  
Methods take two keywords. &#039;:limit&#039; sets the maximum number of documents returned. The default is 1.
&#039;:skip&#039; sets the number of documents to skip in this query. It&#039;s default is 0.
Since the default value of the limit is one, db.find by default is the equivalant of *findOne* in the
mongo documentation.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.indexes'><b>db.indexes</b> <i><tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Function]<br><a class=none name='db.indexes'><b>db.indexes</b> <i><tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Return all indexes in the database.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.insert'><b>db.insert</b> <i>collection document <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Insert a document in a collection. A document is typically generated by `(make-document)`, 
but it can also be a hash table,
a key-value pair or kv list (see the kv functions).  


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.iter'><b>db.iter</b> <i>result <tt>&amp;key</tt> limit mongo</i> =&gt; <i>result</i></a>
<blockquote><br>

next document iteration

</blockquote>






<p><br>[Generic function]<br><a class=none name='db.next'><b>db.next</b> <i>collection cursor-id <tt>&amp;key</tt> limit mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Executes the next call on the iterator identified by cursor-id.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.run-command'><b>db.run-command</b> <i>cmd <tt>&amp;key</tt> arg mongo index collection</i> =&gt; <i>result</i></a>
<blockquote><br>


Run a database command on the server. See the mongo documentation for a list of commands.  
For most commands you can just uses the key-value shown in the mongo documentation.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.save'><b>db.save</b> <i>collection document <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Save a document to the collection. If the document has a unique `_id` value (i.e. if it&#039;s generated
by `(make-document)` ) it will be &#039;upserted&#039; (that is: it will be inserted if the document
doesn&#039;t exist).  If the document a hash table or a kv set, it will be inserted.  
In other words this a a helper-function build around *db.insert* and *db.update*.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.stop'><b>db.stop</b> <i>cursor <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Stop iterating and clean up the iterator on the server by making a server call.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.update'><b>db.update</b> <i>collection selector new-document <tt>&amp;key</tt> mongo upsert multi</i> =&gt; <i>result</i></a>
<blockquote><br>

In a collection update the document(s) identified by the selector statement.  
This method has two keywords. &#039;:upsert&#039; : If t insert the document if the document cannot be 
found in the collection. &#039;:multi&#039;  : Update all documents identified by the selector.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.use'><b>db.use</b> <i>db <tt>&amp;key</tt> host port</i> =&gt; <i>result</i></a>
<blockquote><br>


Use a database on the mongo server. Opens a connection if one isn&#039;t already 
established. (db.use -) can be used to go to a previosuly visited database, 
similar to cd -. 

</blockquote>






<p><br>[Function]<br><a class=none name='docs'><b>docs</b> <i>result</i> =&gt; <i>result</i></a>
<blockquote><br>


Stop the iterator (if any) and return the list of documents returned by the query. 
Typical ue would be in conjunction with db.find like so (docs (iter (db.find &#039;foo&#039; &#039;ll)))


</blockquote>






<p><br>[Standard class]<br><a class=none name='document'><b>document</b></a>
<blockquote><br>

document
Document class. A document consists of key/value pairs stored in a internal hash table plus 
an internally generated unique id.   
Accessors are : &#039;elements&#039; which returns the internal hash table;
&#039;_id&#039; which  returns the unique id and &#039;_local_id&#039; which if true means that 
the document was generated by the client (as opposed to having been read from the server).

</blockquote>






<p><br>[Function]<br><a class=none name='gendoc'><b>gendoc</b> <i>target</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Generic function]<br><a class=none name='get-element'><b>get-element</b> <i>key document</i> =&gt; <i>result</i></a>
<blockquote><br>

Get an element identified by key from the document.

</blockquote>






<p><br>[Function]<br><a class=none name='ht->document'><b>ht->document</b> <i>ht</i> =&gt; <i>result</i></a>
<blockquote><br>

Convert a hash-table to a document.

</blockquote>






<p><br>[Function]<br><a class=none name='iter'><b>iter</b> <i>result <tt>&amp;key</tt> mongo max-per-call</i> =&gt; <i>result</i></a>
<blockquote><br>


Exhaustively iterate through a query. The maximum number of responses 
per query can be specified using the max-per-call keyword.



</blockquote>






<p><br>[Generic function]<br><a class=none name='kv'><b>kv</b> <i>a b <tt>&amp;rest</tt> rest</i> =&gt; <i>result</i></a>
<blockquote><br>


This a helper function for key-value pairs and sets of key-value pairs.  
In a key-value pair like (kv key value) the key has to be a string and the
value something which is serializable. 
key-value pairs can be combined using kv as well : (kv (kv key1 val1) (kv key2 val2)).
This combination of key-value pairs is equivalent to a document without a unique id.
The server will assign a unique is if a list of key-value pairs is saved.

</blockquote>






<p><br>[Function]<br><a class=none name='make-document'><b>make-document</b> <i><tt>&amp;key</tt> oid</i> =&gt; <i>result</i></a>
<blockquote><br>


Constructor.  key &#039;:oid&#039; is a user supplied unique id. An internal id will be generated if none 
is supplied.


</blockquote>






<p><br>[Standard class]<br><a class=none name='mongo'><b>mongo</b></a>
<blockquote><br>

 Encapsulates the connection to the mongo database.
Each connection is a added to a global list of connections.

</blockquote>






<p><br>[Generic function]<br><a class=none name='mongo'><b>mongo</b> <i><tt>&amp;key</tt> index host port</i> =&gt; <i>result</i></a>
<blockquote><br>

mongo connection

</blockquote>






<p><br>[Function]<br><a class=none name='nd'><b>nd</b> <i>result <tt>&amp;key</tt> stream</i> =&gt; <i>result</i></a>
<blockquote><br>


Pretty-print for non-document responses, like the response to a database command.


</blockquote>






<p><br>[Function]<br><a class=none name='now'><b>now</b> <i></i> =&gt; <i>result</i></a>
<blockquote><br>


Return the current date and time in bson format.  


</blockquote>






<p><br>[Function]<br><a class=none name='nwd'><b>nwd</b> <i></i> =&gt; <i>result</i></a>
<blockquote><br>


Show the database set by the `(db.use -)` command


</blockquote>






<p><br>[Generic function]<br><a class=none name='pp'><b>pp</b> <i>result <tt>&amp;key</tt> nd stream</i> =&gt; <i>result</i></a>
<blockquote><br>


Pretty-print the results returned from a query. To be used on the repl. 
This will format each server reply as if it were a document. This is obviously
ok in mosty cases. See nd for an alternative.


</blockquote>






<p><br>[Function]<br><a class=none name='rm'><b>rm</b> <i>result <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Delete all the documents returned by a query. This is not an efficient 
way of deleting documents as it invloves multiple trips to the server.
Mongo allows for execution of java-script on the server side, which provides
an alternative. Typical use would be (rm (iter (db.find &#039;foo&#039; (kv &#039;key&#039; 1)))),
which deletes all documents in foo, with field key equal to 1.


</blockquote>






<p><br>[Generic function]<br><a class=none name='rm-element'><b>rm-element</b> <i>key document</i> =&gt; <i>result</i></a>
<blockquote><br>

Remove element identified by key from a document

</blockquote>






<p><br>[Function]<br><a class=none name='time-zone'><b>time-zone</b> <i></i> =&gt; <i>result</i></a>
<blockquote><br>

Set the time zone appropriate for the current environment.

</blockquote>




<br>&nbsp;<br><h3><a class=none name="ack">Acknowledgements</a></h3>

<p>
This documentation was prepared with <a href="http://weitz.de/documentation-template/">DOCUMENTATION-TEMPLATE</a>.
</p>
<p>
$Header: /usr/local/cvsrep/documentation-template/output.lisp,v 1.14 2008/05/29 08:23:37 edi Exp $
<p><a href="http://www.mohegan-skunkworks.com/index.html">BACK TO MY HOMEPAGE</a>

</body>