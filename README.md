# cl-mongo

[![Join the chat at https://gitter.im/fons/cl-mongo](https://badges.gitter.im/fons/cl-mongo.svg)](https://gitter.im/fons/cl-mongo?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


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
<body bgcolor=white>

<h2> CL-MONGO - api reference</h2>

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
      <li><a href="#$"><code>$</code></a>
      <li><a href="#$!="><code>$!=</code></a>
      <li><a href="#$!in"><code>$!in</code></a>
      <li><a href="#$+"><code>$+</code></a>
      <li><a href="#$-"><code>$-</code></a>
      <li><a href="#$/"><code>$/</code></a>
      <li><a href="#$<"><code>$<</code></a>
      <li><a href="#$<="><code>$<=</code></a>
      <li><a href="#$>"><code>$></code></a>
      <li><a href="#$>="><code>$>=</code></a>
      <li><a href="#$add-to-set"><code>$add-to-set</code></a>
      <li><a href="#$all"><code>$all</code></a>
      <li><a href="#$em"><code>$em</code></a>
      <li><a href="#$exists"><code>$exists</code></a>
      <li><a href="#$in"><code>$in</code></a>
      <li><a href="#$inc"><code>$inc</code></a>
      <li><a href="#$index"><code>$index</code></a>
      <li><a href="#$map-reduce"><code>$map-reduce</code></a>
      <li><a href="#$mod"><code>$mod</code></a>
      <li><a href="#$not"><code>$not</code></a>
      <li><a href="#$pop-back"><code>$pop-back</code></a>
      <li><a href="#$pop-front"><code>$pop-front</code></a>
      <li><a href="#$pull"><code>$pull</code></a>
      <li><a href="#$pull-all"><code>$pull-all</code></a>
      <li><a href="#$push"><code>$push</code></a>
      <li><a href="#$push-all"><code>$push-all</code></a>
      <li><a href="#$set"><code>$set</code></a>
      <li><a href="#$unset"><code>$unset</code></a>
      <li><a href="#$where"><code>$where</code></a>
      <li><a href="#*mongo-default-db*"><code>*mongo-default-db*</code></a>
      <li><a href="#*mongo-default-host*"><code>*mongo-default-host*</code></a>
      <li><a href="#*mongo-default-port*"><code>*mongo-default-port*</code></a>
      <li><a href="#*repo-root*"><code>*repo-root*</code></a>
      <li><a href="#add-element"><code>add-element</code></a>
      <li><a href="#cwd"><code>cwd</code></a>
      <li><a href="#date-time"><code>date-time</code></a>
      <li><a href="#db.add-user"><code>db.add-user</code></a>
      <li><a href="#db.auth"><code>db.auth</code></a>
      <li><a href="#db.collections"><code>db.collections</code></a>
      <li><a href="#db.collections"><code>db.collections</code></a>
      <li><a href="#db.count"><code>db.count</code></a>
      <li><a href="#db.create-collection"><code>db.create-collection</code></a>
      <li><a href="#db.delete"><code>db.delete</code></a>
      <li><a href="#db.distinct"><code>db.distinct</code></a>
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
      <li><a href="#db.sort"><code>db.sort</code></a>
      <li><a href="#db.stop"><code>db.stop</code></a>
      <li><a href="#db.update"><code>db.update</code></a>
      <li><a href="#db.use"><code>db.use</code></a>
      <li><a href="#defjs"><code>defjs</code></a>
      <li><a href="#defsrvjs"><code>defsrvjs</code></a>
      <li><a href="#do-query"><code>do-query</code></a>
      <li><a href="#doc-id"><code>doc-id</code></a>
      <li><a href="#docs"><code>docs</code></a>
      <li><a href="#document"><code>document</code></a>
      <li><a href="#generate-readme"><code>generate-readme</code></a>
      <li><a href="#get-element"><code>get-element</code></a>
      <li><a href="#ht->document"><code>ht->document</code></a>
      <li><a href="#install-js"><code>install-js</code></a>
      <li><a href="#iter"><code>iter</code></a>
      <li><a href="#jsdef"><code>jsdef</code></a>
      <li><a href="#kv"><code>kv</code></a>
      <li><a href="#make-document"><code>make-document</code></a>
      <li><a href="#mapdoc"><code>mapdoc</code></a>
      <li><a href="#mongo"><code>mongo</code></a>
      <li><a href="#mongo"><code>mongo</code></a>
      <li><a href="#mongo-close"><code>mongo-close</code></a>
      <li><a href="#mongo-registered"><code>mongo-registered</code></a>
      <li><a href="#mongo-show"><code>mongo-show</code></a>
      <li><a href="#mongo-swap"><code>mongo-swap</code></a>
      <li><a href="#mr.gc"><code>mr.gc</code></a>
      <li><a href="#mr.gc.all"><code>mr.gc.all</code></a>
      <li><a href="#mr.p"><code>mr.p</code></a>
      <li><a href="#nd"><code>nd</code></a>
      <li><a href="#now"><code>now</code></a>
      <li><a href="#nwd"><code>nwd</code></a>
      <li><a href="#pp"><code>pp</code></a>
      <li><a href="#remove-js"><code>remove-js</code></a>
      <li><a href="#ret"><code>ret</code></a>
      <li><a href="#rm"><code>rm</code></a>
      <li><a href="#rm-element"><code>rm-element</code></a>
      <li><a href="#show"><code>show</code></a>
      <li><a href="#time-zone"><code>time-zone</code></a>
      <li><a href="#with-mongo-connection"><code>with-mongo-connection</code></a>
    </ol>
  <li><a href="#ack">Acknowledgements</a>
</ol>

<br>&nbsp;<br><h3><a class=none name="download">Download</a></h3>

CL-MONGO together with this documentation can be downloaded from <a
href="http://github.com/fons/cl-mongo">http://github.com/fons/cl-mongo</a>. The
current version is 0.1.0.

<br>&nbsp;<br><h3><a class=none name="dictionary">The CL-MONGO dictionary</a></h3>





<p><br>[Macro]<br><a class=none name='$'><b>$</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$!='><b>$!=</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$!in'><b>$!in</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$+'><b>$+</b> <i>arg <tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$-'><b>$-</b> <i>arg <tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$/'><b>$/</b> <i>regex options</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$<'><b>$<</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$<='><b>$<=</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$>'><b>$></b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$>='><b>$>=</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$add-to-set'><b>$add-to-set</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$all'><b>$all</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$em'><b>$em</b> <i>array <tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$exists'><b>$exists</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$in'><b>$in</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$inc'><b>$inc</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$index'><b>$index</b> <i>collection <tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$map-reduce'><b>$map-reduce</b> <i>collection map reduce <tt>&amp;key</tt> query limit out keeptemp finalize verbose</i> =&gt; <i>result</i></a>
<blockquote><br>

Run map reduce on the mongo server. map and reduce are either the names of the 
javascript functions, created with defjs or defsrvjs or are function definitions in javascript.
The keywords refer to option available for map reduce in mongo. This returns a result summary document.
When using :keeptemp t without specificing :out the collection is mr.&lt;collection&gt; 

</blockquote>






<p><br>[Macro]<br><a class=none name='$mod'><b>$mod</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$not'><b>$not</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$pop-back'><b>$pop-back</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$pop-front'><b>$pop-front</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$pull'><b>$pull</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$pull-all'><b>$pull-all</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$push'><b>$push</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$push-all'><b>$push-all</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$set'><b>$set</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$unset'><b>$unset</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Macro]<br><a class=none name='$where'><b>$where</b> <i><tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Special variable]<br><a class=none name='*mongo-default-db*'><b>*mongo-default-db*</b></a>
<blockquote><br>

database opened by the default connection

</blockquote>






<p><br>[Special variable]<br><a class=none name='*mongo-default-host*'><b>*mongo-default-host*</b></a>
<blockquote><br>

host for the default connection.

</blockquote>






<p><br>[Special variable]<br><a class=none name='*mongo-default-port*'><b>*mongo-default-port*</b></a>
<blockquote><br>

port for the default connection.

</blockquote>






<p><br>[Special variable]<br><a class=none name='*repo-root*'><b>*repo-root*</b></a>
<blockquote><br>

root of the repository; used for documentation generation

</blockquote>






<p><br>[Generic function]<br><a class=none name='add-element'><b>add-element</b> <i>key value document</i> =&gt; <i>result</i></a>
<blockquote><br>

add element with key and value to a document

</blockquote>






<p><br>[Function]<br><a class=none name='cwd'><b>cwd</b> <i><tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>

Show the current database.

</blockquote>






<p><br>[Function]<br><a class=none name='date-time'><b>date-time</b> <i>second minute hour day month year <tt>&amp;optional</tt> time-zone</i> =&gt; <i>result</i></a>
<blockquote><br>

Generate a time stamp the mongo/bson protocol understands.

</blockquote>






<p><br>[Generic function]<br><a class=none name='db.add-user'><b>db.add-user</b> <i>username password <tt>&amp;key</tt> mongo readonly</i> =&gt; <i>result</i></a>
<blockquote><br>

 Add a user to the database. 

</blockquote>






<p><br>[Generic function]<br><a class=none name='db.auth'><b>db.auth</b> <i>username password <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>

authenticate a user with a password

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
:all can be used to return a count of
all the documents in the collection.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.create-collection'><b>db.create-collection</b> <i>collection <tt>&amp;key</tt></i> =&gt; <i>result</i></a>
<blockquote><br>

create a collection

</blockquote>






<p><br>[Generic function]<br><a class=none name='db.delete'><b>db.delete</b> <i>collection object <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Delete a document from a collection. The *document* field is used to identify the document to
be deleted.  
You can enter a list of documents. In that the server will be contacted to delete each one of these.
It may be more efficient to run a delete script on the server side.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.distinct'><b>db.distinct</b> <i>collection key <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>

Return all the distinct values of this key in the collection 

</blockquote>






<p><br>[Generic function]<br><a class=none name='db.ensure-index'><b>db.ensure-index</b> <i>collection keys <tt>&amp;key</tt> drop-duplicates unique mongo asc</i> =&gt; <i>result</i></a>
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
but it can also be a hash table, a key-value pair or kv list (see the kv functions).  

</blockquote>






<p><br>[Generic function]<br><a class=none name='db.iter'><b>db.iter</b> <i>result <tt>&amp;key</tt> limit mongo</i> =&gt; <i>result</i></a>
<blockquote><br>

next document iteration

</blockquote>






<p><br>[Generic function]<br><a class=none name='db.next'><b>db.next</b> <i>collection cursor-id <tt>&amp;key</tt> limit mongo</i> =&gt; <i>result</i></a>
<blockquote><br>


Executes the next call on the iterator identified by cursor-id.


</blockquote>






<p><br>[Generic function]<br><a class=none name='db.run-command'><b>db.run-command</b> <i>cmd <tt>&amp;key</tt> arg mongo collection index</i> =&gt; <i>result</i></a>
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






<p><br>[Macro]<br><a class=none name='db.sort'><b>db.sort</b> <i>collection query <tt>&amp;rest</tt> args</i> =&gt; <i>result</i></a>
<blockquote><br>

sort macro : Takes the same arguments and keywords as db.find but converts the query 
   so it works as a sort. use the :field keyword to select the field to sort on.
   Set :asc to nil to reverse the sort order

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






<p><br>[Generic function]<br><a class=none name='db.use'><b>db.use</b> <i>db <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>

Use a database on the mongo server. Opens a connection if one isn&#039;t already 
established. (db.use -) can be used to go to a previosuly visited database, 
similar to cd -. 

</blockquote>






<p><br>[Macro]<br><a class=none name='defjs'><b>defjs</b> <i>name args declaration* statement*</i> =&gt; <i>result</i></a>
<blockquote><br>

Define client side javascript. Works like defun; body is in lisp, with parenscript
   additions, like return. So (defjs hello (x y) (return (+ x y))) defines an adder. 
   macro creates a lisp function which sends the javascript function over to the mongo 
   server to be evaluated. Result is processed and returned to the reader. 
   This will execute 10 times on the server :
   (mapcar (lambda (x) (hello 10 x)) (list 1 2 3 4 5 6 7 8 9 10))

</blockquote>






<p><br>[Macro]<br><a class=none name='defsrvjs'><b>defsrvjs</b> <i>name args declaration* statement*</i> =&gt; <i>result</i></a>
<blockquote><br>

Creates a function which stores and executes javascript on the server. The first time 
   the function is called the javascript function is stored on the server. Subsequent calls will
   call out to the server. 
   Works like defun; the function body is defined in lisp, with parenscript additions. Since
   the body of the function already resides on the server this should have less impact on 
   the network. Use :install t to reinstall.

</blockquote>






<p><br>[Function]<br><a class=none name='do-query'><b>do-query</b> <i>coll <tt>&amp;key</tt> map-fn reduce-fn initial-value query mongo limit selector</i> =&gt; <i>result</i></a>
<blockquote><br>

 Performs a multi-threaded query on a mongo database.  coll is the collection name.  The reduce-fn keyword is used to specify a function which 
will be called for each member of a batch of data returned from mongodb.  
The reduce-fn function is executed while the query for the next batch is in progress. The default for reduce-fn is the identity function. 
The reduction keyword is used to specify a function which is executed when the database queries have finished. 
It&#039;s default implementation is to return a hash table, with the mongodb id as the hash key.  The query, mongo, limit and selector keywords are used in the same way as for db.find. 


</blockquote>






<p><br>[Function]<br><a class=none name='doc-id'><b>doc-id</b> <i>doc</i> =&gt; <i>result</i></a>
<blockquote><br>

return the unique document id

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






<p><br>[Function]<br><a class=none name='generate-readme'><b>generate-readme</b> <i><tt>&amp;key</tt> path</i> =&gt; <i>result</i></a>
<blockquote><br>

 This function generates a README.md file with the latest api description.
The :path keyword specifies the location. It expects a sub-directory &lt;path&gt;/doc. 
Api documentation is generated on the fly, by extracting comments from the classes, 
generics and fuctions exported in the packages file. 
The resulting file is &lt;path&gt;/doc/index.html. &lt;path&gt;/README.md is generated by  
appending the api documentation to &lt;path&gt;/doc/readme-base.md.
:path or *REPO-ROOT* are typically set to the root of the repository.


</blockquote>






<p><br>[Generic function]<br><a class=none name='get-element'><b>get-element</b> <i>key document</i> =&gt; <i>result</i></a>
<blockquote><br>

Get an element identified by key from the document.

</blockquote>






<p><br>[Function]<br><a class=none name='ht->document'><b>ht->document</b> <i>ht</i> =&gt; <i>result</i></a>
<blockquote><br>

Convert a hash-table to a document.

</blockquote>






<p><br>[Macro]<br><a class=none name='install-js'><b>install-js</b> <i>name</i> =&gt; <i>result</i></a>
<blockquote><br>

Allows server based javascripts the be installed without being run.

</blockquote>






<p><br>[Function]<br><a class=none name='iter'><b>iter</b> <i>result <tt>&amp;key</tt> mongo max-per-call</i> =&gt; <i>result</i></a>
<blockquote><br>

Exhaustively iterate through a query. The maximum number of responses 
   per query can be specified using the max-per-call keyword.

</blockquote>






<p><br>[Macro]<br><a class=none name='jsdef'><b>jsdef</b> <i>name</i> =&gt; <i>result</i></a>
<blockquote><br>

Return the body of the javascript function; otherwise nill.

</blockquote>






<p><br>[Generic function]<br><a class=none name='kv'><b>kv</b> <i>a <tt>&amp;rest</tt> rest</i> =&gt; <i>result</i></a>
<blockquote><br>

 This a helper function for key-value pairs and sets of key-value pairs.  
In a key-value pair like (kv key value) the key has to be a string and the
value something which is serializable. 
key-value pairs can be combined using kv as well : (kv (kv key1 val1) (kv key2 val2)).
This combination of key-value pairs is equivalent to a document without a unique id.
The server will assign a unique is if a list of key-value pairs is saved.

</blockquote>






<p><br>[Function]<br><a class=none name='make-document'><b>make-document</b> <i><tt>&amp;key</tt> oid size</i> =&gt; <i>result</i></a>
<blockquote><br>

Constructor.  key &#039;:oid&#039; is a user supplied unique id. An internal id will be generated if none 
   is supplied.

</blockquote>






<p><br>[Function]<br><a class=none name='mapdoc'><b>mapdoc</b> <i>fn document</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Standard class]<br><a class=none name='mongo'><b>mongo</b></a>
<blockquote><br>

 Encapsulates the connection to the mongo database.
Each connection is a added to a global registry.

</blockquote>






<p><br>[Generic function]<br><a class=none name='mongo'><b>mongo</b> <i><tt>&amp;key</tt> host port db name host port db name</i> =&gt; <i>result</i></a>
<blockquote><br>

 This method returns the connection referred to by 
the name identifier from the connection registry. The connection name is unique. 
If no connection with that name exists, a new connection with the supplied or default 
host, port and db parameters will be created. The default host is localhost; 
the default port is  27017; the default db is admin.

</blockquote>






<p><br>[Generic function]<br><a class=none name='mongo-close'><b>mongo-close</b> <i>name</i> =&gt; <i>result</i></a>
<blockquote><br>

Close the connection to the mongo database. 
The name should uniquely identify the connection to close.
This is either a mongo object or the name the object is bound to 
in the connection registry. To close all open connections use the special symbol &#039;all

</blockquote>






<p><br>[Generic function]<br><a class=none name='mongo-registered'><b>mongo-registered</b> <i>name</i> =&gt; <i>result</i></a>
<blockquote><br>

Return a conection registered by this name or nil..

</blockquote>






<p><br>[Function]<br><a class=none name='mongo-show'><b>mongo-show</b> <i></i> =&gt; <i>result</i></a>
<blockquote><br>

 Show all registered connections and their session id

</blockquote>






<p><br>[Generic function]<br><a class=none name='mongo-swap'><b>mongo-swap</b> <i>left right</i> =&gt; <i>result</i></a>
<blockquote><br>

Swap the names of the left and right connections. Typical use would be 
`(mongo-swap :default :alt)`. After the function call :default will refer to the connection
previously referred to as :alt. A connection named :default is returned by `(mongo)` and is the default used in the api. The connections are returned in the order they were passed in (but with the names
swapped between them). To re-open a connection you can say 
`(mongo-close (mongo-swap :default (mongo :host &lt;newhost&gt; :portid &lt;portid&gt; :name :temp)))` 
and a new default connection is registered.

</blockquote>






<p><br>[Function]<br><a class=none name='mr.gc'><b>mr.gc</b> <i><tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>

remove the temporary collections created by map-reduce

</blockquote>






<p><br>[Function]<br><a class=none name='mr.gc.all'><b>mr.gc.all</b> <i><tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
<blockquote><br>

remove the all collections created by map-reduce, temporary as well as permanent

</blockquote>






<p><br>[Function]<br><a class=none name='mr.p'><b>mr.p</b> <i>results</i> =&gt; <i>result</i></a>
<blockquote><br>

show the contents of the results collection map-reduce created

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






<p><br>[Macro]<br><a class=none name='remove-js'><b>remove-js</b> <i>name</i> =&gt; <i>result</i></a>
<blockquote><br>



</blockquote>






<p><br>[Function]<br><a class=none name='ret'><b>ret</b> <i>result</i> =&gt; <i>result</i></a>
<blockquote><br>

return value bound to retval in a return document. Used for db.count, db.distinct, functions etc..

</blockquote>






<p><br>[Function]<br><a class=none name='rm'><b>rm</b> <i>collection query <tt>&amp;key</tt> mongo</i> =&gt; <i>result</i></a>
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






<p><br>[Generic function]<br><a class=none name='show'><b>show</b> <i>things <tt>&amp;key</tt> msg nl order</i> =&gt; <i>result</i></a>
<blockquote><br>

Print a list of things. Things can be users, databases, 
collections in the current database,
the profile and more. Things is a keyword so (show :users) will show all users.

</blockquote>






<p><br>[Function]<br><a class=none name='time-zone'><b>time-zone</b> <i></i> =&gt; <i>result</i></a>
<blockquote><br>

Set the time zone appropriate for the current environment.

</blockquote>






<p><br>[Macro]<br><a class=none name='with-mongo-connection'><b>with-mongo-connection</b> <i>args <tt>&amp;rest</tt> body</i> =&gt; <i>result</i></a>
<blockquote><br>

Creates a connection to a mongodb, makes it the default connection 
  and evaluates the body form.
  args uses the same keyword set as mongo (:db :localhost :port)
  args is passed on to make-mongo when the connection is created.

</blockquote>




<br>&nbsp;<br><h3><a class=none name="ack">Acknowledgements</a></h3>

<p>
This documentation was prepared with <a href="http://weitz.de/documentation-template/">DOCUMENTATION-TEMPLATE</a>.
</p>
<p>
$Header: /usr/local/cvsrep/documentation-template/output.lisp,v 1.14 2008/05/29 08:23:37 edi Exp $
<p><a href="http://www.mohegan-skunkworks.com/index.html">BACK TO MY HOMEPAGE</a>

</body>