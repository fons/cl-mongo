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

## Installation

Use asdf to install cl-mongo. 

## A sample session

`(use-package :cl-mongo)`

`(db.use "test")`

This connects to the test database on the local mongo server listening on its default port.

## Commands

## What's missing
