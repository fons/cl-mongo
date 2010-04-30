(in-package :cl-mongo)

#|
  mongo map reduce
|#

(defmacro $map-reduce (collection map reduce &key (query nil) (limit 0) (out nil) 
		       (keeptemp nil) (finalize nil) (verbose t) )
  "Run map reduce on the mongo server. map and reduce are either the names of the 
javascript functions, created with defjs or defsrvjs or are function definitions in javascript.
The keywords refer to option available for map reduce in mongo. This returns a result summary document."
    `(db.find "$cmd" (kv (kv "mapreduce" ,collection) 
			 (kv "map"    (or (jsdef ,map) ',map ))
			 (kv "reduce" (or (jsdef ,reduce) ',reduce))
			 (when ,finalize (kv "finalize" (jsdef ,finalize)))
			 (kv "out"      ,out) 
			 (kv "verbose"  ,verbose)
			 (kv "limit"    ,limit)
			 (kv "keeptemp" ,keeptemp)
			 (kv "query"    ,query))))
  
(defun mrp (results)
  (when results (db.find (get-element "result" (car (docs results ))) :all)))

;($map-reduce "foo" m r  :out "testit" :query ($> "l" -900) :limit 2)
;($map-reduce "foo" m r  :out "testit" :query ($> "l" -900) )
;(mr-results ($map-reduce "foo" m r :query ($> "l" -9)))
