(in-package :cl-mongo)

#|
  mongo map reduce
|#

(defmacro $map-reduce (collection map reduce &key (query nil) (limit 0) (out nil) 
		       (keeptemp nil) (finalize nil) (verbose t))
  "Run map reduce on the mongo server. map and reduce are either the names of the 
javascript functions, created with defjs or defsrvjs or are function definitions in javascript.
The keywords refer to option available for map reduce in mongo. This returns a result summary document.
When using :keeptemp t without specificing :out the collection is mr.<collection> "
    `(db.find "$cmd" (kv (kv "mapreduce" ,collection) 
			 (kv "map"    (or (jsdef ,map) ',map))
			 (kv "reduce" (or (jsdef ,reduce) ',reduce))
			 (when ,finalize (kv "finalize" (jsdef ,finalize)))
			 (kv "out"      (if ,out ,out (concatenate 'string "mr." ,collection))) 
			 (kv "verbose"  ,verbose)
			 (kv "limit"    ,limit)
			 (kv "keeptemp" ,keeptemp)
			 (kv "query"    ,query))))
  
(defun mr.p (results)
  "show the contents of the results collection map-reduce created" 
  (when results (db.find (get-element "result" (car (docs results))) :all)))



;;

(defun mr.collections(&key (mongo (mongo)))
  (mapcar (lambda (d) (get-element "name" d)) 
	  (docs (db.find "system.namespaces" ($ "name" ($/ "[_]*.mr." "i")) :limit 0 :mongo mongo))))

(defun mr.gc* (query &key (mongo (mongo)))
  (let ((mr-ns (mapcar (lambda (d) (get-element "name" d)) 
		       (docs (db.find "system.namespaces" 
				      (kv "name" ($/ query "i")) 
				      :limit 0 :mongo mongo)))))
    (labels ((tmps()
	       (let ((junk ()))
		 (dolist (el mr-ns)
		   (unless (search ".$_id_" el :from-end t) 
		     (push el junk)))
		 junk))
	     (coll (name)
	       (subseq name (+ 1 (search "." name)))))
      (dolist (el (tmps))
	(db.run-command :drop :collection (coll el) :mongo mongo)))))

(defun mr.gc (&key (mongo (mongo)))
  "remove the temporary collections created by map-reduce"
  (mr.gc* "[_]*.tmp.mr." :mongo mongo))

(defun mr.gc.all (&key (mongo (mongo)))
  "remove the all collections created by map-reduce, temporary as well as permanent"
  (mr.gc* "[_]*.mr." :mongo mongo))

#|
(defjs map1()
  (if (< this.k  90) 
      (emit this.name 1)
      (emit this.k 99)))

(defjs map2()
  (emit this.k (ps:create :count 1)))

(defjs map3()
  (emit this.name (ps:create :count 1)))



(defjs reduce1(k vals)
  (return (aref vals 0)))

(defjs reduce2(k vals)
  (return k))

(defjs reduce3(k vals)
  (return (length (aref vals 0))))

(defjs reduce4(k vals)
  (return (ps:create :vals vals)))

(defjs test_sum()
  (let ((l (make-array 1 2 3 4))
	(sum 0))
    (dolist (c l)
      (incf sum c))
    (return sum)))

(defjs reduce5(k vals)
  (let ((sum 0))
    (dolist (c vals)
      (incf sum c))
    (return sum)))

(defjs map4()
  (emit this.name 1)
  (emit "hello" this.k))

(defjs reduce6(k vals)
  (let ((sum 0)
	(index 0))
    (dolist (c vals)
      (if (= index 3)
	  (return c)
	  (progn
	    (incf index 1)
	    (incf sum c))))
    (return sum)))

(defjs reduce7(k vals)
  (let ((sum 0)
	(index 0))
    (dolist (c vals)
      (if (= index 3)
	  (return c)
	  (progn
	    (incf index 1)
	    (incf sum c))))
    (return sum)))

(defjs reduce8(k vals)
  (let ((arr (make-array))
	(index 0))
    (dolist (c vals)
      (setf (aref arr index) c)
      (incf index 1))
    (return (ps:create :retval (aref arr 5)))))
  

;($map-reduce "foo" m r  :out "testit" :query ($> "l" -900) :limit 2)
;($map-reduce "foo" m r  :out "testit" :query ($> "l" -900))
;(mr-results ($map-reduce "foo" m r :query ($> "l" -9)))
(defjs at()
  (let ((arr (make-array)))
    (setf (aref arr 0) 10)
    (setf (aref arr 1) 20)
    (setf (aref arr 2) 30)
    (setf (aref arr 4) 40)
    (setf (aref arr 5) 80)
    (return (aref arr 2))))

|#
