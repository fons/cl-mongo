(in-package :cl-mongo-test)

(defvar *test-collection* "foo" "name of the test collection")

;--------------------------------------------------------------------------------------
  
(defun count-documents ( &key (collection *test-collection*) (size 5) (wait 1) )
  (reset-test-collection collection size wait)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
    (let ((client-count (length (docs (iter (db.find collection :all)))))
	  (server-count (get-element "n" (car (docs (db.count collection :all))))))
      (format t "testing counting : client count : ~A -- server count ~A ~%" client-count server-count)
      (run-test "testing wether client and server count are equal"
		(force-single-float client-count) 
		(force-single-float (float server-count)))))
  (reset-test-collection collection 0))

(defun find-doc-by-field ( &key (collection  *test-collection*) (size 5) (wait 1) )
  (reset-test-collection collection size wait)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
    (let ((found (length (docs (iter (db.find collection (kv "name" "simple")))))))
      (format t "findOne is default : ~A ~%" found)
      (run-test "test of findone being the default" 1 found)))
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
    (let ((found (length (docs (iter (db.find collection (kv "name" "simple") :limit 0))))))
      (format t "limit == 0 returns all ~A : ~A ~%" size found)
      (run-test "test using keyword :all" size found )))
  (reset-test-collection collection 0))


(defun delete-docs (&key (collection *test-collection*) (size 5) (wait 1) )
  (reset-test-collection collection size wait)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
    (let ((cnt (* 1.0 (get-element "n" (car (docs (db.count collection :all)))))))
      (unless (> cnt 0) (insert-lots collection size)))
    (rm collection :all)
    (let ((cnt (* 1.0 (get-element "n" (car (docs (db.count collection :all)))))))
      (run-test "deleting all documents" 0.0 (force-single-float cnt))))
  (reset-test-collection collection size wait)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" )
    (let ((total  (ret (db.count collection :all)))
	  (target (ret (db.count collection ($< "l" -4)))))
      (db.delete collection ($< "l" -4) )
      (run-test "selective delete" (- total target) (ret (db.count collection :all)))))
  (reset-test-collection collection 0))

(defun db.find-regression-all (&key (collection *test-collection*) (size 5) (wait 1))
  (reset-test-collection collection size wait)  
;;;test retrieval of all documents with an iterator..
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (let ((all (length (docs (iter (db.find "foo" :all))))))
      (run-test "testing db.find :all; counts should match" size all)))
;;; get the first document (this is not indexed so index may fail to be equal to 0...
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (let* ((documents (docs (iter (db.find "foo" :all :limit 1))))
	   (index (get-element "index-this" (car documents))))
      (run-test "get first document" 1 (length documents) )
      (run-test "assuming first document has index 0" 0 index )))
;;; test of skip; skip the first (size - floor (* size 0.5))
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (let* ((documents (docs (iter (db.find "foo" :all :limit 0 :skip (floor (* 0.5 size))))))
	   (skip   (floor (* 0.5 size)))
	   (first-index (get-element "index-this" (car documents)))
	   (last-index  (get-element "index-this" (car (reverse documents))))
	   (expected-count (- size skip))
	   (expected-last-index (- size 1)))
      ;(format t "size : ~D skip ~D first index: ~D last index: ~D expected: ~D expected last index ~D ~%"
      ;size skip first-index last-index expected-count expected-last-index)
      (run-test "testing assumption of first index value" skip first-index )
      (run-test "testing assumption of first index value" expected-last-index last-index )
      (run-test "testing skipping documents" expected-count (length documents) )))
  (reset-test-collection collection 0))


(defun db.sort-regression (&key (collection *test-collection*) (size 5) (wait 1))
  (reset-test-collection collection size wait) 
  (format t "~%------------------------------------------------------------~%")
  (format t "sorting test : printing out server side sorted arrays; no test yet ..~%")
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (let* ((ret (docs (iter (db.sort collection :all :field "k" :selector "k" ))))
	   (elem (mapcar (lambda (d) (get-element "k" d)) ret)))
      (format t "elem : ~A ~%" elem)
      (reduce (lambda (x y) (progn (assert-eql t (< x y) x y ) y)) elem)))
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (let* ((ret (docs (iter (db.sort collection :all :field "k" :selector "k"  :asc nil))))
	   (elem (mapcar (lambda (d) (get-element "k" d)) ret)))
      (format t "elem : ~A ~%" elem)
      (reduce (lambda (x y) (progn (assert-eql t (> x y) x y ) y)) elem)))
  (format t "~%------------------------------------------------------------~%")
  (reset-test-collection collection 0))  

(defun map-reduce-truth (tv l)
  (reduce (lambda (x y) (and x y) ) (mapcar (lambda (x) (assert-eql tv (when x t) x)) l)))
	
(defun db.find-selector-regression (&key (collection *test-collection*) (size 5) (wait 1))
  (reset-test-collection collection size wait)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (let* ((ret (docs (iter (db.find collection :all :selector ($+ "k" "l") ))))
	   (ks  (mapcar (lambda (d) (get-element "k" d)) ret))
	   (ls  (mapcar (lambda (d) (get-element "l" d)) ret))
	   (excl (mapcar (lambda (d) (get-element "index-this" d)) ret)))
      (format t "elems with key k : ~A ~%" ks)
      (run-test "selection is index k"  t (map-reduce-truth t ks))
      (format t "elems with key l : ~A ~%" ls)
      (run-test "selection is index l"  t (map-reduce-truth t ls))
      (format t "elems with key 'index-this' (s/b none) : ~A ~%" excl)
      (run-test "selection is other index "  t (map-reduce-truth nil excl))))

  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (let* ((ret (docs (iter (db.find collection :all :selector ($- "k" "l") ))))
	   (ks  (mapcar (lambda (d) (get-element "k" d)) ret))
	   (ls  (mapcar (lambda (d) (get-element "l" d)) ret))
	   (excl (mapcar (lambda (d) (get-element "index-this" d)) ret)))
      (format t "elems with key k (s/b none !) : ~A ~%" ks)
      (run-test "selection is excluded index k"  t (map-reduce-truth nil ks))
      (format t "elems with key l (s/b none !) : ~A ~%" ls)
      (run-test "selection is excluded index l"  t (map-reduce-truth nil ls))
      (format t "elems with key index-this : ~A ~%" excl)
      (run-test "selected index "  t (map-reduce-truth t excl))))
  (reset-test-collection collection 0))

(defun test-filter(fun lst)
  (let ((l))
    (dolist (obj lst)
      (when (funcall fun obj) (push obj l)))
    (nreverse l)))

;-------------------------------------
(defun db.find-elem-match-regression (&key (collection *test-collection*)  (size 5) (wait 1))
  (labels ((zip (l1 l2)
	     (labels ((zip* (l1 l2 accum)
			(if (or (null l1) (null l2))
			    (nreverse accum)
			    (zip* (cdr l1) (cdr l2) (cons (list (car l1) (car l2) ) accum)))))
	       (zip* l1 l2 ()))))
    (rm collection :all)
    (insert-doc-with-arrays collection size)    
    (sleep wait)
    (let* ((docs (docs (iter (db.find "foo" :all :selector "array-1" :limit 0))))
	   (lst (mapcar (lambda (d) (get-element "array-1" d)) docs))
	   (l1 (mapcar (lambda (el) (get-element "a" (nth 2 el ))) lst) )
	   (l2 (mapcar (lambda (el) (get-element "b" (nth 2 el ))) lst))
	   (arr (zip l1 l2)))

      (run-test "test of element match a = 10 ; b > 30"
		(length (test-filter (lambda (el) (and (eql 10 (car el)) (> (cadr el) 30))) arr))
		(length (docs (iter (db.find collection ($em "array-1" ($ "a" 10) ($> "b" 30) ) :limit 0)))))
		
      (run-test "test of element match a >= 10 ; b < 30"
		(length (test-filter (lambda (el) (and (>= 10 (car el)) (< (cadr el) 30))) arr))
		(length (docs (iter (db.find collection ($em "array-1" ($>= "a" 10) ($< "b" 30) ) :limit 0)))))

      (run-test "test of element match a >= 10 ; not (b < 30)"
		(length (test-filter (lambda (el) (and (eql 10 (car el)) (not (< (cadr el) 30)))) arr))
		(length (docs (iter (db.find collection ($em "array-1" ($>= "a" 10) ($not ($< "b" 30)) ) :limit 0)))))

      (rm collection :all))))

(defun db.find-advanced-query-regression(&key (collection *test-collection*)  (size 23) (wait 1) )
  (reset-test-collection collection size wait)
  (insert-lots-more collection size)
  (sleep wait)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
  (labels ((test-server (arg)
	     (let* ((ret (docs (iter (db.find collection arg :limit 0 :selector ($+ "k") ))))
		    (lst  (mapcar (lambda (d) (get-element "k" d)) ret)))
	       lst)))
    (let*  ((all (test-server :all)) 
	    (all-size (length all))
	    (lst (test-filter (lambda (x) (not (eql nil x))) all))
	    (not-k-count (- all-size (length lst)))
	    (_min  (reduce #'min lst))
	    (_max  (reduce #'max lst))
	    (_mid  (* 0.5 (+ _min _max))))
      
      (run-test "testing >"
		(length (test-filter (lambda (i) (> i _mid)) lst))
		(length (test-server ($> "k" _mid))))
      
      (run-test "testing k < mid "
		(length (test-filter (lambda (i) (< i _mid)) lst))
		(length (test-server ($< "k" _mid) )))
	
      (run-test "testing k >= mid "
      (length (test-filter (lambda (i) (>= i _mid)) lst))
      (length (test-server ($>= ("k" _mid) ) )))
      
      (run-test "testing k <= mid "
		(length (test-filter (lambda (i) (<= i _mid)) lst))
		(length (test-server ($<= ("k" _mid)) )))
		
      (run-test "testing k != min "
      (+ not-k-count (length (test-filter (lambda (i) (not (eql i _min))) lst)))
      (length (test-server ($!= ("k" _min)) )))

      ;;clisp's mod doesn't return an integer !
      ;(format t "lst : ~A ~%" lst)
      ;(format t "lst filtered mod 10 == 1 : ~A ~%" (mapcar (lambda (i) (floor (+ 0.5 (mod i 10)))) lst))
      (run-test "testing mod 10 == 2 "
		(length (test-filter (lambda (i) (equalp 1 (floor (+ 0.5 (mod i 10))))) lst))
		(length (test-server ($mod "k" (10 1)))))
      
      (run-test "testing not mod 10 == 2 "
		(+ not-k-count (length (test-filter (lambda (i) (not (equalp 1 (floor (+ 0.5 (mod i 10)))))) 
						    lst)))
		(length (test-server ($not ($mod "k" (10 1))))))

      (run-test "testing k in (min, mid, max) "
		(length (test-filter (lambda (i) (or (equalp i _min)  
						     (equalp i _mid)
						     (equalp i _max))) lst))
		(length (test-server ($in "k" (_min _mid _max)))))


      (run-test "testing k not in (min, mid, max) "
		(+ not-k-count (length (test-filter (lambda (i) (not (or (equalp i _min)  
									 (equalp i _mid)
									 (equalp i _max)))) lst)))
		(length (test-server ($!in "k" (_min _mid _max)))))


      (run-test "testing k exists "
		(length lst)
		(length (test-server ($exists "k" t ))))
		

      (run-test "testing k does not exist "
		not-k-count
		(length (test-server ($exists "k" nil ))))

      (run-test "testing regex /jump*/i "
		not-k-count 
		(length (docs (iter (db.find collection (kv "regex" ($/ "jump*" "i")) :limit 0)))))


    ))
  (reset-test-collection collection 0)))

(defun db.find-advanced-query-regression-2(&key (collection *test-collection*)  (size 23) (wait 1))
  (reset-test-collection collection size wait)
  (insert-lots-more collection size)
  (sleep wait)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
  (labels ((test-server (arg)
	     (let* ((ret (docs (iter (db.find collection arg :limit 0 :selector ($+ "k") ))))
		    (lst  (mapcar (lambda (d) (get-element "k" d)) ret)))
	       lst)))
    (let*  ((all (test-server :all)) 
	    (all-size (length all))
	    (lst (test-filter (lambda (x) (not (eql nil x))) all))
	    (not-k-count (- all-size (length lst))))

      (run-test "testing $all "
		not-k-count 
		(length (docs (iter (db.find collection ($all "list-1" (1 2 (* 1 3) 4 )) :limit 0)))))
		
      (run-test "testing $all and value in array "
		(length (docs (iter (db.find collection (kv   "list-2" (* 2 3) ) :limit 0))))
		(length (docs (iter (db.find collection ($all "list-2" ( (* 2 3) ) ) :limit 0)))))

      (run-test "testing $in and value in array (this may fail) "
		(length (docs (iter (db.find collection (kv "list-2" (* 2 3) ) :limit 0))))
		(length (docs (iter (db.find collection ($in "list-2" ( (* 2 3) ) ) :limit 0)))))

      (run-test "testing $where count < -12 "
		(length (docs (iter (db.find collection ($< "count" -12 ) :limit 0))))
		(length (docs (iter (db.find collection ($where "this.count < -12" ) :limit 0)))))
      
      
    ))
  (reset-test-collection collection 0)))

(defun test-serialization (&key (collection *test-collection*)  (size 23) (wait 1) )
  (declare (ignore wait size))
  (reset-test-collection collection 0)
  (send-doc-in-doc-in-doc collection)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (pp (db.find collection :all)))
  (rm collection :all))

(defun test-indexing (&key (collection *test-collection*)  (size 23) (wait 1) )
  (reset-test-collection collection size wait)
  ($index collection :rm :all)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    ($index collection :asc "index_this2")
    (insert-lots collection size)
    (sleep wait)
    (run-test "testing indexing on index_this2; simple, should double the doc count "
	      (force-double-float (* 2 size) )
	      (get-element "n" (car (docs (db.count "foo" :all))))))
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    ;;this will fail to install b/c there are duplicates
    ($index collection :unique :desc ("index_this2") )
    ($index collection :show)
    (insert-lots collection size)
    (sleep wait)
    (run-test "testing unique index on index_this2, desc; should increase count"
	      (force-double-float (* 3 size))
	      (get-element "n" (car (docs (db.count "foo" :all))))))
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    ($index collection :unique :drop-duplicates :desc ("index_this2") )
    ($index collection :show)
    (insert-lots collection size)
    (sleep wait)
    (run-test "testing unique index on index_this2, desc with duplicates dropped; should decrease count"
	      (force-double-float size)
	      (get-element "n" (car (docs (db.count "foo" :all))))))
  ($index collection :rm :all)
  (reset-test-collection collection 0))

(defun test-modifiers (&key (collection *test-collection*)  (size 23) (wait 1) )
  (reset-test-collection collection size wait)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (let* ((doc (car (docs (db.find collection ($ "6" 6) ))))
	   (val-before (get-element "k" doc)))

      (db.update collection ($ "6" 6) ($inc "k" 900))
      (run-test "increase value of field using $inc"
		(force-double-float (+ val-before 900))
		(get-element "k" (car (docs (db.find "foo" ($ "6" 6))))))

      (db.update collection ($ "6" 6) ($set "k" -89))
      (run-test "set value of field using $set"
		-89
		(get-element "k" (car (docs (db.find "foo" ($ "6" 6)))))) 

      (db.update collection ($ "6" 6) ($unset "k"))
      (run-test "remove field using $unset"
		nil
		(get-element "k" (car (docs (db.find "foo" ($ "6" 6)))))) 
      
      (db.update collection ($ "6" 6) ($push "k" 1 ))
      (run-test "push a value  to the field using $push"
		1
		(car (get-element "k" (car (docs (db.find "foo" ($ "6" 6)))))))

      (db.update collection ($ "6" 6) ($push-all "k" (78 89 56 34 12 100 ) ))
      (run-test-equal "push a list of values to the field using $push-all"
		      (list 1 78 89 56 34 12 100)
		      (get-element "k" (car (docs (db.find "foo" ($ "6" 6)))))) 

      (db.update collection ($ "6" 6) ($add-to-set "k" 989 ))
      (run-test-equal "add a value to the field using $add-to-set"
		      (list 1 78 89 56 34 12 100 989)
		      (get-element "k" (car (docs (db.find "foo" ($ "6" 6)))))) 

      (db.update collection ($ "6" 6) ($pop-back "k"))
      (run-test-equal "remove last element  with $pop-back"
		      (list 1 78 89 56 34 12 100)
		      (get-element "k" (car (docs (db.find "foo" ($ "6" 6)))))) 

      (db.update collection ($ "6" 6) ($pop-front "k"))
      (run-test-equal "remove first element  with $pop-front"
		      (list 78 89 56 34 12 100)
		      (get-element "k" (car (docs (db.find "foo" ($ "6" 6)))))) 

      (db.update collection ($ "6" 6) ($pull "k" 89))
      (run-test-equal "remove element  with $pull"
		      (list 78 56 34 12 100)
		      (get-element "k" (car (docs (db.find "foo" ($ "6" 6))))))

      (db.update collection ($ "6" 6) ($pull-all "k" (56 34 100)))
      (run-test-equal "remove elements  with $pull-all"
		      (list 78  12 )
		      (get-element "k" (car (docs (db.find "foo" ($ "6" 6))))))


      ))
  (reset-test-collection collection 0))


;;------------------------------------------

(defun test-javascript (&key (collection *test-collection*)  (size 23) (wait 1) )
  (reset-test-collection collection size wait)
  (rm "system.js" :all)
  ($index collection :rm :all)
  ;; Test simple javascript capabilities
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (defjs echo(x) 
      (return x))
    (run-test-equal "echo with client-side js : number" (force-double-float 1) (echo 1))
    (run-test-equal "echo with client-side js : calculation" (force-double-float 3) (echo (+ 1 2)))
    (run-test-equal "echo with client-side js : string" "hello" (echo "hello"))
    (run-test-equal "echo with client-side js : array" 
		    (list "hello" (force-double-float 56) (list (force-double-float 1) 
								(force-double-float 2))) 
		    (echo (list "hello" 56 (list 1 2 )))))

  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (defjs set-value(value) 
      (transform db.foo (lambda (obj) (setf (slot-value obj 'k) value))))
    (labels ((testit(descr value) 
	       (set-value value)
	       (run-test-equal descr (force-double-float size) 
			       (ret (db.count collection (kv "k" value))))))
      (testit "set field in collection to number" 9999)
      (testit "set field in collection to string" "hello")
      (testit "set field in collection to list" (list "hello" 999.99))))
  ;test the remotely installed versions
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (defsrvjs remote_echo(x) 
      (return x))
    (run-test-equal "echo with client-side js : number" (force-double-float 1) (remote_echo 1))
    (run-test-equal "echo with client-side js : calculation" (force-double-float 3) 
		    (remote_echo (+ 1 2)))
    (run-test-equal "echo with client-side js : string" "hello" (remote_echo "hello"))
    (run-test-equal "echo with client-side js : array" 
		    (list "hello" (force-double-float 56) (list (force-double-float 1) 
								(force-double-float 2))) 
		    (remote_echo (list "hello" 56 (list 1 2 )))))

  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (defsrvjs remote_setvalue(value) 
      (transform db.foo (lambda (obj) (setf (slot-value obj 'k) value))))
    (labels ((testit(descr value) 
	       (remote_setvalue value)
	       (run-test-equal descr (force-double-float size) 
			       (ret (db.count collection (kv "k" value))))))
      (testit "set field in collection to number" 9999)
      (testit "set field in collection to string" "hello")
      (testit "set field in collection to list" (list "hello" 999.99))))
  (reset-test-collection collection 0)

  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (defsrvjs tf8() 
      (let* ((l (make-array 1 2 3 4 5))
	     (k (make-array)))
	(ps:dolist (c l)
	  (k.push  c ) 
	  (db.foo.save (ps:create :field c)))
	(return k)))
    (run-test-equal "remotely create objects : return data" 
		    (mapcar #'force-double-float (list 1 2 3 4 5))
		    (tf8))
    (run-test-equal "remotely create objects"
		    (mapcar #'force-double-float (list 1 2 3 4 5))
		    (mapcar (lambda(d) (get-element "field" d)) (docs (db.find "foo" :all)))))
  (reset-test-collection collection size wait)

  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (defsrvjs multip (x y)
      (return (* y x)))
    (install-js multip)
    (defsrvjs tf17(value) 
      (transform db.foo (lambda (obj) 
			  (setf (slot-value obj 'k) (multip value 89)))))
    (tf17 5)
    (run-test-equal "set value referencing remotly installed script"
		    t 
		    (reduce (lambda ( x y) (and x y))
			    (mapcar (lambda (d) (eql (* 5.0d0 89.0d0) (get-element "k" d))) 
				    (docs (db.find "foo" :all))))))
  
  (rm "system.js" :all)
  (reset-test-collection collection 0))

(defun test-map-reduce (&key (collection *test-collection*)  (size 23) (wait 1) )
  (mr.gc.all)
  (reset-test-collection collection size wait)
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (defjs map_keys()
      (emit this.name 900)
      (emit "hello" this.k))
    (defjs get_some_value(k vals)
      (let ((sum 0)
	    (index 0))
	(dolist (c vals)
	  (if (= index 3)
	      (return c)
	      (progn
		(incf index 1)
		(incf sum c))))
	(return sum)))
    ($map-reduce collection map_keys get_some_value :out "testit")
    (run-test-equal "value is equal to value associated with key 'name'"
		    (force-double-float 900)
		    (get-element "value" (car (docs (db.find "testit" ($ "_id" "simple")))))))

  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (defjs map_keys()
      (emit this.name 9))
    (defjs sumreduce(k vals)
      (let ((sum 0))
	(dolist (c vals)
	  (incf sum c))
	(return sum)))
    
    (run-test-equal "sum the value associated with key 'name'"
		    (force-double-float (* size 9))
		    (get-element "value" (car (docs (mr.p 
						     ($map-reduce collection map_keys sumreduce)))))))
  (with-mongo-connection (:host "localhost" :port *mongo-default-port* :db "test" ) 
    (defjs map_keys()
      (emit this.k 9))

    (defjs sumreduce(k vals)
	(return 1))
    
    (run-test-equal "emit value associated with key k"
		    size
		    (length (docs (mr.p ($map-reduce collection map_keys sumreduce :keeptemp t))))))
  
  (remove-js map_keys)
  (remove-js sumreduce)
  (remove-js get_some_value)
  (run-test-equal "automatic cleanup of temp map-reduce stores"
		  2 ;expect two becuase I used :keeptemp t once.. 
		  (length (cl-mongo::mr.collections)))
  ;;keep here; needs some time..
  (mr.gc.all)
  (db.run-command :drop :collection "testit" )
  (run-test-equal "clean up all mr collections"
		  0
		  (length (cl-mongo::mr.collections)))
  (reset-test-collection collection 0))

;;--------------------------------------------------------------------------

(defun test-delete (&optional (collection *test-collection*) )
  (dolist (size (geometric-range 10 5 ))
    (delete-docs :collection collection :size size)))

(defun test-query-field-selection (&optional (collection *test-collection*))
  (dolist (size (geometric-range 5 5 ))
    (find-doc-by-field :collection collection :size size)))

(defun test-document-count (&optional (collection *test-collection*))
  (dolist (size (geometric-range 5 5 ))
    (count-documents :collection collection :size size)))

(defun test-find-all (&optional (collection *test-collection*))
  (dolist (size (geometric-range 2 4 5 ))  
    (db.find-regression-all :collection collection :size size)))

(defun test-sort (&optional (collection *test-collection*))
  (dolist (size (geometric-range 2 4))  
    (db.sort-regression  :collection collection :size (* 10 size)))) 

(defun test-find-selector (&optional (collection *test-collection*))
  (dolist (size (geometric-range 2 4))  
    (db.find-selector-regression :collection collection :size size)))

;;;;;;;;;;;;;


(defun quick-test( &key (collection *test-collection*) (size 43) (wait 1)) 
  (with-test-package "test counting documents"
      (count-documents :collection collection :size size :wait wait))
  (with-test-package "finding documents by field"
    (find-doc-by-field :collection collection :size size :wait wait))
  (with-test-package "deleting documents"
    (delete-docs :collection collection :size size :wait wait))
  (with-test-package "finding documents "
    (db.find-regression-all :collection collection :size size :wait wait))
  (with-test-package "finding documents with a selector"
    (db.find-selector-regression :collection collection :size size :wait wait))
  (with-test-package "sorting documents by field"
    (db.sort-regression  :collection collection :size size :wait wait))
  (with-test-package "matching document elements by field"
    (db.find-elem-match-regression :collection collection :size size :wait wait))
  (with-test-package "advanced query regression elements"
    (db.find-advanced-query-regression :collection collection :size size :wait wait))
  (with-test-package "advanced query regression elements; part 2"
    (db.find-advanced-query-regression-2 :collection collection :size size :wait wait))
  (with-test-package "test update modifiers"
    (test-modifiers :collection collection :size size :wait wait))
  (with-test-package "test java script (local and remote)"
    (test-javascript :collection collection :size size :wait wait))
  (with-test-package "test map-reduce)"
    (test-map-reduce :collection collection :size size :wait wait))
  (with-test-package "test indexing"
    (test-indexing :collection collection :size size :wait wait))
  (with-test-package "test serialization; inspect visually"
    (test-serialization :collection collection :size size :wait wait)))

