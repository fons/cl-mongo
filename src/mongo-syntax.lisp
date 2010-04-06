(in-package :cl-mongo)

(defmacro $exp+ (&rest args)
  (cond (  (zerop (length args) )  '())
	(  (symbolp (car args) ) (cond ( (fboundp (car args)) `(progn ,args))
				       ( (boundp (car args)) `(list ,(car args) ,@(cdr args)))
				       ( t                   `(list ,(car args) ,@(cdr args)))))
	(  (atom  (car args) ) `(cons ,(car args) ($exp+ ,@(cdr args))))
	(  (consp (car args) ) `(cons ($exp+ ,@(car args) ) ($exp+ ,@(cdr args))))
	(t (format t "can only handle atoms and cons")))) 


(defmacro construct-$+- (val arg &rest args)
  (let ((kvc (gensym)))
    `(let ((,kvc (kv ,arg ,val)))
       (dolist (el (list ,@args))
	 (setf ,kvc (kv ,kvc (kv el ,val))))
       ,kvc)))

(defmacro $- (arg &rest args)
  `(construct-$+- 0 ,arg ,@args))

(defmacro $+ (arg &rest args)
  `(construct-$+- +1 ,arg ,@args))

(defmacro expand-selector (&rest args)
  `(let ((result ,@args))
     (cond ( (typep result 'kv-container) result)
	   ( (typep result 'pair)         result)
	   ( (null result)                result)
	   ( t ($+ result)))))

;(op-split (list "k" "l" 8)) ---> ("k" "l"), 8

(defun op-split (lst &optional (accum ()))
  (if (null (cdr lst))
      (values (nreverse accum) (car lst))
      (op-split (cdr lst) (cons (car lst) accum))))

(defun unwrap (lst)
  (if (cdr lst) 
      lst
      (unwrap (car lst))))
;    `(multiple-value-bind (,keys ,val) (op-split (unwrap (list ,@args)))
(defmacro $op* (op &rest args)
  (let ((keys (gensym))
	(key  (gensym))
	(kvc  (gensym))
	(val  (gensym)))
    `(multiple-value-bind (,keys ,val) (op-split (unwrap (list ($exp+ ,@args))))
       (let ((,kvc (kv (car ,keys) (kv ,op ,val))))
	 (dolist (,key (cdr ,keys))
	   (setf ,kvc (kv ,kvc (kv ,key (kv ,op ,val)))))
	 ,kvc))))


(defun map-reduce-op (op lst)
  (reduce (lambda (x y) (kv x y) ) (mapcar (lambda (l) ($op* op l) ) lst)))

(defmacro $op (op &rest args)
  (cond ( (consp (car args) ) `(map-reduce-op ,op ($exp+ ,@args)))
	( t                   `($op* ,op ,@args))))

(defmacro $ (&rest args)
  `(kv ,@args))

(defmacro $> (&rest args)
  `($op "$gt" ,@args))

(defmacro $>= (&rest args)
  `($op "$gte" ,@args))

(defmacro $< (&rest args)
    `($op "$lt" ,@args))

(defmacro $<= (&rest args)
  `($op "$lte" ,@args))

(defmacro $!= (&rest args)
  `($op "$ne" ,@args))

(defmacro $in (&rest args)
  `($op "$in" ,@args))

(defmacro $!in (&rest args)
  `($op "$nin" ,@args))

(defmacro $mod (&rest args)
  `($op "$mod" ,@args))

(defmacro $all (&rest args)
  `($op "$all" ,@args))

(defmacro $exists (&rest args)
  `($op "$exists" ,@args))

(defmacro $size (&rest args)
  `($op "$size" ,@args))

(defun empty-str(str)
  (if (and str (zerop (length str))) 
      (format nil "\"\"")
      str))

(defmacro $/ (regex options)
  `(make-bson-regex (empty-str ,regex) ,options))
  
(defmacro $not (&rest args)
  `(let ((result ,@args))
     (kv (pair-key result) (kv "$not" (pair-value result)))))

(defmacro $kv-eval (&rest args)
  `(kv ,@args))

(defmacro $em (array &rest args)
  `(kv ,array (kv "$elemMatch" (kv ,@args))))

(defmacro $where (&rest args)
  `(kv "$where" ,@args))

#|

($index "foo" "field" :unique :asc)

($index+ "foo" ("field1" :unique :asc) ("field2") )
($index+ "foo" ("field1" :unique :asc :dropDups ) ("field2") )

($index- "foo" *)

($index- "foo" *)


|#

