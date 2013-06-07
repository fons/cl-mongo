(in-package :cl-mongo)

(defmacro $exp+ (&rest args)
  (cond ((zerop (length args))  '())
	((symbolp (car args)) (cond ((fboundp (car args)) `(progn ,args))
				       ((boundp (car args)) `(cons ,(car args) ($exp+ ,@(cdr args))))
				       (t                   `(list ,(car args) ,@(cdr args)))))
	((atom  (car args)) `(cons ,(car args) ($exp+ ,@(cdr args))))
	((consp (car args)) `(cons ($exp+ ,@(car args)) ($exp+ ,@(cdr args))))
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
     (cond ((typep result 'kv-container) result)
	   ((typep result 'pair)         result)
	   ((null result)                result)
	   (t ($+ result)))))

;(op-split (list "k" "l" 8)) ---> ("k" "l"), 8

(defun op-split (lst &optional (accum ()))
  (if (null (cdr lst))
      (values (nreverse accum) (car lst))
      (op-split (cdr lst) (cons (car lst) accum))))

(defun unwrap (lst)
  (if (or (atom lst) (cdr lst))
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
  (reduce (lambda (x y) (kv x y)) (mapcar (lambda (l) ($op* op l)) lst)))

(defmacro $op (op &rest args)
  (cond ((consp (car args)) `(map-reduce-op ,op ($exp+ ,@args)))
	(t                   `($op* ,op ,@args))))


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


(defmacro set-keys (&rest args)
  `(cond ((null ,@args) nil)
	 (t  (reduce (lambda (u v) (append u v)) (mapcar (lambda (x) (list x t)) ,@args)))))

(defun collect-args (lst &optional accum)
  (cond ((null lst)
         (values (nreverse accum) lst))
	((atom lst)
         (values (list lst) nil))
	((not (keywordp (car lst)))
         (error "unexpected format in collect-args"))
	((keywordp (cadr lst))
         (collect-args (cdr lst) (cons (car lst) accum)))
	((null (cadr lst))
         (values (nreverse (cons (car lst) accum)) nil))
	(t (values (nreverse accum) lst))))

(defmacro construct-container* (value args)
  `(cond ((consp ,args) (reduce (lambda (x y) (kv x y)) (mapcar (lambda (x) (kv x ,value)) ,args)))
	 (t (kv ,args ,value))))



(defmacro $index (collection &rest args)
  `(multiple-value-bind (spec fields) (collect-args (unwrap ($exp+ ,@args)))
     (destructuring-bind (&key show rm all unique drop-duplicates asc desc) (append (set-keys spec) fields)
       (let* ((ascenders   (when asc  (construct-container*   1 asc)))
	      (descenders  (when desc (construct-container*  -1 desc)))
	      (index-param (if asc (kv ascenders descenders) descenders)))
	 (cond (show (show :indexes))
	       (rm  (progn (cond (all (nd (db.run-command :deleteindexes :collection ,collection)))
				  (t   (nd (db.run-command :deleteindexes :collection ,collection 
							    :index index-param))))))
	       (t (db.ensure-index ,collection index-param 
				    :unique unique :drop-duplicates drop-duplicates)))))))
	 

;;(db.find "foo" ($ ($ "$min" ($ "value-1" 600)) ($ "$max" ($ "value-1" 610)) ($ "query" ($ nil nil))))


;(normalize-args (list :min '("k" 1) '("z" 89) :max '("l" 2)))

(defun normalize-args (lst &optional accum)
  (cond ((null lst) (nreverse accum))
	((keywordp (car lst)) (normalize-args (cddr lst)
                                              (cons (list (cadr lst)) (cons (car lst) accum))))
	(t  (normalize-args (cdr lst) (cons (cons (car lst) (car accum)) (cdr accum))))))

(defmacro construct-container-lst* (args)
  `(reduce (lambda (x y) (kv x y)) (mapcar (lambda (x) (kv (car x) (cadr x))) ,args)))

(defmacro $range (&rest args)
  `(let ((lst ($exp+ ,@args)))
     (destructuring-bind (&key min max) (normalize-args lst)
       (let ((min-arg (when min (kv "$min" (construct-container-lst* min))))
	     (max-arg (when max (kv "$max" (construct-container-lst* max))))
	     (query   (kv "query" (kv nil nil))))
	 (if min-arg 
	     (kv query min-arg max-arg)
  	     (kv query max-arg))))))

(defmacro upd (op &rest args)
  (cond ((consp (car args)) `(kv ,op (construct-container-lst* ($exp+ ,@args))))
	(t                   `(kv ,op (construct-container-lst* (list ($exp+ ,@args)))))))

(defmacro $inc (&rest args)
  `(upd "$inc" ,@args))

(defmacro $set (&rest args)
  `(upd "$set" ,@args))

; ($unset "k" "l")
(defmacro $unset (&rest args)
  `(kv "$unset" (construct-container* 1 (unwrap ($exp+ ,@args)))))

(defmacro $push (&rest args)
  `(upd "$push" ,@args))

;  `(kv "$push" (construct-container-lst* (list ($exp+ ,@args)))))

(defmacro $push-all (&rest args)
  `(upd "$pushAll" ,@args))

(defmacro $add-to-set (&rest args)
  `(upd "$addToSet" ,@args))

(defmacro $pop-back (&rest args)
  `(kv "$pop" (construct-container* 1 (unwrap ($exp+ ,@args)))))

(defmacro $pop-front (&rest args)
  `(kv "$pop" (construct-container* -1 (unwrap ($exp+ ,@args)))))

(defmacro $pull (&rest args)
  `(upd "$pull" ,@args))

(defmacro $pull-all (&rest args)
  `(upd "$pullAll" ,@args))

(defmacro $where (&rest args)
  `(kv "$where" ,@args))

