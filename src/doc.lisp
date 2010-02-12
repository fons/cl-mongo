(in-package :cl-mongo)

;;
;; This uses documentattion-template to generate reasonably useful 
;; documentation. Some of the edi weitz specific stuff is replaced.


(defun string-replace*(sep new str)
  (let ((l ()))
    (do ((pos  (search sep str :test #'string=)
	       (search sep str :test #'string=)))
	((or (null pos) (eql 0 pos)))
      (push (subseq str 0 pos) l)
      (push new l)
      (setf str (subseq str (+ pos (length sep) ))))
    (nreverse (cons str l))))

(defun string-replace(sep new str)
  (let ((L (string-replace* sep new str)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 s2)) L :initial-value "")))


(defun slurp-stream(stream)
  ;;from
  ;;www.emmett.ca/~sabetts/slurp.html
  ;;
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun write-file(path str)
  (handler-case 
      (with-open-file (stream path :direction :output
			      :if-exists :supersede :if-does-not-exist :create)
	(write-sequence str stream))
    (error(c) 
      (format t "error [~A] on writing to ~A" c path))))

(defun load-file(path)
  (handler-case 
      (with-open-file (stream path :direction :input)
	(slurp-stream stream))
    (error(c) 
      (format t "error [~A] on reading from ~A" c path))))

(defun customize (str) 
  (labels ((customize* (lst str)
	     (if (null lst)
		 str
		 (customize* (cdr lst) 
			     (string-replace (car (car lst)) (cadr (car lst)) str)))))
    (let* ((lst ()))
      (push (list "BSD-style" "MIT-style") lst)
      (push (list "weitz.de/index.html" "www.mohegan-skunkworks.com/index.html") lst)
      (push (list "weitz.de/files/cl-mongo.tar.gz" "github.com/fons/cl-mongo")   lst)
      (customize* lst str))))

(defun gendoc (target) 
  (progn
    (documentation-template:create-template :cl-mongo :subtitle "cl-mongo"
					    :target target
					    :maybe-skip-methods-p t)
    (write-file target (customize (load-file target)))))

(defun strip-html-comments (str) 

(defun segment* (str accum)
  (let* ((start-token "<!--")
	 (end-token   "-->")
	 (start-comment (search start-token str))
	 (end-comment   (search end-token str)
	 (piece     (subseq str 0 start-comment)))
    (if end-comment
	(segment* (subseq str (+ (length end-token) end-comment)) (cons piece accum))
	(nreverse (cons str accum)))))

(defun rebuild* (l accum)
  (if l
      (rebuild (cdr l) (concatenate 'string accum (car l)))
      accum))

(defun select-body (str) 
  (let* ((body-start-token "<body")
	 (body-end-token   "</body>")
	 (start-body (search body-start-token str))
	 (end-body   (search body-end-token str))
	 (piece     (subseq str start-body (+ (length body-start-token) 2 end-body))))
    piece))

(defun strip-comments (str)
  (rebuild* (segment* str () ) ""))

(defun generate-readme (path) 
  (let* ((index-path  (format nil "~A~A" (make-pathname :directory path) "index.html"))
	 (readme-path (format nil "~A~A" (make-pathname :directory path) "readme-base.md"))
	 (target      (format nil "~A~A" (make-pathname :directory path) "README.md"))
	 (readme      (load-file  readme-path))
	 (index       (select-body (strip-comments (load-file index-path))))
	 (merged      (concatenate 'string readme index)))
    (write-file target merged)))

;----
(defun test-segment()
  (let ((str "hello <!-- this is a comment --> world and <!-- an other comment --> foo"))
    (strip-comments str)))

(defun test-it() 
  (generate-readme "/home/fons/Repo/git.hub/cl-mongo/doc/"))

