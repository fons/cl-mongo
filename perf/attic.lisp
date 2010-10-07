
(defun test-it ()
  (let ((nix (do-reduce #'collect)))
    nil))

(defun count-elem (l r)
  (+ l 1))


(defun collect (l r)
  (if (listp l)
      (cons r l)
      (list r l)))

;;with-input-from-string (stream "This is my input via stream.")
;;    (read stream))
;;THIS
;;? (with-output-to-string (stream)
;;    (princ "I'm writing to memory!" stream))
;;"I'm writing to memory!"

;;    (funcall func (list 12))))
;;    (format t "elem ~A ~%" (length elem))))
;;  (length R))
;;      (push 

(defvar *HT* (make-hash-table :test 'equal :size 100))

(defun print-collect (l r)
  (with-output-to-string (stream l)
    (format stream "~A~%" r))
  l)


(defun print-it (elem)
  (with-output-to-string (stream)
    (print-elem elem stream)))


(defun print-it2 (val)
  (with-output-to-string (stream)
    (format stream "hello [~A] ~%" val)
    (format stream "hello 2 ~%") ))

(defun print-it3 (val str)
  (with-output-to-string (stream str)
    (format stream "{==>hello [~A] ~%" val)
    (format stream "==>hello 2} ~%"))
  str)


(setf *S* (make-array 0 :element-type 'character :adjustable t  :fill-pointer 0))

(maphash (lambda (k v) (format t "~A -> ~A~%" k (print-it elem))) *HT*)
