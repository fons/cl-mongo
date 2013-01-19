(in-package :cl-mongo-test)

(assert
 (equalp
  #(84 72 73 83 32 73 83 32 66 73 78 65 82 89 32)
  (cl-mongo::data
   (car                                 ;why does get-element return a list?
    (get-element
     "a"
     (cl-mongo::bson-decode
      28 0 1
      (ql-http::octet-vector 28 0 0 0 5 97 0 15 0 0 0 0 84 72 73 83 32 73
                             83 32 66 73 78 65 82 89 32 0)))))))
