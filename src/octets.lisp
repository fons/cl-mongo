(in-package :cl-mongo)

(defconstant +max-uint32+ 4294967296           "max + 1 value of unsigned 32 bit integer")
(defconstant +max-uint64+ 18446744073709551616 "max + 1 value of unsigned 64 bit integer")

(defun make-octet-vector (sz &key (init-fill 0))
  (make-array sz :element-type '(unsigned-byte 8) :initial-element 0 :fill-pointer init-fill :adjustable t))

(defun make-fixed-size-octet-vector (sz)
  (make-array sz :element-type '(unsigned-byte 8)  :fill-pointer 0 :initial-element 0))

;; (append-to-octet-vector (vector-pop source) (vector-push-extend (vector-pop source) target)))
(defun append-to-octet-vector (source target)
  (assert (adjustable-array-p target))
  ;;(format t "~% (~A : ~A)" source target)
  (cond ((< 0 (length source)) (append-to-octet-vector (vector-pop source) target))
	(t target)))


(defun add-to-array (elem arr)
  (vector-push-extend elem arr)
  arr)

;(defun add-octets*(source target)
;  (pop-to (nreverse source) target))

(defun add-octets (source target &key (start 0) (from-end 0))
  (let ((to (- (length source) start from-end)))
    (dotimes (index to)
      (vector-push-extend (aref source (+ start index)) target))
    target))

(defun set-octets (start source target)
  (dotimes (ind (length source))
    (let ((index (+ start ind)))
      (setf (aref target index) (aref source ind))))
  target)

(defun to-octet (val size)
  (let ((ov (make-fixed-size-octet-vector size)))
    (dotimes (position size)
      (let ((pos (* 8 position)))
	(vector-push (ldb (byte 8 pos) val) ov)))
    ov))

(defun int32-to-octet (val)
  (to-octet val 4))

(defun int64-to-octet (val)
  (to-octet val 8))

(defun byte-to-octet (val)
  (to-octet val 1))

(defun bool-to-byte (val)
  (if (eql t val) 1 0))

(defun to-signed-value (value &key size max-uint)
  (cond ((and (< 0 value)
              (logbitp size value)) (- value max-uint))
	((<= max-uint value) 0)
	(t value)))

(defun to-val (vec size)
  (let ((value 0))
    (dotimes (position size)
      (let ((pos (* 8 position)))
	(setf (ldb (byte 8 pos) value) (aref vec position))))
    value))

(defun octet-to-int32 (vec)
  (to-signed-value (to-val vec 4) :size 31 :max-uint +max-uint32+))

(defun octet-to-int64 (vec)
  (to-signed-value (to-val vec 8) :size 63 :max-uint +max-uint64+))

(defun octet-to-uint64 (vec)
  (to-val vec 8))

(defun octet-to-byte (vec)
  (to-val vec 1))

(defun byte-to-bool (val)
  (eql 1 val))

;; s/b macro
(defun string-to-null-terminated-octet (str)
  (babel:string-to-octets (concatenate 'string str (string #\null))))

(defun null-terminated-octet-to-string (arr length)
  (babel:octets-to-string (subseq arr 0 (- length 1))))

(defun add-integer (int32 array)
  (assert (integerp int32))
  (let ((oct (int32-to-octet int32)))
    (append-to-octet-vector oct array)))

;;sb macro
(defun positive (val)
  (plusp val))

;;------------------------------------------------

(defun to-val.1 (vec start size)
  (let ((value 0))
    (dotimes (position size)
      (let ((pos (* 8 position)))
	(setf (ldb (byte 8 pos) value) (aref vec (+ start position)))))
    value))


(defun octet-to-int32.1 (vec start)
  (to-signed-value (to-val.1 vec start 4) :size 31 :max-uint +max-uint32+))

(defun octet-to-int64.1 (vec start)
  (to-signed-value (to-val.1 vec start 8) :size 63 :max-uint +max-uint64+))

(defun octet-to-uint64.1 (vec start)
  (to-val.1 vec start 8))

(defun octet-to-byte.1 (vec start)
  (to-val.1 vec start 1))
