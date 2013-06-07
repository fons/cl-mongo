(in-package :cl-mongo)

;;--------------------
;; Deal with floats....
;;
;; From Peter Siebel
;;

(defun scale-integer (value bits)
  (if (zerop value)
      (values 0 0)
      (let ((scale (- bits (integer-length value))))
        (values (round (* value (expt 2 scale))) scale))))

(defun scale (mantissa exponent mantissa-bits)
  (multiple-value-bind (mantissa scale) (scale-integer mantissa mantissa-bits)
    (values mantissa (- exponent scale))))

(defun denormalize (mantissa exponent bias mantissa-byte)
  (multiple-value-bind (mantissa exponent) (scale mantissa exponent (byte-size mantissa-byte))
    (incf exponent (byte-size mantissa-byte))
    (values (ash mantissa (- exponent (1+ (- bias)))) (- bias))))

(defun encode-float-bits (float sign-byte exponent-byte mantissa-byte bias)
  (multiple-value-bind (original-mantissa original-exponent sign) (integer-decode-float (float float 0d0))
    (multiple-value-bind (mantissa exponent) (scale original-mantissa original-exponent (1+ (byte-size mantissa-byte)))
      (incf exponent (byte-size mantissa-byte))
      (when (zerop mantissa) (setf exponent (- bias)))
      (when (<= exponent (- bias))
        (setf (values mantissa exponent) (denormalize original-mantissa original-exponent bias mantissa-byte)))
      (incf exponent bias)
      (when (> (integer-length exponent) (byte-size exponent-byte))
        (setf mantissa 0 exponent (ldb (byte (byte-size exponent-byte) 0) (lognot 0))))
      (let ((result 0))
        (setf (ldb sign-byte     result) (if (plusp sign) 0 1))
        (setf (ldb exponent-byte result) exponent)
        (setf (ldb mantissa-byte result) mantissa)
        result))))

(defun encode-double-float-bits (float)
  (encode-float-bits float (byte 1 63) (byte 11 52) (byte 52 0) 1023))

(defun decode-float-bits (bits sign-byte exponent-byte mantissa-byte bias)
  (let ((sign (if (zerop (ldb sign-byte bits)) 1 -1))
        (exponent (ldb exponent-byte bits))
        (mantissa (ldb mantissa-byte bits)))
    (if (= (logcount (ldb exponent-byte bits)) (byte-size exponent-byte))
        (if (zerop mantissa)
            (if (plusp sign) 'positive-infinity 'negative-infinity)
            'not-a-number)
        (progn 
          (when (plusp exponent)
            (incf mantissa (expt 2 (byte-size mantissa-byte))))
          (if (zerop exponent)
              (setf exponent (- 1 bias (byte-size mantissa-byte)))
              (setf exponent (- (- exponent (byte-size mantissa-byte)) bias)))
          (float (* sign (* mantissa (expt 2 exponent))) 0d0)))))

(defun decode-double-float-bits (bits)
  (decode-float-bits bits (byte 1 63) (byte 11 52) (byte 52 0) 1023))
