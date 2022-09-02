;;;; 3-58
;;;; 3-5-2
;;;; 2022/09/02

;;; Given an interpretation of the stream computed by the following procedure:

;; (defun expand (num den radix)
;;   (cons-stream
;;    (quotient (* num radix) den)
;;    (expand (remainder (* num radix) den) den radix)))

;;; (`quotient` is a primitive that returns the integer quotient of two
;;; integers.) What are the successive elements produced by `(expand 1 7 10)`?
;;; What is produced by `(expand 3 8 10)`?

(defmacro cons-stream (car cdr)
  `(cons ,car (lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defun stream-null? (stream)
  (null stream))
(defparameter the-empty-stream nil)

(defun integer-division (divident divisor)
  (multiple-value-bind (quotient remainder)
      (floor divident divisor)
    (cons quotient remainder)))
(defun quotient (divident divisor)
  (car (integer-division divident divisor)))
(defun remainder (divident divisor)
  (cdr (integer-division divident divisor)))

(defun expand (num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
