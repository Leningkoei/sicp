;;;; 3-61
;;;; 3-5-2
;;;; 2022/10/03

;;; Let `S` be a power series (exercise 3.59) whose constant term is 1. Suppose
;;; we want to find the power series `1/S`, that is, the series `X` such that `S
;;; * X = 1`. Write `S = 1 + S_R` where `S_R` is the part of `S` after the
;;; constant term. Then we can solve for `X` as follows:
;;;
;;;         S * X = 1
;;; (1 + S_R) * X = 1
;;;   X + S_R * X = 1
;;;             X = 1 - S_R * X
;;;
;;; In other words, `X` is the power series whose constant term is 1 and whose
;;; higher-order terms are given by the negative of `S_R` times `X`. Use this
;;; idea to write a procedure `invert-unit-series` that computes `1/S` for a
;;; power series `S` with constant term 1. You will need to use `mul-series`
;;; from exercise 3.60.

(defmacro cons-stream (car cdr)
  `(cons ,car (lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defun stream-null? (stream)
  (null stream))
(defparameter the-empty-stream nil)

(defun stream-map (f &rest streams)
  (if (reduce
       #'(lambda (accumulator current-stream)
           (if accumulator
               t
               (stream-null? current-stream)))
       (cons nil streams))
      the-empty-stream
      (cons-stream
       (apply f (map 'list #'stream-car streams))
       (apply #'stream-map f (map 'list #'stream-cdr streams)))))
(defun scale-stream (stream factor)
  (stream-map #'(lambda (current) (* current factor)) stream))
(defun add-stream (stream1 stream2)
  (stream-map #'+ stream1 stream2))
(defun mul-stream (stream1 stream2)
  (cons-stream
   (* (stream-car stream1) (stream-car stream2))
   (add-stream
    (scale-stream (stream-cdr stream1) (stream-car stream2))
    (mul-stream stream1 (stream-cdr stream2)))))

(defun ones ()
  (cons-stream 1 (ones)))
(defun integers ()
  (cons-stream 1 (add-stream (ones) (integers))))
(defun invert-unit-series (S)
  ;; (let* ((X (cons-stream 1 (scale-stream (mul-stream (stream-cdr S) X) -1))))
  ;;   X))
  ;; (labels
  ;;     ((X ()
  ;;        (cons-stream 1 (scale-stream (mul-stream (stream-cdr S) (X)) -1))))
  ;;   (X)))
  (flet
      ((X (X)
         (cons-stream
          1
          (scale-stream (mul-stream (stream-cdr S) (apply X X nil)) -1))))
    (X #'X)))
