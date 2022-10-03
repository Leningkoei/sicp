;;;; 3-62
;;;; 3-5-2
;;;; 2022/10/03

;;; Use the results of exercises 3.60 and 3.61 to define a procedure
;;; `div-series` that divides two power series. `div-series` should work for
;;; any two series, provided that the denominator series begins with a nonzero
;;; constant term. (If the denominator has a zero constant term, then
;;; `div-series` should signal an error.) Show how to use `div-series` together
;;; with the result of exercise 3.59 to generate the power series for tangent.

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
    (mul-series stream1 (stream-cdr stream2)))))
(defun invert-unit-series (S)
  (flet
      ((X (X)
         (cons-stream
          1
          (scale-stream (mul-stream (stream-cdr S) (apply X X nil)) -1))))
    (X #'X)))

(defun div-series (stream1 stream2)
  (if (= (stream-car stream2) 0)
      (error (format t "There exists 0 in stream2!!!"))
      (mul-stream stream1 (invert-unit-series stream2))))
