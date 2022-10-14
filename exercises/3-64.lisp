;;;; 3-64
;;;; 3-5-3
;;;; 2022/10/14

;;; Write a procedure `stream-limit` that takes as arguments a stream and a
;;; number (the tolerance). It should examine the stream until it finds two
;;; successive elements that differ in absolute value by less than the
;;; tolerance, and return the second of the two elements. Using this, we could
;;; compute square roots up to a given tolerance by
;;;
;;; (defun sqrt (x tolerance)
;;;   (stream-limit (sqrt-stream x) tolerance))

(defmacro cons-stream (car cdr)
  `(cons ,car #'(lambda () ,cdr)))
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

(defun average (a b)
  (/ (+ a b) 2))
(defun sqrt-improve (guess x)
  (average guess (/ x guess)))
(defun sqrt-stream (x)
  (labels
      ((guesses ()
         (cons-stream
          1.0
          (stream-map
           #'(lambda (guess)
               (sqrt-improve guess x))
           (guesses)))))
    (guesses)))
(defun stream-limit (stream tolerance)
  (let ((a (stream-car stream))
        (b (stream-car (stream-cdr stream))))
    (if (<= (abs (- a b)) tolerance)
        b
        (stream-limit (stream-cdr stream) tolerance))))
(shadow 'sqrt)
(defun sqrt (x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
