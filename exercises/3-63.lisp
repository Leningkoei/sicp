;;;; 3-63
;;;; 3-5-3
;;;; 2022/10/12

;;; Louis Reasoner asks why the `sqrt-stream` procedure was not written in the
;;; following more straightforward way, without the local variable guesses:
;;;
;;; (defun sqrt-stream (x)
;;;   (cons-stream
;;;    1.0
;;;    (stream-map
;;;     #'(lambda (guess)
;;;         (sqrt-improve guess x))
;;;     (sqrt-stream x))))
;;;
;;; Alyssa P. Hacker replies that this version of the procedure is considerably
;;; less efficient because it performs redundant computation. Explain Alyssa's
;;; answer. Would the two versions still differ in efficiency if our
;;; implementation of `delay` used only `(lambda () <exp>)` without using the
;;; optimization provided by `memo-proc` (section 3.5.1)?

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
  (print nil)
  (average guess (/ x guess)))
(defun sqrt-stream (x)
  (cons-stream
   1.0
   (stream-map
    #'(lambda (guess)
        (sqrt-improve guess x))
    (sqrt-stream x))))
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

;;; (improve^0 improve^1 improve^2 improve^3 improve^4 ...)
;;; (1.0       1.5       1.4166667 1.4142157 ...)
;;; They are same without memo.
