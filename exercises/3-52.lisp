;;;; 3-52
;;;; 3-5-1
;;;; 2022/08/28

;;; Consider the sequence of expressions

(defparameter sum 0)
(defun accum (x)
  (setf sum (+ x sum))
  sum)
(defparameter seq (stream-map #'accum (stream-enumerate-interval 1 20)))
(defparameter y (stream-filter #'evenp seq))
(defparameter z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
(stream-ref y 7)
(display-stream z)

;;; What is the value of `sum` after each of the above expressions is evaluated?
;;; What is the printed response to evaluating the `stream-ref` and
;;; `display-stream` expressions? Would these responses differ if we had
;;; implemented `(delay <exp>)` simply as `(lambda () <exp>)` without using the
;;; optimization provided by `memo-proc`? Explain.

(defun remainder (divident divisor)
  (- divident (* divisor (floor (/ divident divisor)))))

(defmacro cons-stream (car cdr)
  `(cons ,car (lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defparameter the-empty-stream nil)
(defun stream-null? (stream)
  (null stream))

(defun stream-enumerate-interval (begin end)
  (if (= begin end)
      the-empty-stream
      (cons-stream begin (stream-enumerate-interval (+ begin 1) end))))
(defun stream-map (f stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream
       (apply f (list (stream-car stream)))
       (stream-map f (stream-cdr stream)))))
(defun stream-filter (test stream)
  (if (stream-null? stream)
      the-empty-stream
      (if (apply test (list (stream-car stream)))
          (cons-stream
           (stream-car stream)
           (stream-filter test (stream-cdr stream)))
          (stream-filter test (stream-cdr stream)))))
(defun stream-ref (stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))
(defun stream-for-each (f stream)
  (if (stream-null? stream)
      the-empty-stream
      (progn
        (apply f (list (stream-car stream)))
        (stream-for-each f (stream-cdr stream)))))
(defun display-stream (stream)
  (stream-for-each #'print stream))

;;; display: 15 180 230 305 (without cache)
