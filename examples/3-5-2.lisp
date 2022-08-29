;;;; 3-5-2: Infinite Stream
;;;; 2022/08/29

(defmacro cons-stream (car cdr)
  `(cons ,car (lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defparameter the-empty-stream nil)
(defun stream-null? (stream)
  (null stream))


(defun integers-starting-from (n)
  (cons-stream n (integers-starting-from (+ n 1))))
(defparameter integers (integers-starting-from 1))


(defun remainder (divident divisor)
  (- divident (* divisor (floor (/ divident divisor)))))
(defun divisible? (divident divisor)
  ;; (format 't "testing ~A~%" divisor)
  (= (remainder divident divisor) 0))
(defun stream-filter (test stream)
  (cond
    ((stream-null? stream) the-empty-stream)
    ((apply test (list (stream-car stream)))
     (cons-stream
      (stream-car stream)
      (stream-filter test (stream-cdr stream))))
    ('t (stream-filter test (stream-cdr stream)))))
(defparameter sevens
  (stream-filter
   #'(lambda (current) (divisible? current 7))
   (integers-starting-from 1)))


(defun fibgen (a b)
  (cons-stream a (fibgen b (+ a b))))
(defparameter fibs (fibgen 0 1))


(defun stream-ref (stream n)
  (if (= 0 n)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))
(defun sieve (stream)
  (cons-stream
   (stream-car stream)
   (sieve
    (stream-filter
     #'(lambda (x)
         (not (divisible? x (stream-car stream))))
     (stream-cdr stream)))))
(defparameter primes (sieve (integers-starting-from 2)))


;;; Defining streams implicitly

(defparameter ones (cons-stream 1 ones))


(defun stream-enumerate-interval (begin end)
  (if (= begin end)
      the-empty-stream
      (cons-stream begin (stream-enumerate-interval (+ 1 begin) end))))
(defun stream-map (f &rest streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (cons-stream
       (apply f (map 'list #'stream-car streams))
       (apply #'stream-map (cons f (map 'list #'stream-cdr streams))))))
(defun add-streams (&rest streams)
  (apply #'stream-map (cons #'+ streams)))
(defparameter integers-kai (cons-stream 1 (add-streams ones integers)))
(defparameter fibs-kai
  (cons-stream 0 (cons-stream 1 (add-streams fibs-kai (stream-cdr fibs-kai)))))

(defun scale-stream (stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(defparameter double (cons-stream 1 (scale-stream double 2)))

(defun square (x)
  (* x x))
(defparameter primes
  (cons-stream 2 (stream-filter #'prime? (integers-starting-from 3))))
(defun prime? (n)
  (labels
      ((iterator (rest-primes)
         (cond
           ((> (square (stream-car rest-primes)) n) 't)
           ((divisible? n (stream-car rest-primes)) nil)
           ('t (iterator (stream-cdr rest-primes))))))
    (iterator primes)))
