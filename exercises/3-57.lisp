;;;; 3-57
;;;; 3-5-2
;;;; 2022/08/31

;;; How many additions are performed when we compute the `n`th Fibonacci number
;;; using the definition of `fibs` based on the `add-streams` procedure? Show
;;; that the number of additions would be exponentially greater if we had
;;; implemented `(delay <exp>)` simply as `(lambda () <exp>)`, without using the
;;; optimization provided by the `memo-proc` procedure described in section
;;; 3.5.1.

(defmacro cons-stream (car cdr)
  `(cons ,car (lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defmacro stream-null? (stream)
  `(null ,stream))
(defparameter the-empty-stream nil)

(defun add (a b)
  (print '+)
  (+ a b))
(defun fibgen (a b)
  (cons-stream a (fibgen b (add a b))))
(defparameter fibs (fibgen 0 1))

;;; Just greater linely?

(defun stream-map (f &rest streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (cons-stream
       (apply f (map 'list #'stream-car streams))
       (apply #'stream-map (cons f (map 'list #'stream-cdr streams))))))
(defun add-streams (&rest streams)
  (apply #'stream-map (cons #'add streams)))
(defparameter fibs
  (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

;;; index    0  0  1  2  3  4          n
;;; fibs     0  1  1  2  3  5        ...
;;; + count  0  0  1  3  7 15  2^(n - 1)
