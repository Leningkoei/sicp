;;;; 3-70
;;;; 3-5-3
;;;; 2022/01/02

;;; Numbers that can be expressed as the sum of two cubes in more than one way
;;; are sometimes called `Ramanujan numbers`, in honor of the mathematician
;;; Srinivasa Ramanujan. Ordered streams of pairs provide an elegant solution to
;;; the problem of computing these numbers. To find a number that can be written
;;; as the sum of two cubes in two different ways, we need only generate the
;;; stream of pairs of integers (i, j) weighted according to the sum of i^3+j^3
;;; (see exercise 3.70), then search the stream for two consecutive pairs with
;;; the same weight. Write a procedure to generate the Ramanujan numbers. The
;;; first such number is 1,729. What are the next five?

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
(defun stream-for-each (f stream &key (begin 0) (end 'infinite))
  (if (or (stream-null? stream) (equal end 0))
      'done
      (progn
        (when (and (= begin 0) (or (equal end 'infinite) (> end 0)))
          (apply f (stream-car stream) nil))
        (stream-for-each
         f (stream-cdr stream)
         :begin (if (= begin 0) 0 (- begin 1))
         :end (if (equal end 'infinite)
                  'infinite
                  (- end 1))))))
(defun print-line (line)
  (format t "~A~%" line))
(defun display-stream (stream &key (begin 0) (end 16))
  (stream-for-each
   #'(lambda (current) (print-line current))
   stream :begin begin :end end))
(defun stream-+ (stream1 stream2)
  (stream-map #'+ stream1 stream2))
(defun stream-filter (test stream)
  (if (stream-null? stream)
      the-empty-stream
      (if (apply test (stream-car stream) nil)
          (cons-stream
           (stream-car stream)
           (stream-filter test (stream-cdr stream)))
          (stream-filter test (stream-cdr stream)))))

(defparameter ones (cons-stream 1 ones))
(defparameter one-plus (cons-stream 1 (stream-+ one-plus ones)))
(defparameter integers (cons-stream 0 one-plus))
