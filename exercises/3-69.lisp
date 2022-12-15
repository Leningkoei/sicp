;;;; 3-69
;;;; 3-5-3
;;;; 2022/12/15

;;; Write a procedure `triples that takes three infinite streams, S, T, and U,
;;; and produces the stream of triples (S_i, T_j, U_k) such that i <= j <= k.
;;; Use `triples` to generate the stream of all Pythagorean triples of positive
;;; integers, i.e., the triples (i, j, k) such that i <= j and i^2 + j^2 = k^2.

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

(defparameter ones (cons-stream 1 ones))
(defparameter one-plus (cons-stream 1 (stream-+ one-plus ones)))
(defparameter integers (cons-stream 0 one-plus))

(defun interleave (stream1 stream2)
  (if (stream-null? stream1)
      stream2
      (cons-stream
       (stream-car stream1)
       (interleave stream2 (stream-cdr stream1)))))
(defun pairs (stream1 stream2)
  (cons-stream
   (list (stream-car stream1) (stream-car stream2))
   (interleave
    (stream-map
     #'(lambda (x)
         (list (stream-car stream1) x))
     (stream-cdr stream2))
    (pairs (stream-cdr stream1) (stream-cdr stream2)))))
(defun triples (stream1 stream2 stream3)
  (cons-stream
   (list (stream-car stream1) (stream-car stream2) (stream-car stream3))
   (interleave
    (stream-map
     #'(lambda (x) (cons (stream-car stream1) x))
     (stream-cdr (pairs stream2 stream3)))
    (triples (stream-cdr stream1) (stream-cdr stream2) (stream-cdr stream3)))))

(defun main ()
  (display-stream (triples integers integers integers)))
