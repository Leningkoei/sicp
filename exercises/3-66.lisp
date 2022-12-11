;;;; 3-66
;;;; 3-5-3
;;;; 2022/12/11

;;; Examine the stream `(pairs integers integers)`. Can you make any general
;;; comments about the order in which the pairs are placed into the stream? For
;;; example, about how many pairs precede the pair (1, 100)? the pair (99, 100)?
;;; (If you can make precise mathematical statements here, all the better. But
;;; feel free to give more qualitative answers if you find yourself getting
;;; bogged down.)

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
(defun pairs (r s)
  (cons-stream
   (list (stream-car r) (stream-car s))
   (interleave
    (stream-map
     #'(lambda (x)
         (list (stream-car r) x))
     (stream-cdr s))
    (pairs (stream-cdr r) (stream-cdr s)))))

(defun count-1-100 ()
  (display-stream (pairs integers integers) :end 397))

;;; (1, 100) is 397th element

;;; start ~ end : total
;;;     0 ~ 198 :  199
;;;     1 ~ 100 :  100
;;;     2 ~ 5 1 :  5 0
;;;     3 ~ 2 7 :  1 5
;;;     4 ~ 1 5 :  1 2
;;;     5 ~  10 :   6
;;;     6 ~  8  :   3
;;;     7 ~  8  :   2
;;;     8 ~  x  :   0
