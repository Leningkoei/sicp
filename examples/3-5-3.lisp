;;;; 3-5-3: Exploiting the Stream Paradigm
;;;; 2022/10/05

(defmacro cons-stream (car cdr)
  `(cons ,car #'(lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defun stream-null? (stream)
  (null stream))
(defparameter the-empty-stream nil)

(defun stream-ref (stream index)
  (if (= index 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- index 1))))
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
(defun stream-+ (stream1 stream2)
  (stream-map #'+ stream1 stream2))
(defun filter-stream (test stream)
  (if (stream-null? stream)
      the-empty-stream
      (if (apply test (stream-car stream) nil)
          (cons-stream
           (stream-car stream)
           (filter-stream test (stream-cdr stream)))
          (filter-stream test (stream-cdr stream)))))
(defun stream-for-each (f stream &key (begin 0) (end 'infinite))
  (if (or (stream-null? stream)
          (equal end 0))
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
(defun scale-stream (stream factor)
  (stream-map #'(lambda (current) (* factor current)) stream))
(defparameter ones
  (cons-stream 1 ones))
(defparameter integers
  (cons-stream 1 (stream-+ integers ones)))

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

(defun partial-sums (stream)
  (labels
      ((partial-sums (stream sum)
         (if (stream-null? stream)
             the-empty-stream
             (let ((sum (+ sum (stream-car stream))))
               (cons-stream sum (partial-sums (stream-cdr stream) sum))))))
    (partial-sums stream 0)))
(defun pi-summands (n)
  (cons-stream (/ 1.0 n) (stream-map #'- (pi-summands (+ n 2)))))
(defparameter pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(defun square (number)
  (* number number))
(defun euler-transform (stream)
  (let ((s0 (stream-ref stream 0))
        (s1 (stream-ref stream 1))
        (s2 (stream-ref stream 2)))
    (cons-stream
     (- s2
        (/ (square (- s2 s1))
           (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr stream)))))

(defun make-tableau (transform stream)
  (cons-stream
   stream
   (make-tableau transform (apply transform stream nil))))
(defun accelerate-sequence (transform stream)
  (stream-map #'stream-car (make-tableau transform stream)))
;; (display-stream (accelerate-sequence #'euler-transform pi-stream))
;; error: 0.0 / 0.0

;;; Infinite streams of pairs
;;; 2022/10/15

(defun prime? (n)
  (labels
      ((divides? (a b)
         (= (mod b a) 0))
       (square (number)
         (* number number))
       (find-divisor (n test-divisor)
         (cond
           ((> (square test-divisor) n) n)
           ((divides? test-divisor n) test-divisor)
           (t (find-divisor n (+ test-divisor 1)))))
       (smallest-divisor (n)
         (find-divisor n 2)))
    (= n (smallest-divisor n))))

(defun stream-append (stream1 stream2)
  "It is unsuitable when stream1 is a infinite stream!!!"
  (if (stream-null? stream1)
      stream2
      (cons-stream
       (stream-car stream1)
       (stream-append (stream-cdr stream1) stream2))))
(defun interleave (stream1 stream2)
  (if (stream-null? stream1)
      stream2
      (cons-stream
       (stream-car stream1)
       (interleave stream2 (stream-cdr stream1)))))
;;; shittyt
(defun pairs (r s)
  (cons-stream
   (list (stream-car r) (stream-car s))
   (interleave
    (stream-map
     #'(lambda (x)
         (list (stream-car r) x))
     (stream-cdr s))
    (pairs (stream-cdr r) (stream-cdr s)))))
(defparameter int-pairs
  (pairs integers integers))

(defparameter target
  (filter-stream
   #'(lambda (pair)
       (prime? (+ (car pair) (cadr pair))))
   int-pairs))
