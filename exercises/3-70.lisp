;;;; 3-70
;;;; 3-5-3
;;;; 2022/12/16

;;; It would be nice to be able to generate streams in which the pairs appear in
;;; some useful order, rather than in the order that results from an `ad hoc`
;;; interleaving process. We can use a technique similar to the `merge`
;;; procedure of exercise 3.56, if we define a way to say that one pair of
;;; intgers is "less than" another. One way to do this is to define a
;;; "weighting function" W(i, j) and stipulate that (i_1, j_1) is less than
;;; (i_2, j_2) if W(i_1, j_1) < W(i_2, j_2). Write a procedure `merge-weighted`
;;; that is like `merge`, expect that `merge-weighted` takes an additional
;;; argument `weight`, which is a procedure that computes the weight of a pair,
;;; and is used to determine the order in which elements should appear in the
;;; resulting merged stream. Using this, generalize `pairs` to a procedure
;;; `weighted-pairs` that takes two streams, together with a procedure that
;;; computes a weighting function, and generates the stream of pairs, ordered
;;; according to weight. Use your procedure to generate
;;;
;;; a. the stream of all pairs of positive integers (i, j) with i <= j ordered
;;; according to the sum i + j
;;;
;;; b. the stream of all pairs of positive integers (i, j) with i <= j, where
;;; neither `i` nor `j` is divisible by 2, 3, or 5, and the pairs are ordered
;;; according to the sum 2i + 3j + 5ij.

;;; merge -- 3-56
;;; (shadow 'merge)
;;; (defun merge (stream1 stream2)
;;;   (cond
;;;     ((stream-null? stream1) stream2)
;;;     ((stream-null? stream2) stream1)
;;;     ('t
;;;      (let ((stream1-car (stream-car stream1))
;;;            (stream2-car (stream-car stream2)))
;;;        (cond
;;;          ((< stream1-car stream2-car)
;;;           (cons-stream stream1-car (merge (stream-cdr stream1) stream2)))
;;;          ((> stream1-car stream2-car)
;;;           (cons-stream stream2-car (merge stream1 (stream-cdr stream2))))
;;;          ('t
;;;           (cons-stream
;;;            stream1-car
;;;            (merge (stream-cdr stream1) (stream-cdr stream2)))))))))

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

(defun merge-weighted (stream1 stream2 &key (weight #'identity))
  (cond
    ((stream-null? stream1) stream2)
    ((stream-null? stream2) stream1)
    ('t
     (let ((weight1 (apply weight (stream-car stream1) nil))
           (weight2 (apply weight (stream-car stream2) nil)))
       (cond
         ((< weight1 weight2)
          (cons-stream
           (stream-car stream1)
           (merge-weighted (stream-cdr stream1) stream2 :weight weight)))
         ((> weight1 weight2)
          (cons-stream
           (stream-car stream2)
           (merge-weighted stream1 (stream-cdr stream2) :weight weight)))
         ('t
          (cons-stream
           (stream-car stream1)
           (cons-stream
            (stream-car stream2)
            (merge-weighted
             (stream-cdr stream1)
             (stream-cdr stream2)
             :weight weight)))))))))
(defun weighted-pairs (stream1 stream2 weight)
  (cons-stream
   (list (stream-car stream1) (stream-car stream2))
   (merge-weighted
    (stream-map
     #'(lambda (x)
         (list (stream-car stream1) x))
     (stream-cdr stream2))
    (weighted-pairs (stream-cdr stream1) (stream-cdr stream2) weight)
    :weight weight)))

(defun weight-a (pair)
  (+ (car pair) (cadr pair)))
(defun check-a ()
  (display-stream (weighted-pairs integers integers #'weight-a)))

(defun weight-b (pair)
  (let ((i (car pair)) (j (cadr pair)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))
(defun check-b ()
  (display-stream
   (stream-map
    #'(lambda (pair)
        (list pair (weight-b pair)))
    (stream-filter
     #'(lambda (pair)
         (let ((i (car pair)) (j (cadr pair)))
           (or (= (mod i 2) 0)
               (= (mod i 3) 0)
               (= (mod i 5) 0)
               (= (mod j 2) 0)
               (= (mod j 3) 0)
               (= (mod j 5) 0))))
     (weighted-pairs integers integers #'weight-b)))))
