;;;; 3-55
;;;; 3-5-2
;;;; 2022/08/30

;;; Define a procedure `partial-sums` that takes as argument a stream `S` and
;;; returns the stream whose elements are `S_0`, `S_0 + S_1`, `S_0 + S_1 + S_2`,
;;; .... For example, `(partial-sums integers)` should be the stream 1, 3, 6,
;;; 10, 15, ....

(defun partial-sum (list sum)
  (if (null list)
      nil
      (let ((sum (+ sum (car list))))
        (cons sum (partial-sum (cdr list) sum)))))

(defmacro cons-stream (car cdr)
  `(cons ,car (lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defun stream-null? (stream)
  (null stream))
(defparameter the-empty-stream nil)

(defun stream-map (f &rest streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (cons-stream
       (apply f (map 'list #'stream-car streams))
       (apply #'stream-map (cons f (map 'list #'stream-cdr streams))))))
(defparameter ones (cons-stream 1 ones))
(defun add-streams (&rest streams)
  (apply #'stream-map (cons #'+ streams)))
(defparameter integers (cons-stream 1 (add-streams ones integers)))

(defun partial-sum (stream)
  (labels
      ((partial-sum (stream sum)
         (if (stream-null? stream)
             the-empty-stream
             (let ((sum (+ sum (stream-car stream))))
               (cons-stream sum (partial-sum (stream-cdr stream) sum))))))
    (partial-sum stream 0)))
