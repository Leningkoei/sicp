;;;; 3-65
;;;; 3-5-3
;;;; 2022/10/14

;;; Use the series
;;;              1     1     1
;;; ln(2) = 1 - --- + --- - --- + ...
;;;              2     3     4
;;; to compute three sequences of approximations to the natural logarithm of 2,
;;; in the same way we did above for `pi`. How rapidly to these sequences
;;; converge?

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
;; (defun partical-sums (stream)
;;   (labels
;;       ((partical-sum (stream sum)
;;          (if (stream-null? stream)
;;              the-empty-stream
;;              (let ((sum (+ sum (stream-car stream))))
;;                (cons-stream sum (partial-sum (stream-cdr stream) sum))))))
;;     (partial-sum stream 0)))
(defun reduce-stream (f stream)
  (labels
      ((reduce-stream (accumulator stream)
         (if (stream-null? stream)
             the-empty-stream
             (let ((accumulator (apply f accumulator (stream-car stream) nil)))
               (cons-stream
                accumulator
                (reduce-stream accumulator (stream-cdr stream)))))))
    (reduce-stream (stream-car stream) (stream-cdr stream))))

(defparameter ln-2
  (labels
      ((ln-2-summands (n)
         (cons-stream
          (/ 1.0 n)
          (stream-map #'- (ln-2-summands (+ n 1))))))
    (reduce-stream #'+ (ln-2-summands 1))))
