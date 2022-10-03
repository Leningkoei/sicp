;;;; 3-60
;;;; 3-5-2
;;;; 2022/10/02

;;; With power series represented as streams of coefficients as in exercise
;;; 3.59, adding series is implemented by `add-streams`. Complete the definition
;;; of the following procedure for multiplying series:
;;;
;;; (define (mul-series s1 s2)
;;;   (cons-stream <??> (add-streams <??> <??>)))
;;;
;;; You can test your procedure by verifying that `sin^2(x) + cos^2(x) = 1,
;;; using the series from exercise 3.59.

(defmacro cons-stream (car cdr)
  `(cons ,car (lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defun stream-null? (stream)
  (null stream))
(defparameter the-empty-stream nil)

(defun stream-map (f stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream
       (apply f (stream-car stream) nil)
       (stream-map f (stream-cdr stream)))))
(defun scale-stream (stream factor)
  (stream-map #'(lambda (current) (* current factor)) stream))
(defun add-stream (stream1 stream2)
  (stream-map #'+ stream1 stream2))

(defun mul-series (stream1 stream2)
  (cons-stream
   (* (stream-car stream1) (stream-car stream2))
   (add-stream
    (scale-stream (stream-cdr stream1) (stream-car stream2))
    (mul-series stream1 (stream-cdr stream2)))))

;;; test
;; (defparameter sine-power (mul-series sine-series sine-series))
;; (defparameter cosine-power (mul-series cosine-series cosine-series))
;; (defparameter sum
;;   (apply #'+ (slice-stream :end 5 (add-stream sine-power cosine-power))))
;; ;; 14177/14175 == 1
