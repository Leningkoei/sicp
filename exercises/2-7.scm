;;;; 2-7
;;;; 2-1-4
;;;; 2022/03/29

;;; Alyssa's program is incomplete because she has not specified the
;;; implementation of the interval abstraction. Here is an definition of the
;;; interval constructor:
;;; (define (make-interval a b) (cons a b))

(load "../extended-exercises/2-1-4.scm")

(define make-interval (lambda (lower upper)
  (cons lower upper)))
(define lower-bound (lambda (interval)
  (car interval)))
(define upper-bound (lambda (interval)
  (cdr interval)))

(define test (lambda ()
  (define a (make-interval 1 2))
  (define b (make-interval 3 4))
  (display (add-interval a b))
  (newline)
  (display (mul-interval a b))
  (newline)
  (display (div-interval a b))    ; (cdr b) must not 0
  (newline)))

