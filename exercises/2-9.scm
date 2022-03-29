;;;; 2-9
;;;; 2-1-4
;;;; 2022/03/29

(define half (lambda (number)
  (/ number 2)))

;;; The `width` of an interval is half of the difference between its upper and
;;; lower bounds. The width is a measure of the uncertainty of the number
;;; specified by the interval. For some arithmetic operations the width of the
;;; result of combining two intervals is a function only of the widths of the
;;; argument intervals, whereas for others the width of the combination is not a
;;; function of the widths of the argument intervals. Show that the width of the
;;; sum (or difference) of two intervals is a function only of the widths of the
;;; intervals being added (or subtracted). Give examples to show that this is
;;; not true for multiplication or division.

(load "2-8.scm")

(define width (lambda (interval)
  (define lower (lower-bound interval))
  (define upper (upper-bound interval))
  (half (- upper lower))))

(define _test (lambda (operator base-operator a b)
  (define a.width (width a))
  (define b.width (width b))
  (define base-combine.width (base-operator a.width b.width))
  (define combine (operator a b))
  (define combine.width (width combine))
  ; (display base-combine.width)
  ; (newline)
  ; (display combine.width)
  ; (newline)
  (= base-combine.width combine.width)))
(define test (lambda ()
  (define a (make-interval 1 2))
  (define b (make-interval 3 4))
  (display "+: ")
  (display (_test add-interval + a b))
  (newline)
  (display "-: ")
  (display (_test sub-interval - a b))
  (newline)
  (display "-: ")
  (display (_test sub-interval + a b))
  (newline)
  (display "*: ")
  (display (_test mul-interval * a b))
  (newline)
  (display "/: ")
  (display (_test div-interval / a b))
  (newline)
  (display "/: ")
  (display (_test div-interval * a b))
  (newline)))

