;;;; 2-2
;;;; 2-1-2
;;;; 2022/03/26

;;; Consider the problem of representing line segments in a plane. Each segment
;;; is represented as a pair of points: a string point and an ending point.
;;; Define a constructor `make-segment` that define the representation of
;;; segments in terms of points. Furthermore, a point can be represented as a
;;; pair of numbers: the `x` coordinate and they coordinate.
;;; Accordingly, specify a constructor `make-point`and selectors `x-point` and
;;; `y-point` that define this representation. Finally, using your selectors and
;;; constructors, define a procedure `midpoint-segment` that makes a line
;;; segment as argument and returns its midpoint (the point whose coordinates
;;; are the average of the coordinates of the endpoints). To try your
;;; procedures, you'll need a way to print points:

(define print-point (lambda (p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline)))

(define make-segment (lambda (start-point end-point)
  (cons start-point end-point)))
(define start-point (lambda (segment)
  (car segment)))
(define end-point (lambda (segment)
  (cdr segment)))

(define make-point (lambda (x-point y-point)
  (cons x-point y-point)))
(define x-point (lambda (point)
  (car point)))
(define y-point (lambda (point)
  (cdr point)))

(define average (lambda (x y)
  (/ (+ x y) 2)))
(define midpoint (lambda (a b)
  (define a-x (x-point a))
  (define a-y (y-point a))
  (define b-x (x-point b))
  (define b-y (y-point b))
  (define x (average a-x b-x))
  (define y (average a-y b-y))
  (make-point x y)))
(define midpoint-segment (lambda (segment)
  (define a (start-point segment))
  (define b (end-point   segment))
  (midpoint a b)))

(define test (lambda ()
  (define a (make-point 0 0))
  (define b (make-point 2 2))
  (define ab (make-segment a b))
  (print-point (midpoint-segment ab))))

