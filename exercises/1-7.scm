;;;; 1-7
;;;; P16
;;;; The `good-enough?` test used in computing square roots will not be very
;;;; effective for finding the square roots of very small numbers. Also, in real
;;;; computers, arithmetic operations are almost always performed with limited
;;;; precision. This makes our test inadequate for very large numbers. Explain
;;;; these statements, with examples showing how the test fails for small and
;;;; large numbers. An alternative strategy for implementing a square-root
;;;; procedure that uses this kind of end test. Does this work better for small
;;;; and large numbers?

(define (good-enough? guess x)
  (< (abs (- (square guess x) x)) 0.0001))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
  (if (good-enough?-kai guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (good-enough?-kai guess x)
  (< (abs (- guess (improve guess x))) 0.0001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))

