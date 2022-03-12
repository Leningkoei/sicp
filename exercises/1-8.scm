;;;; Newton's method for cube roots is based on the fact that if `y` is an
;;;; approximation to the cube root of `x`, then a better approximation is given
;;;; by the value
;;;; (x / y ** 2 + 2 * y) / 3
;;;; Use this formula to implement a cube-root procedure analogous to the
;;;; square-root procedure.

(define (my-cubt x)
  (cubt-iter 1.0 x))
(define (cubt-iter guess x)
  (if (good-enough? guess x)
    guess
    (cubt-iter (improve guess x) x)))
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.0001))
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

