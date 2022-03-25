;;;; 1-45
;;;; 1-4-3
;;;; 2022/03/25

;;; We saw in section 1.3.3 that attempting to compute square roots by naively
;;; finding a fixed point of `y -> x / y` does not converge, and that this can
;;; be fixed by average damping. The same method works for finding cube roots as
;;; fixed points of the average-damped `y -> x / y ** 2`. Unfortunately, the
;;; process does not work for fourth roots -- a single average damp is not
;;; enough to make a fixed-point search for `y -> x / y ** 3` converge. On the
;;; other hand, if we average damp twice (i.e., use the average damp of the
;;; average damp of `y -> x / y ** 3`) the fixed-point search does converge. Do
;;; some experiments to determine how many average damps are required to compute
;;; `n`th roots using `fixed-point`, `average-damp`, and the `repeated`
;;; procedure of exercise 1.43. Assume that any arithmetic operations you need
;;; are available as primitives.

(define tolerance 0.00001)
(define distance (lambda (a b)
  (abs (- a b))))
(define close-enough? (lambda (a b)
  (< (distance a b) tolerance)))
(define fixed-point (lambda (f first-guess)
  (define try (lambda (guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next)))))
  (try first-guess)))

(define average (lambda (a b)
  (/ (+ a b) 2)))
(define average-damp (lambda (f)
  (lambda (x)
    (average x (f x)))))

(define compose (lambda (f g)
  (lambda (x)
    (f (g x)))))
(define repeated (lambda (f count)
  (define iterator (lambda (i result)
    (if (< i count)
      (iterator (1+ i) (compose f result))
      result)))
  (define starter (lambda (x) x))
  (iterator 0 starter)))

(define nth-average-damp (lambda (f n)
  ((repeated average-damp n) f)))

(define x-nth-root (lambda (x n)
  (define g (lambda (y)
    (define deno (expt y (-1+ n)))
    (/ x deno)))
  (define n-2-th-average-damp (nth-average-damp g (-1+ n)))
  (* 1.0 (fixed-point n-2-th-average-damp 1))))

(define my-sqrt (lambda (x)
  (x-nth-root x 2)))
(define my-curt (lambda (x)
  (x-nth-root x 3)))

