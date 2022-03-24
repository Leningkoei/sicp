;;;; 1-40
;;;; 1-3-4
;;;; 2022/03/24

;;; Define a procedures `cubic` that can be used together with the `Newtons-
;;; method` procedure in expressions of the form
;;; (Newtons-method (cubic a b c) 1)
;;; to approximate zeros of the cubic `x ** 3 + a x ** 2 + b x + c`.

(define dx 0.00001)
(define deriv (lambda (g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx))))
(define Newtons-transform (lambda (g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x))))))

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

(define Newtons-method (lambda (g guess)
  (fixed-point (Newtons-transform g) guess)))

(define cubic (lambda (a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c))))

