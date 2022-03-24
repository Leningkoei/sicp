;;;; 1-3-4 Procedures as Returned Values
;;;; 2022/03/24

(define distance (lambda (a b)
  (abs (- a b))))
(define tolerance 0.00001)
(define fixed-point (lambda (f first-guess)
  (define close-enough? (lambda (v1 v2)
    (< (distance v1 v2) tolerance)))
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

(define my-sqrt (lambda (x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0)))

;;; Newton's method

(define dx 0.00001)
(define deriv (lambda (g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx))))

;;            g(x)
;; f(x) = x - -----
;;            g'(x)
(define Newtons-transform (lambda (g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x))))))
;; Get x of g(x) = 0 by the fixed point of f(x);
(define Newtons-method (lambda (g guess)
  (fixed-point (Newtons-transform g) guess)))

(define my-sqrt-kai (lambda (x)
  (Newtons-method (lambda (y) (- (square y) x)) 1.0)))

;;; Abstractions and first-class procedures

(define fixed-point-of-transform (lambda (g transform guess)
  (fixed-point (transform g) guess)))

(define my-sqrt-kai-ni (lambda (x)
  (define g (lambda (y)
    (/ x y)))
  (fixed-point-of-transform g average-damp 1.0)))

