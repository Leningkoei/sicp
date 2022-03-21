;;;; 1-3-3 Procedures as General Methods
;;;; 2022/03/21

;;; Finding roots of equations by the half-interval method

(define search (lambda (f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value) (search f neg-point midpoint))
              ((negative? test-value) (search f midpoint pos-point))
              (else midpoint)))))))

(define average (lambda (x y)
  (/ (+ x y) 2)))
(define close-enough? (lambda (x y)
  (< (abs (- x y)) 0.001)))

(define half-interval-method (lambda (f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else (error "Values are not of opposite sign" a b))))))

;;; Finding fixed points of functions

(define tolerance 0.00001)
(define fixed-point (lambda (f first-guess)
  (define close-enough? (lambda (v1 v2)
    (< (abs (- v1 v2)) tolerance)))
  (define try (lambda (guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next)))))
  (try first-guess)))

(define my-sqrt (lambda (x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0)))

