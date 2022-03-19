;;;; 1-3-1: Procedures as Arguments
;;;; 2022/03/19

(define sum-integers (lambda (a b)
  (if (< a b)
    (+ a
       (sum-integers (1+ a) b))
    0)))
(define sum-cubes (lambda (a b)
  (if (< a b)
    (+ (cube a)
       (sum-cubes (1+ a) b))
    0)))
(define pi-sum (lambda (a b)
  (if (< a b)
    (+ (/ 1.0 (* a (+ a 2)))
       (pi-sum (+ a 4) b))
    0)))

; (define <name> (lambda (a b)
;   (if (< a b)
;     (+ (<term> a)
;        (<name> (<next> a) b))
;     0)))
(define sum (lambda (term a next b)
  (if (< a b)
    (+ (term a)
       (sum term (next a) next b))
    0)))

(define sum-integers-kai (lambda (a b)
  (sum (lambda (x) x) a 1+ b)))
(define sum-cubes-kai (lambda (a b)
  (sum cube a 1+ b)))
(define pi-sum-kai (lambda (a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ 4 x))
       b)))

(define integral (lambda (f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx)))

