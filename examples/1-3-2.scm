;;;; 1-3-2 Constructing Procedures Using Lambda
;;;; 2022/03/21

(define reload (lambda ()
  (load "1-3-2.scm")))

;;; Using `let` to create local variables

;;; f(x, y) = x(1 + x y) ** 2 + y(1 - y) + (1 + x y)(1 - y)
;;; a = 1 + x y
;;; b = 1 - y
;;; f(x, y) = x(a ** 2) + y b + a b

(define f (lambda (x y)
  (define f-helper (lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b))))
  (f-helper (+ 1 (* x y))
            (- 1 y))))
(define f-kai (lambda (x y)
  ((lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b))) (+ 1 (* x y))
                 (- 1 y))))
(define f-kai-ni (lambda (x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b)))))
(define f-kai-ni-kou (lambda (x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b))))

