;;;; 1-13
;;;; 1-2-2
;;;; 2022/03/14

;;; Prove that `Fib(n)` is the closest integer to `phi ** n / 5 ** 0.5`, where
;;; `phi = (1 + 5 ** 0.5) / 2`. Hint: Let `psi = (1 - 5 ** 0.5) / 2`. Use
;;; induction and the definition of the Fibonacci numbers to prove that `Fib(n)
;;; = (phi ** n - psi ** n) / 5 ** 0.5`.

(define f (lambda (n)
  (<= (abs (- (fib n)
          (phi n)))
      1)))
(define phi (lambda (n)
  (/ (expt (/ (+ 1 (sqrt 5)) 2) n) (sqrt 5))))
(define fib (lambda (n)
  (define iterator (lambda (a b count)
    (if (= count n)
      b
      (iterator (+ a b) a (1+ count)))))
  (iterator 1 0 0)))

;;; (f 1000) -> #f
;;; Accuracy not enough.

(define f-kai (lambda (n)
  (<= (psi n) 5)))
(define psi (lambda (n)
  (expt (/ (- 3 (sqrt 5)) 2) n)))

;;; But (f 1000) -> Error: Floating-point underflow

