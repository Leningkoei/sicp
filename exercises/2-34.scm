;;;; 2-34
;;;; 2-3-3
;;;; 2022/04/08

(load "print.scm")
(define (reload) (load "2-34.scm"))

;;; Evaluating a polynomial in `x` at a given value of `x` can be formulated as
;;; an accumulation. We evaluate the polynomial
;;; a_n * x ^ n + a_(n - 1) * x ^ (n - 1) + ... + a_1 * x + a_0
;;; using a well-known algorithm called `Horner's rule`, which structures the
;;; computation as
;;; (...(a_0 * x ^ n + a_(n - 1)) * x + ... + a1) * x + a_0
;;; In other words, we start with `a_n`, multiply by `x`, add `a_(n - 1)`,
;;; multiply by `x`, and so on, until we reach `a_0`. Fill in the following
;;; template to produce a procedure that evaluates a polynomial using Horner's
;;; rule. Assume that the polynomial are arranged in a sequence, from `a_0`
;;; through `a_n`.

; (define Horner-eval (lambda (x coefficient-sequence)
;   (accumulate (lambda (this-coeff higher-terms) <??>)
;               0
;               coefficient-sequence)))

;;; For example, to compute `1 + 3x + 5x^3 + x^5` at `x = 2` you would evaluate
;;; (Horner-eval 2 (list 1 3 0 5 0 1))

(load "accumulate.scm")

(define Horner-eval (lambda (x coefficient-sequence)
  (define op (lambda (this-coeff higher-terms)
    (+ this-coeff (* x higher-terms))))
  (accumulate op 0 coefficient-sequence)))

(define (test)
  (print (Horner-eval 2 (list 1 3 0 5 0 1))))

; 1 + 3x + 5x^3 + x^5 = 1 + 3x  + 0x^2 + 5x^3 + 0x^4 + 1x^5
;                     = 1 + x(3 + 0x   + 5x^2 + 0x^3 + x^4)
;                     = 1 + x(3 + x(0  + 5x   + 0x^2 + x^3))
;                     = 1 + x(3 + x(0  + x(5  + 0x   + x^2)))
;                     = 1 + x(3 + x(0  + x(5  + x(0  + x))))

