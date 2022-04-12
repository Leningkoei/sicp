;;;; 1-06
;;;; 1-1-7

;;; Alyssa P. Hacker doesn't see why `if` needs to be provided as a special
;;; form. "Why can't I just define it as an ordinary procedure in terms of
;;; `cond`?" she asks. Alyssa's friend Eva Lu Ator claims this can indeed be
;;; done, and she defines a new version of `if`:

(define (new-if predicate then-clause else-clause) ; Applicative-order Evaluation.
  (cond (predicate then-clause)
        (else else-clause)))

;;; Eva demonstrates the program for Alyssa:

;;; Delighted, Alyssa uses `new-if` to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))              ; Evaluated Before Predicate. So Never Stop.

;; predicate: Boolean,
;; then-clause: Function<type>,
;; else-clause: Function<type>
;; type
(define new-if-kai (lambda (predicate then-clause else-clause)
  (cond (predicate (then-clause))
        (else (else-clause)))))
(define sqrt-iter-kai (lambda (guess x)
  (new-if-kai (good-enough? guess x) (lambda () guess)
    (lambda () (sqrt-iter-kai (improve guess x) x)))))

(define good-enough? (lambda (guess x)
  (< (abs (- (square guess) x)) 0.001)))
(define improve (lambda (guess x)
  (average guess (/ x guess))))
(define average (lambda (x y)
  (/ (+ x y) 2)))

