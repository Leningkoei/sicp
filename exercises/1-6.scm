;;;; 1-6
;;;; P16
;;;; Alyssa P. Hacker doesn't see why `if` needs to be provided as a special
;;;; form. "Why can't I just define it as an ordinary procedure in terms of
;;;; `cond`?" she asks. Alyssa's friend Eva Lu Ator claims this can indeed be
;;;; done, and she defines a new version of `if`:

(define (new-if predicate then-clause else-clause) ; Applicative-order Evaluation.
  (cond (predicate then-clause)
        (else else-clause)))

;;;; Eva demonstrates the program for Alyssa:

;;;; Delighted, Alyssa uses `new-if` to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))              ; Evaluated Before Predicate. So Never Stop.

