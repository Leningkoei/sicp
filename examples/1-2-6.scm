;;;; 1-2-6 Test for Promality
;;;; 2022/03/17

;;; Searching for divisors

(define prime? (lambda (n)
  (= n (smallest-divisor n))))

;; Find the smallest integral divisor (greater than 1) of a given number `n`.
(define smallest-divisor (lambda (n)
  (find-divisor n 2)))

;; Exhaustion test-divisor from 2.
(define find-divisor (lambda (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))))
(define divides? (lambda (a b)
  (= (remainder b a) 0)))

;;; The Fermat test

;; Compute the exponential of a number modulo another number.
;; (x * y) % m = ((x % m) * (y % m)) % m
(define expmod (lambda (base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m)))))

(define Fermat-test (lambda (n)
  (define try-it (lambda (a)
    (= a (expmod a n n))))
  ; (define my-random (lambda ()
  ;   (+ 1 (random (- n 1)))))
  ; (try-it (my-random))))
  (try-it (+ 1 (random (- n 1))))))

(define fast-prime? (lambda (n times)
  (cond ((= times 0) true)
  ((Fermat-test n) (fast-prime? n (-1+ times)))
  (else #f))))

