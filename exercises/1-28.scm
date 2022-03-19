;;;; 1-28
;;;; 1-2-6
;;;; 2022/03/19

;;; One variant of the Fermat test that cannot be fooled is called the `Miller-
;;; Rabin` test (Miller 1976; Rabin 1980). This starts from an alternate form of
;;; Fermat's Little Theorem, which states that if `n` is a prime number and `a`
;;; is any positive integer less than `n`, then `a` raised to the `(n - 1)`st
;;; power modulo `n` using the `expmod` procedure. However, whenever we perform
;;; the squaring step in `expmod`, we check to see if we have discovered a
;;; "nontrivial square root of 1 modulo n", that is, a number not equal to `1`
;;; or `n - 1` whose square is equal to `1` modulo `n`. It is possible to prove
;;; that if such a nontrivial square root of `1` exists, then `n` is not prime.
;;; It is also possible to prove that if `n` is an odd number that is not prime,
;;; then, for at least half the numbers `a < n`, computing `a ** (n - 1)` in
;;; this way will reveal a nontrivial square root of `1` modulo `n`. (This is
;;; why the Miller-Rabin test cannot be fooled.) Modify the `expmod` procedure
;;; to signal if it discovers a nontrivial square root of `1`, and use this to
;;; implement the Miller-Rabin test with a procedure analogous to `Fermat-test`.
;;; Check your procedure by testing various known primes and non-primes. Hint:
;;; One convenient way to make `expmod` signal is to have it return 0.

(define expmod (lambda (base exponent mod)
  (cond ((= 0 exponent) 1)
        ((even? exponent) (remainder (square (expmod base (/ exponent 2) mod))
                                     mod))
        (else (remainder (* base (expmod base (-1+ exponent) mod))
                         mod)))))

(define Miller-Rabin-test (lambda (n)
  (define try-it (lambda (a)
    (= 1 (expmod-checked a (-1+ n) n))))
  (try-it (1+ (random (-1+ n))))))
(define expmod-checked (lambda (base exponent mod)
  (cond ((= 0 exponent) 1)
        ((even? exponent)
          (remainder-square-checked (expmod-checked base (/ exponent 2) mod)
                                    mod))
        (else
          (remainder (* base (expmod-checked base (-1+ exponent) mod)) mod)))))
(define remainder-square-checked (lambda (x mod)
  (if (and (not (or (= x 1)
                    (= x (-1+ mod))))
           (= 1 (remainder (square x) mod)))
    0
    (remainder (square x) mod))))

