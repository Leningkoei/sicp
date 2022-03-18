;;;; 1-24
;;;; 1-2-6
;;;; 2022/03/18

;;; Modify the `timed-prime-test` procedure of exercise 1.22 to use
;;; `fast-prime?` (the Fermat method), and test each of the 12 primes you found
;;; in that exercise. Since the Fermat test has Omega(log(n)) growth, how would
;;; you expect the time to test primes near 1,000,0000 to compare with the time
;;; needed to test primes near 1000? Do your data bear this out? Can you explain
;;; any discrepancy you find?

(define timed-prime-test (lambda (n)
  (newline)
  (display n)
  (start-prime-test n (runtime))))
(define start-prime-test (lambda (n start-time)
  (if (fast-prime? n 10)
    (report-prime (- (runtime) start-time))
    #f)))
(define report-prime (lambda (elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t))

(define fast-prime? (lambda (n times)
  (cond ((= 0 times) #t)
        ((Fermat-test n) (fast-prime? n (-1+ times)))
        (else #f))))
(define Fermat-test (lambda (n)
  (try-it (random-from-1-to-n n) n)))
(define try-it (lambda (a n)
  (= a (expmod a n n))))
(define expmod (lambda (base exponent mod)
  (cond ((= exponent 0) 1)
        ((even? exponent) (remainder (square (expmod base (/ exponent 2) mod))
                                     mod))
        (else (remainder (* base (expmod base (-1+ exponent) mod))
                         mod)))))
(define random-from-1-to-n (lambda (n)
  (1+ (random (-1+ n)))))

