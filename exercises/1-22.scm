;;;; 1-22
;;;; 1-2-6
;;;; 2022/03/17

;;; Most Lisp implementations include a primitive called `runtime` that returns
;;; an integer that specifies the amount of time the system has been running
;;; (measured, for example, in microseconds). The following `timed-prime-test`
;;; procedure, when called with an integer `n`, prints `n` and checks to see if
;;; `n` is prime. If `n` is prime, the procedure prints three asterisks followed
;;; by the amount of time used in performing the test.

(define timed-prime-test (lambda (n)
  (newline)
  (display n)
  (start-prime-test n (runtime))))
(define start-prime-test (lambda (n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))
    #f)))    ; Different with the source.
(define report-prime (lambda (elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t))    ; Different with the source.

(define prime? (lambda (n)
  (= (smallest-divisor n) n)))
(define smallest-divisor (lambda (n)
  (find-divisor n 2)))
(define find-divisor (lambda (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divisor? test-divisor n) test-divisor)
        (else (find-divisor n (1+ test-divisor))))))
(define divisor? (lambda (a b)
  (= 0 (remainder b a))))

;;; Using this procedure, write a procedure `search-for-primes` that checks the
;;; primality of consecutive odd integers in a specified range. Use your
;;; procedure to find the three smallest primes larger than 1000; larger than
;;; 10,000; larger than 1,000,000. Note the time needed to test each prime.
;;; Since the testing algorithm has order of growth of `Omega(n ** 0.5)`, you
;;; should expect that testing for primes around 10,000 should take about
;;; `10 ** 0.5` times as long as testing for primes around 1000. Do your timing
;;; data bear this out? How well do the data for 100,000 and 1,000,000 support
;;; the `n ** 0.5` prediction? Is your result compatible with the notion that
;;; programs on your machine run in time proportional to the number of steps
;;; required for the computation?

(define search-for-primes (lambda (min-number max-count)
  (cond ((= max-count 0))
        ((timed-prime-test min-number) (search-for-primes (1+  min-number)
                                                          (-1+ max-count)))
        (else (search-for-primes (1+ min-number) max-count)))))

