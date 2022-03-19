;;;; 1-27
;;;; 1-2-6
;;;; 2022/03/19

;;; Demonstrate that the Carmichael numbers listed in footnote 47 really do fool
;;; the Fermat test. That is, write a procedure that takes an integer `n` and
;;; tests whether `a ** n` is congruent to `a` modulo `n` for every `a < n`, and
;;; try your procedure on the given Carmichael numbers.

(define check-Carmichael (lambda (Carmichael)
  (define iterator (lambda (pre-result count)
    (cond ((= count Carmichael) #t)
          ((not pre-result) #f)
          (else (iterator (Fermat-test Carmichael count) (1+ count))))))
  (iterator #t 1)))
(define Fermat-test (lambda (n a)
  (= a (expmod a n n))))
(define expmod (lambda (base exponent mod)
  (cond ((= exponent 0) 1)
        ((even? exponent) (remainder (square (expmod base (/ exponent 2) mod))
                                     mod))
        (else (remainder (* base (expmod base (-1+ exponent) mod))
                          mod)))))

(define check-example (lambda ()
  (display-line (test-Carmichael  561))
  (display-line (test-Carmichael 1105))
  (display-line (test-Carmichael 1729))
  (display-line (test-Carmichael 2465))
  (display-line (test-Carmichael 2821))
  (display-line (test-Carmichael 6601))))
(define display-line (lambda (content)
  (display content)
  (newline)))

