;;;; 1-25
;;;; 1-2-6
;;;; 2022/03/18

;;; Alyssa P. Hacker complains that we went to a lot of extra work in writing
;;; `expmod`. After all, she says, since we already know how to compute
;;; exponentials, we could have simply written

(define expmod (lambda (base exponent mod)
  (remainder (fast-expt base exponent) mod)))

;;; Is she correct? Would this procedure serve as well for our fast prime
;;; tester? Explain.

(define fast-expt (lambda (base exponent)
  (cond ((= 0 exponent) 1)
        ((even? exponent) (square (fast-expt base (/ exponent 2))))
        (else (* base (fast-expt base (1- exponent)))))))

(define better-expmod (lambda (base exponent mod)
  (cond ((= exponent 0) 1)
        ((even? exponent) (remainder (square (better-expmod base
                                                            (/ exponent 2)
                                                            mod))
                                     mod))
        (else (remainder (* base (better-expmod base (-1+ exponent) mod))
                         mod)))))

;;; First `expmod`'s fast-expt will create a very long number.

