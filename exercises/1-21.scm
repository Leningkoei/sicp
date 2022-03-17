;;;; 1-21
;;;; 1-2-6
;;;; 2022/03/17

;;; Use the `small-divisor` procedure to find the smallest divisor of each of
;;; the following numbers: 199, 1999, 19999.

(define smallest-divisor (lambda (n)
  (find-divisor n 2)))
(define find-divisor (lambda (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))))
(define divides? (lambda (a b)    ; a is b's divides?
  (= (remainder b a) 0)))

(define f (lambda ()
  (display-line (smallest-divisor 199))
  (display-line (smallest-divisor 1999))
  (display-line (smallest-divisor 19999))))
(define display-line (lambda (content)
  (display content)
  (newline)))

