;;;; 1-16
;;;; 1-2-4
;;;; 2022/03/15

;;; Design a procedure that evolves an iterative exponentiation process that
;;; uses successive squaring and uses a logarithmic number of steps, as does
;;; `fast-expt`. (Hint: Using the observation that `(b ** n / 2) ** 2 = (b ** 2)
;;; ** (n / 2), keep, along with the exponent `n` and the base `b`, an
;;; additional state variable `a`, and define the state transformation in such a
;;; way that the product `a` `b ** n` is unchanged from state to state. At the
;;; beginning of the process `a` is taken to be 1, and the answer is given by
;;; the value of `a` at the end of process. In general, the technique of
;;; defining an `invariant quantity` that remains unchanged from state to state
;;; is a powerful way to think about the design of iterative algorithms.)

;;; recursion
;;; b ** n = (b ** (n / 2)) ** 2    if `n` is even
;;; b ** n = b * b ** (n - 1)       if `n` is odd

(define fast-expt (lambda (b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (-1+ n)))))))

;;; iteration
;;; if `n` is even:
;;;   a * b ** n = a * (b ** 2) ** (n / 2)
;;;   a' = a, b' = b ** 2, n' = n / 2
;;; if `n` is odd:
;;;   a * b ** n = a * b * b ** (n - 1)
;;;   a' = a * b, b' = b, n' = n - 1

(define fast-expt-kai (lambda (b n)
  (define iterator (lambda (a b n)
    (cond ((= n 0) a)
          ((even? n) (iterator a (square b) (/ n 2)))
          (else (iterator (* a b) b (- n 1))))))
  (iterator 1 b n)))

;;; half-iteration half-recursion
;;; easier to understand than purl iteration and
;;; saver? to evaluate   than purl recursion ^a^

(define fast-expt-kai-ni (lambda (b n)
  (define iterator (lambda (b n)
    (cond ((= n 0) 1)
          ((even? n) (iterator (square b) (/ n 2)))
          (else (* b (iterator b (- n 1)))))))
  (iterator b n)))

; (fast-expt-kai-ni 2 4)
; (iterator 2 4)
; (iterator (square 2) (/ 4 2))
; (iterator 4 2)
; (iterator (square 4) (/ 2 2))
; (iterator 16 1)
; (* 16 (iterator 16 (- 1 1)))
; (* 16 (iterator 16 0))
; (* 16 1)
; 16
; (fast-expt-kai-ni 3 3)
; (iterator 3 3)
; (* 3 (iterator 3 2))
; (* 3 (iterator 9 1))
; (* 3 (* 9 (iterator 9 0)))
; (* 3 (* 9 1))
; (* 3 9)
; 27

