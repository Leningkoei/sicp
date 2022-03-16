;;;; 1-20
;;;; 1-2-5
;;;; 2022/03/16

;;; The process that a procedure generates is of course dependent on the rules
;;; used by the interpreter. As an example, consider the iterative `gcd`
;;; procedure given above. Suppose we were to interpret this procedure using
;;; normal-order evaluation, as discussed in section 1.1.5. (The normal-order-
;;; evaluation rule for `if` is described in exercise 1.5.) Using the
;;; substitution method (for normal order), illustrate the process generated in
;;; evaluating `(gcd 206 40)` and indicate the `remainder` operations that are
;;; actually performed. How many `remainder` operations are actually performed
;;; in the normal-order evaluation of (gcd 206 40)? In the applicative-order
;;; evaluation?

(define my-gcd (lambda (a, b)
  (if (= b 0)
    a
    (my-gcd b (remainder a b)))))

;;; applicative order
; (my-gcd 206 40)
; (my-gcd 40  6)
; (my-gcd 6   4)
; (my-gcd 4   2)
; (my-gcd 2   0)
; 2

; ;;; normal order
; ; b = 40 count = 0
; (my-gcd 206 40)
; ; b = 6  count = 1
; (my-gcd 40 (remainder 206 40))
; ; b = 4  count = 2
; (my-gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; ; b = 2  count = 4
; (my-gcd (remainder 40 (remainder 206 40))
;         (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; ; b = 0  count = 7
; ; (my-gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; ;         (remainder (remainder 40 (remainder 206 40))
; ;                    (remainder (remainder 206 40)
; ;                               (remainder 40 (remainder 206 40)))))
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; ; total = (+ 1 2 4 7 4) = 18

