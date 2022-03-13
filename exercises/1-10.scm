;;;; 1-10
;;;; 1-2-1
;;;; 2022/03/13

;;; The following procedure computes a mathematical function called Ackermann's
;;; function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;;; What are the values of the following expressions?

(A 1 10)
; (A (- 1 1) (A 1 (- 10 1)))
; (A 0 (A 1 9))
; (A 0 (A (- 1 1) (A 1 (- 9 1))))
; (A 0 (A 0 (A 1 8)))
; ...
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
; ...
; (A 0 512)
; 1024
; 2 ** 10

(A 2 4)
; (A (- 2 1) (A 2 (- 4 1)))
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2))))
; (A 1 (A 0 (A 0 4)))
; (A 1 (A 0 8))
; (A 1 16)
; 2 ** 16

(A 3 3)
; (A (- 3 1) (A 3 (- 3 1)))
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 4)
; (A 1 16)
; 2 ** 16

;;; Consider the following procedures, where `A` is the procedure defined above:

(define (f n) (A 0 n))
; 2 * n

(define (g n) (A 1 n))
; 2 ** n

(define (h n) (A 2 n))
; (A 1 (A 2 n-1))
; 2 ** (A 2 n-1)
; 2 ** 2 ** ... ** 2 ; (and (= (count 2) n) (= order <-))

(define (k n) (* 5 n n))
; 5 * (n ** 2)

;;; Give concise mathematical definitions for the functions computed by the
;;; procedures `f`, `g`, and `h` for positive integer values of `n`. For
;;; example, `(k n)` computes `5 * (n ** 2)`.

