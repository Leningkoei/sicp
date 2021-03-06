;;;; 1-05
;;;; 1-1-6
;;;; 2022/03/12

;;; Ben Bitdiddle has invented a test to determine whether the interpreter he
;;; is faced with is using applicative-order evaluation or normal-order
;;; evaluation. He defines the following two procedures:

(define (p)
  (p))

(define (test x y)
  (if (= x 0)
    0
    y))

;;; Applicative-order Evaluation: Evaluated when it is a parameter.
;;; Normal-order      Evaluation: Lazy mode. Possible evaluating one more times.

