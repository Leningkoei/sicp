;;;; 1-4
;;;; 1-1-6
;;;; 2022/03/12

;;; Observe that our model of evaluation allows for combinations whose
;;; operators are compound expressions. Use this observation to describe the
;;; behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

