;;;; 3-20
;;;; 3-3-1
;;;; 2022/05/26

;;; Draw environment diagrams to illustrate the evaluation of the sequence of
;;; expressions
;;; (define x (cons 1 2))
;;; (define z (cons x x))
;;; (set-car! (cdr z) 17)
;;; (car x)
;;; >> 17
