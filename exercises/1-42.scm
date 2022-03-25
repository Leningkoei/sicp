;;;; 1-42
;;;; 1-3-4
;;;; 2022/03/25

;;; Let `f` and `g` be two one-argument functions. The `composition f` after `g`
;;; is defined to be the function `x -> f(g(x))`. Define a procedure `compose`
;;; that implements composition. For example, if `inc` is a procedure that adds
;;; `1` to its argument,
;;; ((compose square inc) 6)
;;; 49

(define compose (lambda (function-f function-g)
  (lambda (x)
    (function-f (function-g x)))))

(define inc (lambda (x)
  (1+ x)))
(define test (lambda ()
  ((compose square inc) 6)))

