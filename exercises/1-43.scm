;;;; 1-43
;;;; 1-4-3
;;;; 2022/03/25

;;; If `f` is a numerical function and `n` is a positive integer, then we can
;;; form the `n`th repeated application of `f`, which is defined to be the
;;; function `x -> x + n`. If `f` is the operation of squaring a number, then
;;; the `n`th repeated application of `f` and a positive integer `n` and returns
;;; the procedure that computes the `n`th repeated application of `f`. Your
;;; procedure should be able to be used as follows:
;;; ((repeated square 2) 5)
;;; 625
;;; Hint: You may find it convenient to use `compose` from exercise 1.42
;;; ---
;;; `double` from exercise 1.41 in fact is a special form of `compose` from
;;; exercise 1.42.

(define compose (lambda (f g)
  (lambda (x)
    (f (g x)))))
(define repeated (lambda (function count)
  (define iterator (lambda (i result)
    (if (< i count)
      (iterator (1+ i) (compose function result))
      result)))
  (iterator 1 function)))

(define test (lambda ()
  ((repeated square 2) 5)))

