;;;; 1-11
;;;; 1-2-2
;;;; 2022/03/14

;;; A function `f` is defined by the rule that `f(n) = n if n < 3` and `f(n) =
;;; f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3`. Write a procedure that computes
;;; `f` by means of a recursive process. Write a procedure that computes `f` by
;;; means of an iterative process.

(define f (lambda (n)
  (if (< n 3)
    n
    (+ (* 1 (f (- n 1)))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3)))))))

(define f-kai (lambda (n)
  (iterator 2 1 0 n)))
(define iterator (lambda (n pre-n pre-pre-n count)
  (if (> count 0)
    (iterator (+ (* 1 n) (* 2 pre-n) (* 3 pre-pre-n))
              n
              pre-n
              (- count 1))
    pre-pre-n)))

