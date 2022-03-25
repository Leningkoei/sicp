;;;; 1-44
;;;; 1-4-3
;;;; 2022/03/25

;;; The idea of `smoothing` a function is an important concept in signal
;;; processing. If `f` is a function and `dx` is some small number, then the
;;; smoothed version of `f` is the function whose value at a point `x` is the
;;; average of `f(x - dx)`, `f(x)`, and `f(x + dx)`. Write a procedure `smooth`
;;; that takes as input a procedure that computes `f` and returns a procedure
;;; that computes the smoothed `f`. It is sometimes valuable to repeatedly
;;; smooth a function (that is, smooth the smoothed function, and so on) to
;;; obtained the `n-fold smoothed function`. Show how to generate the `n`-fold
;;; smoothed function of any given function using `smooth` and `repeated` from
;;; exercise 1.43.

(define average-3 (lambda (a b c)
  (/ (+ a b c) 3)))
(define dx 0.00001)
(define smooth (lambda (f)
  (lambda (x)
    (let ((x-dx (- x dx))
          (x+dx (+ x dx)))
    (average-3 (f x-dx) (f x) (f x+dx))))))

(define compose (lambda (f g)
  (lambda (x)
    (f (g x)))))
(define repeated (lambda (function count)
  (define iterator (lambda (i result)
    (if (< i count)
      (iterator (1+ i) (compose function result))
      result)))
  (iterator 1 function)))

(define smooth-nth (lambda (f n)
  (repeated f n)))

;;; No test.

