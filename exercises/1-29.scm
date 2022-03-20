;;;; 1-29
;;;; 1-3-1
;;;; 2022/03/20

(define reload (lambda ()
  (load "./1-29.scm")))

;;; Simpson's Rule is a more accurate method of numerical integration than the
;;; method illustrated above. Using Simpson's Rule, the integral of a function
;;; `f` between `a` and `b` is approximated as
;;; (h / 3) *
;;; (y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n - 2) + 4y_(n - 1) + y_n)
;;; where `h = (b - a) / n`, for some even integer `n`, and `y_k = f(a + k h)`.
;;; (Increasing `n` increases the accuracy of the approximation.) Define a
;;; procedure that takes as arguments `f`, `a`, `b`, and `n` and returns the
;;; value of the integral, computed using Simpson's Rule. Use your procedure to
;;; integrate `cube` between `0` and `1` (with `n = 100` and `n = 1000`), and
;;; compare the results to those of the `integral` procedure shown above.

(define sum (lambda (term a next b)
  (if (< a b)
    (+ (term a)
       (sum term (next a) next b))
    0)))
(define Simpson-rule (lambda (f a b n)
  (define h (/ (- b a) n))
  (define term (lambda (i)
    ; (display-line-and-return
    (cond ((= i 0) (f a))
          ((= i n) (f (+ a (* n h))))
          ((odd?  i) (* 4 (f (+ a (* i h)))))
          ((even? i) (* 2 (f (+ a (* i h))))))))
    ; )
  (define next (lambda (i) (1+ i)))
  (* (/ h 3) (sum term 0 next n) 1.0)))
(define display-line (lambda (content)
  (display content)
  (newline)))
(define display-line-and-return (lambda (content)
  (display-line content)
  content))

(define do-test (lambda ()
  (Simpson-rule cube 0 1 1000)))

