;;;; 1-38
;;;; 1-3-3
;;;; 2022/03/23

;;; In 1737, the Swiss mathematician Leonhard Euler published a memoir `De
;;; Fractionibus Continuis`, which included a continued fraction expansion for
;;; `e - 2`, where `e` is the base of the natural logarithms. In this fraction,
;;; the `N_i` are all `1`, and the `D_i` are successively `1, 2, 1, 1, 4, 1, 1,
;;; 6, 1, 1, 8, ...` Write a program that uses your `cont-frac` procedure from
;;; exercise 1.37 to approximate `e`, based on Euler's expansion.

(define cont-frac (lambda (get-n get-d k)
  (define iterator (lambda (i n/d)
    (if (> i 0)
      (let ((n (get-n i))
            (d (get-d i)))
        (iterator (-1+ i) (/ n (+ d n/d))))
      n/d)))
  (iterator k 0)))

(define f (lambda ()
  (define get-n (lambda (i) 1.0))
  (define get-d (lambda (i)
    (cond ((= i 1)  1)
          ((= i 2)  2)
          ((= i 3)  1)
          ((= i 4)  1)
          ((= i 5)  4)
          ((= i 6)  1)
          ((= i 7)  1)
          ((= i 8)  6)
          ((= i 9)  1)
          ((= i 10) 1)
          ((= i 11) 8))))
  (cont-frac get-n get-d 11)))

