;;;; 1-18
;;;; 1-2-4
;;;; 2022/03/16

;;; Using the results of exercises 1.16 and 1.17, devise a procedure that
;;; generates an iterative process for multiplying two integers in terms of
;;; adding, doubling, and halving and uses a logarithmic number of steps.

(define fast-*-kai (lambda (a b)
  (define iterator (lambda (k a b)
    (cond ((= b 0) k)
          ((even? b) (iterator k (double a) (halve b)))
          (else (iterator (+ k a) a (- b 1))))))
  (iterator 0 a b)))

