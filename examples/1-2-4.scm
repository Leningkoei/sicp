;;;; 1-2-4
;;;; 2022/03/15

;;; Consider the problem of computing the exponential of a given number. We
;;; would like a procedure that takes as arguments a base `b` and a positive
;;; integer exponent `n` and computes `b ** n`.

;;; recursion
;;; b ** n = b * b ** (n - 1)
;;; b ** 0 = 1

(define exponential (lambda (b n)
  (if (= n 0)
    1
    (* b (exponential b (-1+ n))))))

;;; iteration

(define exponential-kai (lambda (b n)
  (define iterator (lambda (result count)
    (if (= count n)
      result
      (iterator (* b result) (+ count 1)))))
  (iterator 1 0)))

;;; fast-expt
;;; recursion
;;; b ** n = (b ** (n / 2)) ** 2    if `n` is even
;;; b ** n = b * b ** (n - 1)       if `n` is odd

(define fast-expt (lambda (b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1)))))))

