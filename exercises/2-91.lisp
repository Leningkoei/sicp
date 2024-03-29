;;;; 2-91
;;;; 2-5-3
;;;; 2022/08/11

;;; A univariate polynomial can be divided by another one to procedure a
;;; polynomial quotient and polynomial remainder. For example,
;;;
;;; x ^ 5 - 1
;;; --------- = x ^ 3 + x, remainder x - 1
;;; x ^ 2 - 1
;;;
;;; Division can be performed via long division. That is, divide the
;;; highest-order term of the dividend by the highest-order term of the
;;; divisor. The result is the first term of the quotient. Next, multiply the
;;; result by the divisor, subtract that from the dividend, and produce the rest
;;; of the answer by recursively dividing the difference by the divisor. Stop
;;; when the order of the divisor exceeds the order of the dividend and declare
;;; the dividend to be the remainder. Also, if the dividend ever becomes zero,
;;; return zero as both quotient and remainder.
;;;
;;; We can design a `div-poly` procedure on the model of `add-poly` and
;;; `mul-poly`. The procedure checks to see if the two polys have the same
;;; variable. If so, `div-poly` strips off the variable and passes the problem
;;; to `div-terms`. It is convenient to design `div-terms` to compute both the
;;; quotient and the remainder of a division. `div-terms` can take two term
;;; lists as arguments and return a list of the quotient term list and the
;;; remainder term list.
;;;
;;; Complete the following definition of `div-terms` by filling in the missing
;;; expressions. Use this to implement `div-poly`, which takes two polys as
;;; arguments and returns a list of the quotient and remainder polys.

(defun div-terms (L1 L2)
  "Have known `polynomial long division`?"
  (if (empty-termlist? L1)
      `(,(the-empty-termlist) ,(the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            `(,(the-empty-termlist) ,L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                      (div-terms
                       (sub-terms
                        L1
                        (mul-term-by-all-terms (make-term new-o new-c) L2))
                       L2)))
                `(,(adjoin-term (make-term new-o new-c) (car rest-of-result))
                  ,(cdr rest-of-result))))))))

(defun div-poly (p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (map 'list
           #'(lambda (item)
               "`item` will be quotient or remainder."
               (make-poly (variable p1) item))
           (div-terms (term-list p1) (term-list p2)))
      (error (format '() "Polys not in same var -- DIV-POLY ~A" `(,p1 ,p2)))))
