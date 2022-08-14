;;;; 2-95
;;;; 2-5-3
;;;; 2022/08/13

;;; Define p1, P2, and P3 to the polynomials

;;; P1:   x^2 - 2x + 1
;;; P2: 11x^2      + 7
;;; P3:        13x + 5

;;; Now define `Q_1` to be the product of `P_1` and `P_2` and `Q_2` to be the
;;; product of `P_1` and `P_3`, and use `greatest-common-divisor` (exercise
;;; 2.94) to compute the GCD of `Q_1` and `Q_2`. Note that the answer is not the
;;; same as `P_1`. This example introduces noninteger operations into the
;;; computation, causing difficulties with the GCD algorithm. To understand what
;;; is happening, try tracing `gcd-terms` while computing the GCD or try
;;; performing the division by hand.

;;; We can solve the problem exhibited in exercise 2.95 if we use the following
;;; modification of the GCD algorithm (which really works only in the case of
;;; polynomials with integer coefficients). Before performing any polynomial
;;; division in the GCD computation, we multiply the dividend by an integer
;;; constant factor, chosen to guarantee that no fractions will arise during the
;;; division process. Our answer will thus differ from the actual GCD by an
;;; integer constant factor, but this does not matter in the case of reducing
;;; rational functions to lowest terms; the GCD will be used to divide both the
;;; numerator and denominator, so the integer constant factor will cancel not.

;;; More precisely, if `P` and `Q` are polynomials, let `O_1` be the order of
;;; `P` (i.e., the order of the largest term of `P`) and let `O_2` be the order
;;; of `Q`. Let `c` be the leading coefficient of `Q`. Then it can be shown
;;; that, if we multiply `P` by the `integerizing factor `c^(1 + O_1 - O_2), the
;;; resulting polynomial can be divided by `Q` by using the `div-terms`
;;; algorithm without introducing any fractions. The operation of multiplying
;;; the dividend by this constant and then dividing is sometimes called the
;;; `pseudodivision` of `P` by `Q`. The remainder of the division is called the
;;; `pseudoremainder`.

(defun make-term (order coefficient)
  `(term ,order ,coefficient))
(defun order (term)
  (cadr term))
(defun coefficient (term)
  (caddr term))

(defun make-empty-term-list ()
  '())
(defun empty-term-list? (term-list)
  (null term-list))
(defun adjoin-term-list (term-list term)
  (if (reduce #'(lambda (pre-result term-in-term-list)
                  (if pre-result
                      pre-result
                      (= (order term-in-term-list) (order term))))
              `(() . ,term-list))
      ;; If there is term in term list with the same order with term.
      ;; adjoin it.
      (map 'list
           #'(lambda (term-in-term-list)
               (if (= (order term-in-term-list) (order term))
                   (make-term (order term-in-term-list)
                              (+ (coefficient term-in-term-list)
                                 (coefficient term)))
                   term-in-term-list))
           term-list)
      ;; push it into a right position.
      (sort `(,term . ,term-list)
            #'(lambda (term-a term-b)
                (> (order term-a) (order term-b))))))

(defun make-polynomial (variable term-list)
  `(polynomial ,variable ,term-list))
(defun variable (polynomial)
  (cadr polynomial))
(defun term-list (polynomial)
  (caddr polynomial))

(defun add-polynomial (p1 p2)
  (if (variable= (variable p1) (variable p2))
      (make-polynomial (variable p1) (add-terms (term-list p1) (term-list p2)))
      (error
       (format '() "Polynomials are not in same variable -- ADD-POLYNOMIAL ~A"
               `(,p1 ,p2)))))
