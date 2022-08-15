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

(defpackage term
  (:use :common-lisp))
(in-package :term)

(defun make-term (order coefficient)
  `(term ,order ,coefficient))
(defun term? (term?)
  (eq term? 'term))
(defun order (term)
  (cadr term))
(defun coefficient (term)
  (caddr term))
(defun add (augend addend)
  (if (= (order augend) (order addend))
      (make-term (order augend)
                 (+ (coefficient augend) (coefficient addend)))
      (error (format '() "Terms are not in same order -- ADD-TERM ~A"
                     `(,augend ,addend)))))
(defun negative (term)
  (make-term (order term)
             (- (coefficient term))))
(defun mul (multiplicand multiplier)
  (make-term (+ (order multiplicand) (order multiplier))
             (* (coefficient multiplicand) (coefficient multiplier))))

(export 'make-term)
(export 'term?)
(export 'order)
(export 'coefficient)
(export 'add)
(export 'mul)


(defpackage term-list
  (:use :common-lisp)
  (:import-from :term :make-term)
  (:import-from :term :term?)
  (:import-from :term :order)
  (:import-from :term :coefficient))
(in-package :term-list)

(defun make-term-list (&rest terms)
  (reduce #'(lambda (pre-result term)
              (if pre-result
                  (term? term)
                  pre-result))
          `(() . ,terms))
  `(,@terms))
(defun empty? (term-list)
  (null term-list))
(defun first-term (term-list)
  (if (empty? term-list)
      (error (format '() "Trying to get first term from a empty term list -- TERM-LIST:FIRST-TERM"))
      (car term-list)))
(defun remove-0-term (rest-term-list)
  (if rest-term-list
      (if (= (coefficient (car rest-term-list)) 0)
          (remove-0-term (cdr rest-term-list))
          `(,(car rest-term-list) .
            ,(remove-0-term (cdr rest-term-list))))
      '()))
(defun adjoin-term (term-list term)
  (remove-0-term
   (if (reduce #'(lambda (pre-result term-in-term-list)
                   (if pre-result
                       pre-result
                       (= (order term-in-term-list) (order term))))
               `(() . ,term-list))
       ;; If there is term in term list with the same order with term.
       ;; Adjoin it.
       (map 'list
            #'(lambda (term-in-term-list)
                (if (= (order term-in-term-list) (order term))
                    (term:add term-in-term-list term)
                    term-in-term-list))
            term-list)
       ;; Push it into a right position.
       (sort `(,term . ,term-list)
             #'(lambda (term-a term-b)
                 (> (order term-a) (order term-b)))))))
(defun add (augend addend)
  (labels ((iterator (newest-augend rest-addend)
             (if rest-addend
                 (iterator (adjoin-term newest-augend (car rest-addend))
                           (cdr rest-addend))
                 newest-augend)))
    (iterator augend addend)))
(defun sub (subtraction subtract)
  (add subtraction (map 'list #'term:negative subtract)))
(defun mul (multiplicand multiplier)
  (reduce #'add
          (map 'list
               #'(lambda (term-of-multiplier)
                   (map 'list
                        #'(lambda (term-of-multiplicand)
                            (term:mul term-of-multiplicand term-of-multiplier))
                        multiplicand))
               multiplier)))
(defun div (divident divisor)
  ;; to test
  (if (empty? divident)
      `(,(make-term-list) 0)
      (let ((divident-first-term (first-term divident))
            ( divisor-first-term  (first-term divisor)))
        (if (> (order divisor-first-term) (order divident-first-term))
            `(,(make-term-list) ,divident)
            (let* ((      quotient*-order (- (order divident) (order divisor)))
                   (quotient*-coefficient (/ (coefficient divident)
                                            (coefficient divisor)))
                   (quotient* (make-term-list
                               (make-term quotient*-order
                                          quotient*-coefficient)))
                   (substract (mul divisor quotient*))
                   (remainder* (sub divident substract))
                   (rest-result (div remainder* divisor)))
              `(,(add quoitent* (car rest-result)) ,(cadr rest-result)))))))

(export 'make-term-list)
(export 'add)
(export 'sub)
(export 'mul)


(defpackage polynomial
  (:use :common-lisp))
(in-package :polynomial)

(defun make-polynomial (variable term-list)
  `(polynomial ,variable ,term-list))
(defun var (polynomial)
  (cadr polynomial))
(defun term-list (polynomial)
  (caddr polynomial))
(defun var= (var1 var2)
  (eq var1 var2))
(defun add (augend addend)
  (if (var= (var augend) (var addend))
      (make-polynomial (var augend)
                       (term-list:add (term-list augend) (term-list addend)))
      (error
       (format '() "Polynomials are not in same variable -- POLYNOMIAL:ADD ~A"
               `(,augend ,addend)))))
(defun sub (subtraction subtract)
  (if (var= (var subtraction) (var subtract))
      (make-polynomial (var subtraction)
                       (term-list:sub (term-list subtraction)
                                      (term-list subtract)))
      (error
       (format '() "Polynomials are not in same variable -- POLYNOMIAL:SUB ~A"
               `(,subtraction ,subtract)))))
(defun mul (multiplicand multiplier)
  (if (var= (var multiplicand) (var multiplier))
      (make-polynomial (var multiplicand)
                       (term-list:mul (term-list multiplicand)
                                      (term-list multiplier)))
      (error
       (format '() "Polynomials are not in same variable -- POLYNOMIAL:MUL ~A"
               `(,multiplicand ,multiplier)))))

(export 'make-polynomial)
(export 'var)
(export 'term-list)
(export 'var=)
(export 'add)
(export 'mul)
