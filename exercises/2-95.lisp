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
  (eq (car term?) 'term))
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
(export 'negative)
(export 'mul)


(defpackage term-list
  (:use :common-lisp)
  (:import-from :term :make-term)
  (:import-from :term :term?)
  (:import-from :term :order)
  (:import-from :term :coefficient))
(in-package :term-list)

(shadow 'common-lisp:gcd)

(defun make-term-list (&rest terms)
  (if (reduce #'(lambda (pre-result term)
                  (if pre-result
                      (term? term)
                      '()))
              `(t . ,terms))
      terms
      (error (format '() "There are something no term type -- MAKE-TERM-LIST ~A"
                     terms))))
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
  "divident: term-list -> divisor: term-list ->
(quotient: term-list . remainder: term-list)"
  (if (empty? divident)
      ;; divident is a 0 term in term list, and remainder is 0 term in term list.
      `(,(make-term-list) . ,(make-term-list))
      (let ((divident-first-term (first-term divident))
            ( divisor-first-term  (first-term divisor)))
        (if (> (order divisor-first-term) (order divident-first-term))
            `(,(make-term-list) . ,divident)
            (let* ((      quotient*-order (- (order divident-first-term)
                                             (order  divisor-first-term)))
                   (quotient*-coefficient (/ (coefficient divident-first-term)
                                             (coefficient  divisor-first-term)))
                   (quotient* (make-term-list
                               (make-term quotient*-order
                                          quotient*-coefficient)))
                   (substract (mul divisor quotient*))
                   (remainder* (sub divident substract))
                   (rest-result (div remainder* divisor)))
              `(,(add quotient* (car rest-result)) . ,(cdr rest-result)))))))
(defun remainder (divident divisor)
  (cdr (div divident divisor)))
(defun gcd (a b)
  (if (empty? b)
      a
      (gcd b (remainder a b))))

(export 'make-term-list)
(export 'add)
(export 'sub)
(export 'mul)
(export 'div)
(export 'gcd)


(defpackage polynomial
  (:use :common-lisp))
(in-package :polynomial)

(shadow 'common-lisp:variable)
(shadow 'common-lisp:gcd)

(defun make-polynomial (variable term-list)
  `(polynomial ,variable ,term-list))
(defun variable (polynomial)
  (cadr polynomial))
(defun term-list (polynomial)
  (caddr polynomial))
(defun variable= (a b)
  (eq a b))
(defun add (augend addend)
  (if (variable= (variable augend) (variable addend))
      (make-polynomial (variable augend)
                       (term-list:add (term-list augend) (term-list addend)))
      (error
       (format '() "Polynomials are not in same variable -- POLYNOMIAL:ADD ~A"
               `(,augend ,addend)))))
(defun sub (subtraction subtract)
  (if (variable= (variable subtraction) (variable subtract))
      (make-polynomial (variable subtraction)
                       (term-list:sub (term-list subtraction)
                                      (term-list subtract)))
      (error
       (format '() "Polynomials are not in same variable -- POLYNOMIAL:SUB ~A"
               `(,subtraction ,subtract)))))
(defun mul (multiplicand multiplier)
  (if (variable= (variable multiplicand) (variable multiplier))
      (make-polynomial (variable multiplicand)
                       (term-list:mul (term-list multiplicand)
                                      (term-list multiplier)))
      (error
       (format '() "Polynomials are not in same variable -- POLYNOMIAL:MUL ~A"
               `(,multiplicand ,multiplier)))))
(defun div (divident divisor)
  "divident: polynomial -> divisor: polynomial ->
(quotient: polynomial . remainder: polynomial)"
  (if (variable= (variable divident) (variable divisor))
      (let ((result (div (term-list divident) (term-list divisor))))
        `(,(make-polynomial (variable divident) (car result))
          ,(make-polynomial (variable divident) (cdr result))))
      (error
       (format '() "Polynomials are not in same variable -- POLYNOMIAL:DIV ~A"
               `(,divident ,divisor)))))
(defun gcd (divident divisor)
  (if (variable= (variable divident) (variable divisor))
      (make-polynomial (variable divident)
                       (term-list:gcd (term-list divident) (term-list divisor)))
      (error
       (format '() "Polynomials are not in same variable -- POLYNOMIAL:GCD ~A"
               `(,divident ,divisor)))))

(export 'make-polynomial)
(export 'variable)
(export 'term-list)
(export 'variable=)
(export 'add)
(export 'mul)
(export 'div)
(export 'gcd)


(in-package :common-lisp-user)

(defun make-term (order coefficient)
  (term:make-term order coefficient))
(defun make-term-list (&rest terms)
  (apply #'term-list:make-term-list terms))
(defun make-polynomial (variable term-list)
  (polynomial:make-polynomial variable term-list))

(defun test ()
  (let ((p1 (make-polynomial 'x (make-term-list (make-term 2 1)
                                                (make-term 1 -2)
                                                (make-term 0 1))))
        (p2 (make-polynomial 'x (make-term-list (make-term 2 11)
                                                (make-term 0 7))))
        (p3 (make-polynomial 'x (make-term-list (make-term 1 13)
                                                (make-term 0 7)))))
    (let ((q1 (polynomial:mul p1 p2))
          (q2 (polynomial:mul p1 p3)))
      (polynomial:gcd q1 q2))))
