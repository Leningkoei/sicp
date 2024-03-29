;;;; 2-96
;;;; 2-5-3
;;;; 2022/08/17

;;; a. Implement the procedure `pseudoremainder-terms`, which is just like
;;; `remainder-terms` except that it multiplies the dividend by the integerizing
;;; factor described above before calling `div-terms`. Modify `gcd-terms` to use
;;; `pseudoremainder-terms`, and verify that `greatest-common-divisor` now
;;; produces an answer with integer coefficients on the example in exercise
;;; 2.95.

;; answer: gcd-kai

;;; b. The GCD now has integer coefficients, but they are large than those of
;;; `P_1`. Modify `gcd-terms` so that it removes common factors from the
;;; coefficients of the answer by dividing all the coefficients by their
;;; (integer) greatest common divisor.

;; answer: gcd-kai-ni

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
(defun div (divident divisor)
  (make-term (- (order divident) (order divisor))
             (/ (coefficient divident) (coefficient divisor))))

(export 'make-term)
(export 'term?)
(export 'order)
(export 'coefficient)
(export 'add)
(export 'negative)
(export 'mul)
(export 'div)


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
            ( divisor-first-term (first-term  divisor)))
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
(defun psudoremainder (divident divisor)
  (let* ((o1 (order       (first-term divident)))
         (o2 (order       (first-term  divisor)))
         ( c (coefficient (first-term  divisor)))
         (constant (expt c (+ 1 o1 (- o2)))))
    (cdr (div (mul divident (make-term-list (make-term 0 constant))) divisor))))
(defun gcd (a b)
  (if (empty? b)
      a
      (gcd b (remainder a b))))
(defun gcd-kai (a b)
  (if (empty? b)
      a
      (gcd-kai b (psudoremainder a b))))
(defun gcd-kai-ni (a b)
  (let ((psudoremainder (gcd-kai a b)))
    (let ((gcd (apply #'common-lisp:gcd (map 'list
                                             #'(lambda (term)
                                                 (coefficient term))
                                             psudoremainder))))
      (map 'list
           #'(lambda (term)
               (term:div term (make-term 0 gcd)))
           psudoremainder))))

(export 'make-term-list)
(export 'add)
(export 'sub)
(export 'mul)
(export 'div)
(export 'gcd)
(export 'gcd-kai)
(export 'gcd-kai-ni)


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
(defmacro with-variable-check (name a b handle-same)
  `(if (variable= (variable ,a) (variable ,b))
       ,handle-same
       (error
        (format '() "Polynomials are not in same variable -- POLYNOMIAL:~A ~A"
                ,name `(,,a ,,b)))))
(defun add (augend addend)
  (with-variable-check 'add augend addend
    (make-polynomial (variable augend)
                     (term-list:add (term-list augend) (term-list addend)))))
(defun sub (subtraction subtract)
  (with-variable-check 'sub subtraction subtract
    (make-polynomial (variable subtraction)
                     (term-list:sub (term-list subtraction)
                                    (term-list subtract   )))))
(defun mul (multiplicand multiplier)
  (with-variable-check 'mul multiplicand multiplier
    (make-polynomial (variable multiplicand)
                     (term-list:mul (term-list multiplicand)
                                    (term-list multiplier)))))
(defun div (divident divisor)
  "divident: polynomial -> divisor: polynomial ->
(quotient: polynomial . remainder: polynomial)"
  (with-variable-check 'div divident divisor
    (let ((result (div (term-list divident) (term-list divisor))))
      `(,(make-polynomial (variable divident) (car result))
        ,(make-polynomial (variable divident) (cdr result))))))
(defun gcd (divident divisor)
  (with-variable-check 'gcd divident divisor
    (make-polynomial (variable divident)
                     (term-list:gcd (term-list divident) (term-list divisor)))))
(defun gcd-kai (divident divisor)
  (with-variable-check 'gcd-kai divident divisor
    (make-polynomial (variable divident)
                     (term-list:gcd-kai (term-list divident) (term-list divisor)))))
(defun gcd-kai-ni (divident divisor)
  (with-variable-check 'gcd-kai-ni divident divisor
    (make-polynomial (variable divident)
                     (term-list:gcd-kai-ni (term-list divident) (term-list divisor)))))

(export 'make-polynomial)
(export 'variable)
(export 'term-list)
(export 'variable=)
(export 'add)
(export 'mul)
(export 'div)
(export 'gcd)
(export 'gcd-kai)
(export 'gcd-kai-ni)


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
      (print `(gcd        ,(polynomial:gcd        q1 q2)))
      (print `(gcd-kai    ,(polynomial:gcd-kai    q1 q2)))
      (print `(gcd-kai-ni ,(polynomial:gcd-kai-ni q1 q2)))
      '())))
