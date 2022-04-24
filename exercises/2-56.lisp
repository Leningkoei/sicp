;;;; 2-56
;;;; 2-3-2
;;;; 2022/04/24

;;; Show how to extend the basic differentiator to handle more kinds of
;;; expressions. For instance, implement the differentiation rule
;;; d(u^n)               du
;;; ------ = n u^(n-1) (----)
;;;   dx                 dx
;;; by adding a new clause to the `deriv` program and defining appropriate
;;; procedures `exponentiation?`, `base`, `exponent`, and
;;; `make-exponentiation`. (You may use the symbol `**` to denote
;;; exponentiation.) Build in the rules that anything raised to the power `0`
;;; and `1` and anything raised to the power `1` is the thing itself.

(defun ^ (base exponent)
  (expt base exponent))
(defun exponentiation? (e)
  "Is `e` a exponentiation?"
  ;; e: exp
  (and (listp e) (equal (car e) '^)))
(defun base (exponentiation)
  "Base of the exponentiation."
  (cadr exponentiation))
(defun exponent (exponentiation)
  "Exponent of the exponentiation."
  (caddr exponentiation))
(defun make-exponentiation (base exponent)
  "Exp | Number -> Exp | Number -> Exp | Number"
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (t (list '^ base exponent))))

(defun deriv (exp var)
  (cond ((number?   exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum?      exp) (make-sum (deriv (addend exp) var)
                                   (deriv (augend exp) var)))
        ((product?  exp) (make-sum (make-product (multiplier exp)
                                                 (deriv (multiplicand exp) var))
                                   (make-product (deriv (multiplier exp) var)
                                                 (multiplicand exp))))
        ;;+
        ((exponentiation? exp)
         (let ((base (base exp)) (exponent (exponent exp)))
           (make-product
           (make-product exponent
                          (make-exponentiation base (make-sum exponent -1)))
            (deriv base var))))
        ;;-
        (t (error "unknown expression type -- DERIV"))))
