;;;; 2-73
;;;; 2-4-3
;;;; 2022/07/08

;;; Section 2.3.2 described a program that performs symbolic differentiation:

(defun deriv (exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplicand exp) var)
                        (multiplier exp))))
        ;; <more rules can be added here>
        ('t (error (format '() "Unknown expression type -- DERIV ~A" exp)))))

;;; We can regard this program as performing a dispatch on the type of the
;;; expression to be differentiated. In this situation the "type tag" of the
;;; datum is the algebraic operator symbol (such as `+`) and the operation being
;;; performed is `deriv`. We can transform this program into data-directed style
;;; by rewriting the basic derivative procedure as

(defun operator (exp)
  (car exp))
(defun operands (exp)
  (cdr exp))
(defun deriv (exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ('t (funcall (get 'deriv (operator exp)) (operands exp) var))))

;;; a. Explain what was done above. Why can't we assimilate the predicates
;;; `number?` and `same-variable?` into the data-directed dispatch?

;; type of exp: (type-tag (data-a data-b))
;; When the operator of exp is sum, (get 'deriv (operator exp)) -> sum?
;; When the operator of exp is pro, (get 'deriv (operator exp)) -> pro?
;; Because it is hard to write the selector, maybe

;;; b. Write the procedures for derivatives of sums and products, and the
;;; auxiliary code required to install them in the table used by the program
;;; above.

(defun make-sum (addend augend)
  (cond ((=number? addend 0) augend)
        ((=number? augend 0) addend)
        ((and (number? addend) (number? augend)) (+ addend augend))
        ('t `(+ ,addend ,augend))))
(defun install-sum-package ()
  (labels ((addend (exp) (car exp))
           (augend (exp) (cadr exp))
           (deriv-sum (exp var)
             (make-sum (deriv (addend exp) var)
                       (deriv (augend exp) var))))
    (put 'deriv '+ #'deriv-sum)
    'done))

(defun make-product (multiplier multiplicand)
  (cond ((or (=number? multiplier 0) (=number? multiplicand 0)) 0)
        ((=number? multiplier 1) multiplicand)
        ((=number? multiplicand 1) multiplier)
        ((and (number? multiplier) (number? multiplicand))
         (* multiplier multiplicand))
        ('t `(* ,multiplier ,multiplicand))))
(defun install-product-package ()
  (labels ((multiplier (exp) (car exp))
           (multiplicand (exp) (cdr exp))
           (deriv-product (exp var)
             (make-sum
              (make-product multiplier (deriv multiplicand var))
              (make-product multiplicand (deriv multiplier var)))))
    (put 'deriv '* #'deriv-product)
    'done))

;;; c. Choose any additional differentiation rule that you like, such as the one
;;; for exponents (exercise 2.56), and install it in this data-directed system.

(defun make-exponentation (base exponent)
  (cond ((=number? base 0) 0)
        ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        ('t `(^ ,base ,exponent))))
(defun install-exponentation-package ()
  (labels ((base (exp) (car exp))
           (exponent (exp) (cdr exp))
           (deriv-exponentation (exp var)
             (make-product
              (make-product (exponent exp)
                            (make-exponentation (base exp)
                                                (- (exponent exp) 1)))
              (deriv (exp base) var))))
    (put 'deriv '^ #'deriv-exponentation)
    'done))

;;; d. In this simple algebraic manipulator the type of an expression is the
;;; algebraic operator that binds it together. Suppose, however, we indexed the
;;; procedures in the opposite way, so that the dispatch line in `deriv` looked
;;; like
;; (funcall (get (operator exp) 'deriv) (operands exp) var)
;;; What corresponding changes to the derivative system are required?

;;; The order
;;; (put 'deriv '+ #'deriv-sum) -> (put '+ 'deriv #'deriv-sum)
