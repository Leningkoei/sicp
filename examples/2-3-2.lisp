;;;; 2-3-2: Symbolic Differentiation
;;;; 2022/04/24

;;; The differentiation program with abstract data

;; (variable? e)             Is `e` a variable?
;; (same-variable? v1 v2)    Are `v1` and `v2` the same variable?

;; (sum? e)                  Is `e` a sum?
;; (addend s)                Addend of the sum `s`.
;; (augend s)                Augend of the sum `s`.
;; (make-sum a1 a2)          Construct the sum of `a1` and `a2`.

;; (product? e)              Is `e` a product?
;; (multiplier p)            Multiplier of the product `p`.
;; (multiplicand p)          Multiplicand of the product `p`.
;; (make-product m1 m2)      Construct the product of `m1` and `m2`.

;; Using these, and the primitive predicate `number?`, which identifies
;; numbers, we can express the differentiation rules as the following
;; procedure:

(defun number? (e)
  "Is `e` a number?"
  ;; e: exp
  (numberp e))
(defun =number? (exp number)
  "Checks whether an expression is equal to a given number."
  (and (number? exp) (= exp number)))
(defun variable? (e)
  "Is `e` a variable?"
  ;; e: exp
  (symbolp e))
(defun same-variable? (v1 v2)
  "Are `v1` and `v2` the same variable?"
  ;; v.: variable.
  (equal v1 v2))

(defun sum? (e)
  "Is `e` a sum?"
  ;; e: exp
  (and (listp e) (equal (car e) '+)))
(defun addend (s)
  "Addend of the sum `s`."
  ;; s: sum
  (cadr s))
(defun augend (s)
  "Augend of the sum `s`."
  ;; s: sum
  (caddr s))
(defun make-sum (addend augend)
  "Construct the sum of addend and augend."
  ;; (list '+ addend augend))
  (cond ((=number? addend 0) augend)
        ((=number? augend 0) addend)
        ((and (number? addend) (number? augend)) (+ addend augend))
        (t (list '+ addend augend))))

(defun product? (e)
  "Is `e` a product?"
  ;; e: exp
  (and (listp e) (equal (car e) '*)))
(defun multiplier (p)
  "Multiplier of the product `p`."
  ;; p: product
  (cadr p))
(defun multiplicand (p)
  "Multiplicand of the product `p`."
  ;; p: product
  (caddr p))
(defun make-product (multiplier multiplicand)
  "Construct the product of multiplier and multiplicand."
  ;; (list '* multiplier multiplicand))
  (cond ((or (=number? multiplier 0) (=number? multiplicand 0)) 0)
        ((=number? multiplier 1) multiplicand)
        ((=number? multiplicand 1) multiplier)
        ((and (number? multiplier) (number? multiplicand) (* multiplier
                                                             multiplicand)))
        (t (list '* multiplier multiplicand))))

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
        ((exponentiation? exp)
         (let ((base (base exp)) (exponent (exponent exp)))
           (make-product
            (make-product exponent
                          (make-exponentiation base (make-sum exponent -1)))
            (deriv base var))))
        (t (error "unknown expression type -- DERIV"))))
