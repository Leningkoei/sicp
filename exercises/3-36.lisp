;;;; 3-36
;;;; 3-3-3
;;;; 2022/06/25

;;; Suppose we evaluate the following sequence of expressions in the global
;;; environment:

;; (defparameter a (make-connector))
;; (defparameter b (make-connector))
;; (set-value! a 10 'user)

;;; At some time during evaluation of the `set-value!`, the following expression
;;; from the connector's local procedure is evaluated:

;; (for-each-except setter 'inform-about-value constraints)

;;; Draw an environment diagram showing the environment in which the above
;;; expression is evaluated.
