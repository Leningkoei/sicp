;;;; 3-10
;;;; 3-2-3
;;;; 2022/05/20

;;; In the `make-withdraw` procedure, the local variable `balance` is created
;;; as a parameter of `make-withdraw`. We could also create the local state
;;; variable explicitly, using `let`, as follows:
(defun make-withdraw (initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (progn (setf balance (- balance amount)) balance)
          "Insufficient funds"))))
;;; Recall from section 1.3.2 that `let` is simply syntactic sugar for a
;;; procedure call:
;;; (let ((<var> <exp>)) body)
;;; is interpreted as an alternate syntax for
;;; ((lambda (<var>) <body>) <exp>)
;;; Use the environment model to analyze this alternate version
;;; (define W1 (make-withdraw 100))
;;; (W1 50)
;;; (define W2 (make-withdraw 100))
;;; Show that the two versions of `make-withdraw` create objects with the same
;;; behavior. How do the environment structures differ for the two versions?
