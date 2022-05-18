;;;; 3-1-1
;;;; 2022/05/18

((lambda (balance)
   (defun new-withdraw (amount)
     (if (>= balance amount)
         (progn (setf balance (- balance amount)) balance)
         "Insufficient funds")))
 100)

;; (define make-withdraw (balance)
;;   (lambda (amount)
;;     (if (>= balance amount)
;;         (progn (setf balance (- balance amount)) balance)
;;         "Insufficient funds")))
(defmacro make-withdraw (name balance)
  `((lambda (balance)
      (defun ,name (amount)
        (if (>= balance amount)
            (progn (setf balance (- balance amount)) balance)
            "Insufficient funds")))
    ,balance))

(defun make-account (balance)
  (let ((withdraw
          (lambda (amount)
            (if (>= balance amount)
                (progn (setf balance (- balance amount)) balance)
                "Insufficient funds")))
        (deposit
          (lambda (amount)
            (setf balance (+ balance amount))
            balance)))
    (let ((dispatch
            (lambda (m)
              (cond ((equal m 'withdraw) withdraw)
                    ((equal m 'deposit) deposit)
                    (t (error (format nil "Unknown request --make-account ~A"
                                      m)))))))
      dispatch)))

(defun test-make-account ()
  (let ((test-account (make-account 100)))
    (format t "Withdraw 50, balance: ~A"
            (funcall (funcall test-account 'withdraw) 50))
    (fresh-line)
    (format t "Withdraw 60, balance: ~A"
            (funcall (funcall test-account 'withdraw) 60))
    (fresh-line)
    (format t "Deposit  40, balance: ~A"
            (funcall (funcall test-account 'deposit)  40))
    (fresh-line)
    (format t "Withdraw 60, balance: ~A"
            (funcall (funcall test-account 'withdraw) 60))
    (fresh-line)))
