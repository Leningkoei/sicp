;;;; 3-03
;;;; 3-1-1
;;;; 2022/05/19

;;; Modify the `make-account` procedure so that it creates password-protected
;;; accounts. That is, `make-account` should take a symbol as an additional
;;; argument, as in
;;; (define acc (make-account 100 'secret-password))
;;; The resulting accoumt object should process a request only if it is
;;; accompanied by the password with which the account was created, and should
;;; otherwise return a complaint:
;;; ((acc 'secret-password 'withdraw) 40)
;;; >> 60
;;; ((acc 'some-other-password 'deposit) 50)
;;; >> "Incorrect password"

(defun make-account (balance password)
  (let ((withdraw
          (lambda (amount)
            (if (>= balance amount)
                (progn (setf balance (- balance amount)) balance)
                "Insufficient funds")))
        (deposit
          (lambda (amount)
            (setf balance (+ balance amount)) balance))
        (complaint
          (lambda (&rest rest)
            (declare (ignore rest))
            "Incorrect password")))
    (let ((dispatch
            (lambda (input-password request)
              (cond ((not (equal password input-password)) complaint)
                    ((equal 'withdraw request) withdraw)
                    ((equal 'deposit  request) deposit)
                    (t (error (format nil "Unknown request -- make-account ~A"
                                      request)))))))
      dispatch)))

(defun test ()
  (let ((test-account (make-account 100 'secret-password)))
    (format t "~A"
            (funcall (funcall test-account 'secret-password 'withdraw) 40))
    (fresh-line)
    (format t "~A"
            (funcall (funcall test-account 'some-other-password 'deposit) 50))
    (fresh-line)))
