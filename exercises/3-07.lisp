;;;; 3-07
;;;; 3-1-3
;;;; 2022/05/19

;;; Consider the bank account objects created by `make-account`, with the
;;; password modification described in exercise 3.3. Suppose that our banking
;;; system requires the ability to make joint accounts. Define a procedure
;;; `make-joint` that accomplishes this. `make-joint` should take three
;;; arguments. The third argument is a new password. `make-joint` is to create
;;; an additional access to the original account using the new password. For
;;; example, if `peter-acc` is a bank account with password `open-sesame`, then
;;; (define paul-acc
;;;   (make-joint peter-acc 'open-sesame 'rosebud))
;;; will allow one to make transactions on `peter-acc` using the name
;;; `paul-acc` and the password `rosebud`. You may wish to modify your solution
;;; to exercise 3.3 to accommodate this new feature.

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
            "Incorrect password"))
        (change-password
          (lambda (new-password)
            (setf password new-password) t)))
    (let ((dispatch
            (lambda (input-password request)
              (cond ((not (equal password input-password)) complaint)
                    ((equal 'change-password request) change-password)
                    ((equal 'withdraw request) withdraw)
                    ((equal 'deposit  request) deposit)
                    (t (error (format nil "Unknown request -- make-account ~A"
                                      request)))))))
      dispatch)))
(defun make-joint (account password this-password)
  (lambda (input-password request)
    (if (not (equal input-password this-password))
        (lambda (&rest rest) (declare (ignore rest)) "Incorrect password")
        (funcall account password request))))

(defparameter peter-acc
  (make-account 50 'open-sesame))
(defparameter paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
(defun test ()
  (let ((peter-acc (make-account 50 'open-sesame)))
    (let ((paul-acc (make-joint peter-acc 'open-sesame 'rosebud)))
      (format t "~A" (funcall (funcall peter-acc 'open-sesame 'deposit) 50))
      (fresh-line)
      (format t "~A" (funcall (funcall paul-acc 'rosebud 'deposit) 50))
      (fresh-line)
      (format t "~A" (funcall (funcall peter-acc 'rosebud 'deposit) 50))
      (fresh-line)
      (format t "~A" (funcall (funcall paul-acc 'open-sesame 'deposit) 50))
      (fresh-line))))
