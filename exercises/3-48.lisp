;;;; 3-48
;;;; 3-4-2
;;;; 2022/07/02

;;; Explain in detail why the deadlock-avoidance method described above, (i.e.,
;;; the accounts are numbered, and each process attempts to acquire the
;;; small-numbered account first) avoids deadlock in the exchange
;;; problem. Rewrite `serialized-exchange` to incorporate this idea. (You will
;;; also need to modify `make-account` so that each account is created with a
;;; number, which can be accessed by sending an appropriate message.)

(defun get-repeatless-number ()
  "It function will return a repeatless number."
  'todo)

(defun make-account-and-serializer (balance)
  (flet ((withdraw (amount)
           (if (>= balance amount)
               (progn (setf balance (- balance amount)) balance)
               "Insufficient funds"))
         (deposit (amount)
           (setf balance (+ balance amount)) balance))
    (let ((balance-serializer (make-serializer))
          (weight (get-repeatless-number)))
      (flet ((dispatch (m)
               (cond ((equal m 'withdraw) #'withdraw)
                     ((equal m 'deposit)  #'deposit)
                     ((equal m 'balance)  balance)
                     ((equal m 'serializer) serializer)
                     ((equal m 'weight)   weight)
                     ('t (error (format '() "Unknown request -- MAKE-ACCOUNT-AND-SERIALIZER ~A"
                                        m))))))
        #'dispatch))))

(defun serialized-exchange (account1 account2)
  (let ((serializer1 (funcall account1 'serializer))
        (serializer2 (funcall account2 'serializer))
        (weight1 (funcall account1 'weight))
        (weight2 (funcall account2 'weight)))
    (if (> weight1 weight2)
        (funcall (funcall serializer1 (funcall serializer2 #'exchange))
                 account1 account2)
        (funcall (funcall serializer2 (funcall serializer1 #'exchange))
                 account1 account2))))
