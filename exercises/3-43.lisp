;;;; 3-43
;;;; 3-4-2
;;;; 2022/06/30

;;; Suppose that the balances in three accounts start out as $10, $20, and $30,
;;; and that multiple processes run, exchanging the balances in the
;;; accounts. Argue that if the processes are run sequentially, after any number
;;; of concurrent exchanges, the account balances should be $10, $20, and $30 in
;;; some order. Draw a timing diagram like the on in figure 3.29 to show how
;;; this condition can be violated if the exchanges are implemented using the
;;; first version of the account-exchange program in this section. On the other
;;; hand, argue that even with this `exchange` program, the sum of the balances
;;; in the accounts will be preserved. Draw a timing diagram to show how even
;;; this condition would be violated if we did not serialize the transactions on
;;; individual accounts.

(defun make-account-and-serializer (balance)
  (labels ((withdraw (amount)
             (if (>= balance amount)
                 (progn (setf balance (- balance amount)) balance)
                 "Insufficient funds"))
           (deposit (amount)
             (setf balance (+ balance amount)) balance))
    (let ((balance-serializer (make-serializer)))
      (flet ((dispatch (m)
               (cond ((equal m 'withdraw) #'withdraw)
                     ((equal m 'deposit)  #'deposit)
                     ((equal m 'balance)    balance)
                     ((equal m 'serializer) balance-serializer)
                     ('t (error (format '()
                                        "Unknown request -- MAKE-ACCOUNT-AND-SERIALIZER ~A"
                                        m))))))
        #'dispatch))))

(defun deposit (account mount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    (funcall (funcall s d) amount)))

(defun exchange (account1 account2)
  (let ((difference (- (account1 'balance) (account2 'balance))))
    (funcall (funcall account1 'withdraw) difference)
    (funcall (funcall account2 'deposit)  difference)))

(defun serialized-exchange (account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (funcall (fucnall serializer1 (funcall serializer2 exchange))
             account1 account2)))

;;; I don't know what's your means.
