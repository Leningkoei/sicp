;;;; 3-45
;;;; 3-4-2
;;;; 2022/06/30

;;; Louis Reasoner thinker our bank-account system is unnecessarily complex and
;;; error-prone now that deposits and withdrawals aren't automatically
;;; serialized. He suggests that `make-account-and-serializer` should have
;;; exported the serializer (for use by such procedures as serialized-exchange)
;;; in addition to (rather than instead of) using it to serialize accounts and
;;; deposits as `make-account` did. He proposes to redefine accounts as follows:

(defun make-accout-and-serializer (balance)
  (labels ((withdraw (amount)
             (if (>= balance amount)
                 (progn (setf balance (- balance amount)) balance)
                 "Insufficient funds"))
           (deposit  (amount)
             (setf (balance (+ balance amount))) balance))
    (let ((balance-serializer (make-serializer)))
      (flet ((dispatch (m)
               (cond ((equal m 'withdraw)
                      (funcall balance-serializer #'withdraw))
                     ((equal m 'deposit)
                      (funcall balance-serializer #'deposit))
                     ((equal m 'balance)
                      balance)
                     ((equal m 'serializer)
                      balance-serializer)
                     ('t (error (format '()
                                        "Unknown request -- MAKE-ACCOUNT ~A"
                                        m))))))
        #'dispatch))))

;;; Then deposits are handled as with the original `make-account`:

(defun deposit (account amount)
  (funcall (funcall account 'deposit) amount))

;;; Explain what is wrong with Louis's reasoning. In particular, consider what
;;; happens when `serialized-exchange` is called.

(defun exchange (account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    (funcall (funcall account1 'withdraw) difference)
    (funcall (funcall account2 'deposit)  difference)))
(defun serialized-exchange (account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (funcall (funcall serializer1 (funcall serializer2 #'exchange))
             account1 account2)))

;;; Answer from Internet:

;; Wait forever.
