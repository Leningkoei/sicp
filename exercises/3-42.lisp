;;;; 3-42
;;;; 3-4-2
;;;; 2022/06/30

;;; Ben Bitdiddle suggests that it's a waste of time to create a new serialized
;;; procedure in response to every `withdraw` and `deposit` message. He says
;;; that `make-account` could be changed so that the calls to `protected` are
;;; done outside the `dispatch` procedure. That is, an account would return the
;;; same serialized procedure (which was created at the same time as the
;;; account) each time it is asked for a withdrawal procedure.

(defun make-account (balance)
  (labels ((withdraw (amount)
             (if (>= balance amount)
                 (progn (setf balance (- balance amount))
                        balance)
                 "Insufficient funds"))
           (deposit (amount)
             (setf balance (+ balance amount))
             balance))
    (let ((protected (make-serializer)))
      (let ((protected-withdraw (funcall protected #'withdraw))
            (protected-deposit  (funcall protected #'deposit)))
        (flet ((dispatch (m)
                 (cond ((equal m 'withdraw) protected-withdraw)
                       ((equal m 'deposit)  protected-deposit)
                       ((equal m 'balance)  balance)
                       ('t (error (format '()
                                          "Unknown request -- MAKE-ACCOUNT ~A"
                                          m))))))
          #'dispatch)))))

;;; Is this a safe change to make? in particular, is there any difference in
;;; what concurrency is allowed by these two versions of `make-account`?

;;; Answer from Internet:

;;; Safe.
