;;;; 3-41
;;;; 3-4-2
;;;; 2022/06/29

;;; Ben Bitdiddle worries that it would be better to implement the bank account
;;; as follows (where the commented line has been changed):

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
      (flet ((dispatch (m)
               (cond ((equal m 'withdraw)
                      (funcall protected #'withdraw))
                     ((equal m 'deposit)
                      (funcall protected #'deposit))
                     ;; ((equal m 'balance)
                     ;;  ;; original
                     ;;  balance)
                     ((equal m 'balance)
                      ;; Ben's
                      (protected (lambda () balance)))
                     ('t (error (format '()
                                        "Unknown request -- MAKE-ACCOUNT ~A"
                                        m))))))
        #'dispatch))))

;;; because allowing unserialized access to the bank balance can result in
;;; anomalous behavior. Do you agree? Is there any scenario that demonstrates
;;; Ben's concern?

;;; Return balance already is a cell.
