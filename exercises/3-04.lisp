;;;; 3-04
;;;; 3-1-1
;;;; 2022/05/19

;;; Modify the `make-account` procedure of exercise 3.3 by adding another local
;;; state variable so that, if an account is accessed more than seven
;;; consecutive times with an incorrect password, it invokes the procedure
;;; `call-the-cops`.

(defun make-account (balance password)
  (let ((consecutive-times 0)
        (call-the-cops
          (lambda (&rest rest) (declare (ignore rest)) "Call the cops"))
        (withdraw
          (lambda (amount)
            (if (>= balance amount)
                (progn (setf balance (- balance amount)) balance)
                "Insufficient funds")))
        (deposit
          (lambda (amount)
            (setf balance (+ balance amount)) balance))
        (complaint
          (lambda (&rest rest) (declare (ignore rest)) "Incorrect password")))
    (flet ((handle-success (request)
             (setf consecutive-times 0)
             (cond ((equal 'withdraw request) withdraw)
                   ((equal 'deposit  request) deposit)
                   (t (error (format nil "Unknown request -- make-account ~A"
                                     request)))))
           (handle-fail ()
             (setf consecutive-times (+ consecutive-times 1))
             (if (> consecutive-times 6)
                 call-the-cops
                 complaint)))
      (let ((dispatch
              (lambda (input-password request)
                (if (equal input-password password)
                    (handle-success request)
                    (handle-fail)))))
        dispatch))))

(defparameter test-account (make-account 100 'secret-password))
(defun input-right-password ()
  (funcall (funcall test-account 'secret-password 'deposit) 10))
(defun input-wrong-password ()
  (funcall (funcall test-account 'some-other-password 'deposit) 10))
