;;;; 3-3-1: Mutable List Structure
;;;; 2022/05/26

;;; Sharing and identity

;;; Mutation is just assignment

(defun my-cons (x y)
  (let ((set-car!
          (lambda (new-value)
            (setf x new-value)))
        (set-cdr!
          (lambda (new-value)
            (setf y new-value))))
    (let ((dispatch
            (lambda (m)
              (cond ((eq m 'car) x)
                    ((eq m 'cdr) y)
                    ((eq m 'set-car!) set-car!)
                    ((eq m 'set-cdr!) set-cdr!)
                    (t (error (format nil "Undefined operation -- cons ~A"
                                      m)))))))
      dispatch)))
(defun my-car (z)
  (funcall z 'car))
(defun my-cdr (z)
  (funcall z 'cdr))
(defun set-car! (z new-value)
  (funcall (funcall z 'set-car!) new-value))
(defun set-cdr! (z new-value)
  (funcall (funcall z 'set-cdr!) new-value))
