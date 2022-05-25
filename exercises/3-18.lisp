;;;; 3-18
;;;; 3-3-1
;;;; 2022/05/25

;;; Write a procedure that examines a list and determines whether it contains a
;;; cycle, that is, whether a program that tried to find the end of the list by
;;; taking successive `cdrs` would go into an infinite loop. Exercise 3.13
;;; constructed such lists.

(defun check-cycle (cycle)
  (labels
      ((iterator (slow fast)
         (if (or (null slow) (null fast))
             nil
             (if (eq slow fast)
                 t
                 (iterator (cdr slow) (cddr fast))))))
    (iterator cycle (cdr cycle))))

(defun make-cycle (list)
  (rplacd (last list) list)
  list)
(defun test ()
  (let ((list (list 'a 'b 'c 'd)))
    (let ((cycle (make-cycle list)))
      (check-cycle cycle))))
