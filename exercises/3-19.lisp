;;;; 3-19
;;;; 3-3-1
;;;; 2022/05/25

;;; Redo exercise 3.18 using an algorithm that takes only a constant amount of
;;; space. (This requires a very clever idea.)

(defun check-cycle (cycle)
  (labels
      ((iterator (slow fast)
         (cond ((or (null slow) (null fast)) nil)
               ((eq slow fast) t)
               (t (iterator (cdr slow) (cddr fast))))))
    (iterator cycle (cdr cycle))))
