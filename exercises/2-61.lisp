;;;; 2-61
;;;; 2-3-3
;;;; 2022/04/26

;;; Given an implementation of `adjoin-set` using the ordered
;;; representation. By analogy with `element-of-set?` show how to take
;;; advantage of the ordering to produce a procedure that requires on the
;;; average about half as many steps as with the unordered representation.

(defun adjoin-set (x set)
  "Returns a set that contains the elements of the original set and also the adjoined element."
  (labels
   ((iterator (rest)
              (let ((cur (car rest)))
                (cond ((< x cur) (cons x rest))
                      ((= x cur) rest)
                      ((> x cur) (cons cur (iterator (cdr rest))))))))
   (iterator set)))
