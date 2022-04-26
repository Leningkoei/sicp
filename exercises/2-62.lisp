;;;; 2-61
;;;; 2-3-3
;;;; 2022/04/26

;;; Give a O(n) implementation of `union-set` for sets represented as ordered
;;; lists.

(defun union-set (set1 set2)
  "Computes the union of two sets, which is the set containing each element that appears in either argument."
  (cond ((null set1) set2)
        ((null set2) set1)
        (t (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                   ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                   ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))
