;;;; 2-59
;;;; 2-3-3
;;;; 2022/04/25

;;; Implement the `union-set` operation for the unordered-list representation
;;; of sets.

(defun union-set (set1 set2)
  "Computes the union of two sets, which is the set containing each element that appears in either argument."
  (if (null set1) set2
    (let ((element (car set1)))
      (if (element-of-set? element set2)
          (union-set (cdr set1) set2)
        (union-set (cdr set1) (cons element set2))))))
