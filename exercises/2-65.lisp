;;;; 2-65
;;;; 2-3-3
;;;; 2022/04/29

;;; Use the results of exercises 2.63 and 2.64 to give `O(n)` implementations
;;; of `union-set` and `intersection-set` for sets implemented as (balanced)
;;; binary trees.

(defun union-set (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        (t (let ((entry1 (tree-entry set1))
                 (entry2 (tree-entry set2))
                 (left-branch-1 (tree-left-branch set1))
                 (left-branch-2 (tree-left-branch set2))
                 (right-branch-1 (tree-right-branch set1))
                 (right-branch-2 (tree-right-branch set2)))
             (cond ((= entry1 entry2)
                    (make-tree entry1
                               (union-set left-branch-1 left-branch-2)
                               (union-set right-branch-1 right-branch-2)))
                   ((< entry1 entry2)
                    (make-tree entry1
                               left-branch-1
                               (union-set (right-branch-1 set2))))
                   ((> entry1 entry2)
                    (make-tree entry1
                               (union-set (left-branch-1 set2))
                               right-branch-1)))))))
(defun intersection-set (set1 set2)
  (if (or (null set1) (null set2)) nil
    (let ((entry1 (tree-entry set1))
          (entry2 (tree-entry set2))
          (left-branch-1 (tree-left-branch set1))
          (left-branch-2 (tree-left-branch set2))
          (right-branch-1 (tree-right-branch set1))
          (right-branch-2 (tree-right-branch set2)))
      (cond ((= entry1 entry2)
             (make-tree entry1
                        (intersection-set left-branch-1 left-branch2)
                        (intersection-set right-branch-1 right-branch-2)))
            ((< entry1 entry2)
             (union-set (intersection-set right-branch-1
                                          (make-tree entry2 nil right-branch-2))
                        (intersection-set (make-tree entry1 left-branch-1 nil)
                                          left-branch-2)))
            ((> entry1 entry2)
             (union-set (intersection-set left-branch-1
                                          (make-tree entry2 left-branch-2 nil))
                        (intersection-set (make-tree entry1 nil right-branch-1)
                                          right-branch-2)))))))
