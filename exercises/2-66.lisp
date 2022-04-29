;;;; 2-66
;;;; 2-3-3
;;;; 2022/04/29

;;; Implement the `lookup` procedure for the case where the set of records is
;;; structured as a binary tree, ordered by the numerical values of the keys.

(defun lookup (given-key set)
  (if (null set) nil
    (let ((entry (tree-entry set)))
      (let ((key (key entry)))
        (cond ((= given-key key) entry)
              ((< given-key key) (lookup given (left-branch set)))
              ((> given-key key) (lookup given (right-branch set))))))))
