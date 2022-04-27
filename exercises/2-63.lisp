;;;; 2-63
;;;; 2-3-3
;;;; 2022/04/27

;;; Each of the following two procedures converts a binary tree to list.
(defun tree->list-1 (tree)
  (if (null tree) nil
    (append (tree->list-1 (tree-left-branch tree))
            (cons (tree-entry tree)
                  (tree->list-1 (tree-right-branch tree))))))
(defun tree->list-2 (tree)
  (labels
   (copy-to-list (tree result-list)
                 (if (null tree) result-list
                   (copy-to-list (tree-left-branch tree)
                                 (cons (tree-entry tree)
                                       (copy-to-list (tree-right-branch tree)
                                                     result-list)))))))
;;; a. Do the two procedures produce the same result for every tree? If not,
;;; how do the results differ? What lists do the two procedures produce for the
;;; trees in figure 2.16?
;;; b. Do the two procedures have the same order of growth in the number of
;;; steps required to convert a balanced tree with `n` elements to a list? If
;;; not, which one grows more slowly?
