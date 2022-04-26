;;;; 2-60
;;;; 2-3-3
;;;; 2022/04/25

;;; We specified that a set would be represented as a list with no
;;; duplicates. Now suppose we allow duplicates. For instance, the let `{ 1, 2,
;;; 3 }` could be represented as the list `(2 3 2 1 3 2 2)`. Design procedures
;;; `element-of-set?`, `adjoin-set`, `union-set`, `intersection-set` that
;;; operate on this representation. How does the efficiency of each compare
;;; with the corresponding procedure for the non-duplicate representation? Are
;;; there applications for which you would use this presentation in preference
;;; to the non-duplicate one?

(defun element-of-set? (x set)
  "Determines whether a given element is a member of a set."
  (cond ((null set) nil)
        ((equal x (car set)) t)
        (t (element-of-set? x (cdr set)))))
(defun adjoin-set (x set)
  "Returns a set that contains the elements of the original set and also the adjoined element."
  (cons x set))
(defun intersection-set (set1 set2)
  "Computes the intersection of two sets, which is the set containing only elements that appear in both arguments."
  (cond ((or (null set1) (null set2)) nil)
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (t (intersection-set (cdr set1) set2))))
(defun union-set (set1 set2)
  "Computes the union of two sets, which is the set containing each element that appears in either argument."
  (append set1 set2))

(defun test ()
  (let ((set1 (list 0 0 1 1 2 2 3 3)) (set2 (list 1 1 2 2 3 3 4 4)))
    (print (element-of-set? 0 set1))
    (print (element-of-set? 0 set2))
    (print (adjoin-set 4 set1))
    (print (adjoin-set 0 set2))
    (print (intersection-set set1 set2))
    (print (union-set set1 set2))
    nil))
