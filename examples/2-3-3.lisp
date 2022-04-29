;;;; 2-3-3: Representing Sets
;;;; 2022/04/25

;;; Set as unordered lists

(in-package common-lisp-user)
(defpackage unordered-set
  (:use    common-lisp)
  (:export element-of-set?
           adjoin-set
           intersection-set
           union-set
           test))
(in-package unordered-set)

(defun element-of-set? (x set)
  "Determines whether a given element is a member of a set."
  (cond ((null set) nil)
        ((equal x (car set)) t)
        (t (element-of-set? x (cdr set)))))
(defun adjoin-set (x set)
  "Returns a set that contains the elements of the original set and also the adjoined element."
  (if (element-of-set? x set) set
    (cons x set)))
(defun intersection-set (set1 set2)
  "Computes the intersection of two sets, which is the set containing only elements that appear in both arguments."
  (cond ((or (null set1) (null set2)) nil)
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (t (intersection-set (cdr set1) set2))))
(defun union-set (set1 set2)
  "Computes the union of two sets, which is the set containing each element that appears in either argument."
  (if (null set1) set2
    (let ((element (car set1)))
      (if (element-of-set? element set2)
          (union-set (cdr set1) set2)
        (union-set (cdr set1) (cons element set2))))))

(defun test ()
  (let ((set1 (list 0 1 2 3)) (set2 (list 1 2 3 4)))
    (print (element-of-set? 0 set1))
    (print (element-of-set? 0 set2))
    (print (adjoin-set 4 set1))
    (print (adjoin-set 0 set2))
    (print (intersection-set set1 set2))
    (print (union-set set1 set2))
    nil))

;;; Set as ordered lists

(in-package common-lisp-user)
(defpackage ordered-set
  (:use    common-lisp)
  (:export element-of-set?
           adjoin-set
           intersection-set
           union-set
           test))
(in-package ordered-set)

(defun element-of-set? (x set)
  "Determines whether a given element is a member of a set."
  (cond ((null set) nil)
        ((= x (car set)) t)
        ((< x (car set)) nil)
        (t (element-of-set? x (cdr set)))))
(defun adjoin-set (x set)
  "Returns a set that contains the elements of the original set and also the adjoined element."
  (labels
   ((iterator (rest)
              (if (null rest) (list x)
                (let ((cur (car rest)))
                  (cond ((< x cur) (cons x rest))
                        ((= x cur) rest)
                        ((> x cur) (cons cur (iterator (cdr rest)))))))))
   (iterator set)))
(defun intersection-set (set1 set2)
  "Computes the intersection of two sets, which is the set containing only elements that appear in both arguments."
  (if (or (null set1) (null set2)) nil
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2) (intersection-set (cdr set1) set2))
            ((> x1 x2) (intersection-set set1 (cdr set2)))))))
(defun union-set (set1 set2)
  "Computes the union of two sets, which is the set containing each element that appears in either argument."
  (cond ((null set1) set2)
        ((null set2) set1)
        (t (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                   ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                   ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))
(defun test ()
  (let ((set1 (list 0 1 2 3)) (set2 (list 1 2 3 4)))
    (print (element-of-set? 0 set1))
    (print (element-of-set? 0 set2))
    (print (adjoin-set 4 set1))
    (print (adjoin-set 0 set2))
    (print (intersection-set set1 set2))
    (print (union-set set1 set2))
    nil))

;;; Sets as binary trees

(in-package common-lisp-user)
(defpackage binary-tree-set
  (:use    common-lisp)
  (:export test))
(in-package binary-tree-set)

(defun make-tree (entry left-branch right-branch)
  (list entry left-branch right-branch))
(defun tree-entry (tree)
  (car tree))
(defun tree-left-branch (tree)
  (cadr tree))
(defun tree-right-branch (tree)
  (caddr tree))
(defun tree->list (tree)
  (labels
   (copy-to-list (tree result-list)
                 (if (null tree) result-list
                   (copy-to-list (tree-left-branch tree)
                                 (cons (tree-entry tree)
                                       (copy-to-list (tree-right-branch tree)
                                                     result-list)))))))
(defun list->tree (elements)
  (labels
   (quotient
    (a b)
    (let ((c (mod a b)))
      (let ((d (- a c)))
        (/ d b))))
   (partial-tree
    (elts n)
    (if (= 0 n) (cons nil elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remain-tree (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
   (car (partial-tree elements (length elements)))))

(defun element-of-set? (x set)
  "Determines whether a given element is a member of a set."
  (cond ((null set) nil)
        ((= x (tree-entry set)) t)
        ((< x (tree-entry set)) (element-of-set? x (tree-left-branch set)))
        ((> x (tree-entry set)) (element-of-set? x (tree-right-branch set)))))
(defun adjoin-set (x set)
  (if (null set) (make-tree x nil nil)
    (let ((entry (tree-entry set)))
      (cond ((= x entry) (make-tree x nil nil))
            ((< x entry) (make-tree entry
                                    (adjoin-set x (tree-left-branch set))
                                    (tree-right-branch set)))
            ((> x entry) (make-tree entry
                                    (tree-left-branch set)
                                    (adjoin-set x (tree-right-branch set))))))))
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

;;; Sets and information retrieval

(defun lookup (given-key set-of-records)
  (cond ((null set-of-records) nil)
        ((equal given-key (key (car set-of-records)))
         (car set-of-records))
        (t (lookup given-key (cdr set-of-records)))))

(in-package common-lisp-user)
(defun test ()
  (unordered-set:test)
  (ordered-set:test)
  nil)
