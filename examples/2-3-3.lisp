;;;; 2-3-3: Representing Sets
;;;; 2022/04/25

;;; Set as unordered lists

(in-package common-lisp-user)
(defpackage unordered-set
  (:use :common-lisp)
  (:export :element-of-set?
           :adjoin-set
           :intersection-set
           :union-set
           :test))
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
  (:use :common-lisp)
  (:export :element-of-set?
           :adjoin-set
           :intersection-set
           :union-set
           :test))
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
(defun test ()
  (unordered-set:test)
  (ordered-set:test)
  nil)
