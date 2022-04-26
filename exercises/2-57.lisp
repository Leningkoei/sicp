;;;; 2-57
;;;; 2-3-2
;;;; 2022/04/25

;;; Extend the differentiation program to handle sums and products of arbitrary
;;; numbers of (two or more) terms. Then the last example above could be
;;; expressed as
;;; (deriv '(* x y (+ x 3)) 'x)
;;; Try to do this by changing only the representation for sums and products,
;;; without changing the `deriv` procedure at all. For example, the `addend` of
;;; a sum would be the first term, and the `augend` would be the sum of the
;;; rest of the terms.

(defun make-sum-kai (addend &rest augends)
  ""
  (reduce 'make-sum (cons addend augends)))
(defun make-product-kai (multiplier &rest multiplicands)
  ""
  (reduce 'make-product (cons multiplier multiplicands)))
