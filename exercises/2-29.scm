;;;; 2-29
;;;; 2-2-2
;;;; 2022/04/03

;;; A binary mobile consists of two branches, a left branch and a right branch.
;;; Each branch is a rod of a certain length, from which hangs either a weight
;;; or another binary mobile. We can represent a binary mobile using compound
;;; data by constructing it from two branches (for example, using `list`):

(define make-mobile (lambda (left right)
  (list left right)))

;;; A branch is constructed from a `length` (which must be a number)

;;;   key: アンナ、[ ミズキ、ノイム、ネネカ (two of three) ]
;;; value: ユキ
;;;     *: ナナか
;;;    **: キャル、カスミ
;;;   ***: ミサキ
;;;  ****: ハツネ
;;; *****: イリや、キョウか

