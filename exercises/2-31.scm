;;;; 2-31
;;;; 2-2-2
;;;; 2022/04/04

;;; Abstract your answer to exercise 2.30 to procedure a procedure `tree-map`
;;; with the property that `square-tree` could be defined as

; (define square-tree (lambda (tree-map square tree)))

(load "print.scm")

(define tree.map:Tree (lambda (procedure:Function this:Tree)
  (map (lambda (sub-tree:Tree/Number)
    (if (pair? sub-tree:Tree/Number)
      (tree.map:Tree procedure:Function sub-tree:Tree/Number)
      (procedure:Function sub-tree:Tree/Number))) this:Tree)))

(define (test)
  (define procedure square)
  (define tree:Tree (list 1
                     (list 2 (list 3 4) 5)
                     (list 6 7)))
  (tree.map:Tree procedure tree:Tree))

