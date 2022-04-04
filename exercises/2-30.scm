;;;; 2-30
;;;; 2-2-2
;;;; 2022/04/04

(define print (lambda (content)
  (display content)
  (newline)))

;;; Define a procedure `square-tree` analogous to the `square-list` procedure of
;;; exercise 2.21. That is, `square-list` should behave as follows:

; (square-tree (list 1
;                    (list 2 (list 3 4) 5)
;                    (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

;;; Define `square-tree` both directly (i.e., without using any higher-order
;;; procedures) and also by using `map` and recursion.

(define tree.square:Tree (lambda (this:Tree/Number)
  (cond ((null? this:Tree/Number) '())
        ((not (pair? this:Tree/Number)) (square this:Tree/Number))
        (else (cons (tree.square:Tree (car this:Tree/Number))
                    (tree.square:Tree (cdr this:Tree/Number)))))))
(define tree.square-map-ver:Tree (lambda (this:Tree)
  (define procedure (lambda (sub-tree:Tree/Number)
    (if (pair? sub-tree:Tree/Number)
      (tree.square-map-ver:Tree sub-tree:Tree/Number)
      (square sub-tree:Tree/Number))))
  (map procedure this:Tree)))

;;; It's same with 2.2.2 "Mapping over trees" totally.

(define test (lambda ()
  (define tree:Tree (list 1
                          (list 2 (list 3 4) 5)
                          (list 6 7)))
  (print (tree.square:Tree tree:Tree))
  (print (tree.square-map-ver:Tree tree:Tree))))

