;;;; 2-35
;;;; 2-2-3
;;;; 2022/04/08

(define (reload)
  (load "2-35.scm"))
(load "print.scm")

;;; Redefine `count-leaves` from section 2.2.2 as an accumulation:

(load "accumulate.scm")

(define Tree.count-leaves:Number (lambda (this:Tree)
  (define procedure:Number (lambda (sub-tree:Tree/Number)
    (if (pair? sub-tree:Tree/Number)
      (Tree.count-leaves:Number sub-tree:Tree/Number)
      1)))
  (accumulate + 0 (map procedure:Number this:Tree))))

(define (test)
  (define tree:Tree (list (list 1 2) (list 3 4)))
  (print (Tree.count-leaves:Number tree:Tree))
  (print (Tree.count-leaves:Number (list tree:Tree tree:Tree))))

