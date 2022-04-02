;;;; 2-28
;;;; 2-2-2
;;;; 2022/04/02

(define print (lambda (content)
  (display content)
  (newline)))

;;; Write a procedure `fringe` that takes as argument a tree (represented as a
;;; list) and returns a list whose elements are all the leaves of the tree
;;; arranged in left-to-right order. For example,

; (define x (list (list 1 2) (list 3 4)))
; ;; (1 2 3 4)
; (fringe x)
; ;; (1 2 3 4 1 2 3 4)
; (fringe (list x x))

(define fringe (lambda (items)
  (cond ((null? items) '())
        ((pair? items) (append (fringe (car items)) (fringe (cdr items))))
        (else (list items)))))

(define test (lambda ()
  (define x (list (list 1 2) (list 3 4)))
  (define x^2 (list x x))
  (define x^4 (list x^2 x^2))
  (print (fringe x))
  (print (fringe x^2))
  (print (fringe x^4))))

