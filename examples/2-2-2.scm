;;;; 2-2-2: Hierarchical Structures
;;;; 2022/04/01

(define print (lambda (content)
  (display content)
  (newline)))

(define my-length (lambda (items)
  (if (null? items)
    0
    (1+ (my-length (cdr items))))))
(define count-leaves (lambda (items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items)) (count-leaves (cdr items)))))))

;;; Mapping over trees

(define scale-tree (lambda (tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor))))))
(define scale-tree-kai (lambda (tree factor)
  (define procedure (lambda (sub-tree)
    (if (pair? sub-tree)
        (scale-tree-kai sub-tree factor)
        (* sub-tree factor))))
  (map procedure tree)))

(define (test)
  (define items (list (list 0 1) (list 2 3)))
  (print (my-length items))
  (print (count-leaves items))
  (print (scale-tree items 10))
  (print (scale-tree-kai items 10)))

