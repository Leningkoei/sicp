;;;; 2-2-2: Hierarchical Structures
;;;; 2022/04/01

(define my-length (lambda (items)
  (if (null? items)
    0
    (1+ (my-length (cdr items))))))
(define count-leaves (lambda (items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items)) (count-leaves (cdr items)))))))

(define (test)
  (define items (list (list 0 1) (list 2 3)))
  (display (my-length items))
  (newline)
  (display (count-leaves items))
  (newline))

