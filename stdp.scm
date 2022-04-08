;;;; 2022/04/08

(define (print . contents)
  (for-each (lambda (content) (display content) (display " ")) contents)
  (newline))

(define nil '())

(define accumulate fold-right)

