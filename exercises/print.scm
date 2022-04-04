(define (print . contents)
  (for-each (lambda (content) (display content) (display " ")) contents))

