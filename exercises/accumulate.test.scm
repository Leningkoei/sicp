(load "accumulate.scm")
(load "print.scm")

(define (test)
  (print (accumulate + 0 (list 0 1 2 3))))

