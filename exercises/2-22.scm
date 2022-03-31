;;;; 2-22
;;;; 2-2-1
;;;; 2022/03/31

;;; Louis Reasoner tries to rewrite the first `square-list` procedure of
;;; exercise 2.21 so that it evolves an iterative process:

(define square-list (lambda (items)
  (define iterator (lambda (things answer)
    (display answer)
    (newline)
    (if (null? things)
      answer
      (iterator (cdr things)
                (cons (square (car things)) answer)))))
  (iterator items '())))

;;; Unfortunately, defining `square-list` this way produces the answer list in
;;; the reverse order of the one desired. Why?

;;; Louis then tries to fix his bug by interchanging the arguments to cons:

(define square-list-kai (lambda (items)
  (define iterator (lambda (things answer)
    (if (null? things)
      answer
      (iterator (cdr things)
                (cons answer (square (car things)))))))
  (iterator items '())))

;;; This doesn't work either. Explain.

(define (test)
  (define items (list 0 1 2 3))
  (display (square-list items))
  (newline)
  (display (square-list-kai items))
  (newline))

;;; Procedures of CONStructor is a stack structure?

