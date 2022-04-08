;;;; 2-39
;;;; 2-2-3
;;;; 2022/04/08

(load "../stdp.scm")
(define (reload)
  (load "2-39.scm"))

;;; Complete the following definitions of `reverse` (exercise 2.18) in terms of
;;; `fold-right` and `fold-left` from exercise 2.38:
;;; (define (reverse sequence) (fold-right (lambda (x y) <??>) nil sequence))
;;; (define (reverse sequence) (fold-left  (lambda (x y) <??>) nil sequence))

(define Sequence.reverse:Sequence (lambda (this:Sequence)
  (define iterator:Sequence (lambda (result:Sequence rest:Sequence)
    (if (null? rest:Sequence)
      result:Sequence
      (iterator:Sequence (cons (car rest:Sequence) result:Sequence)
                         (cdr rest:Sequence)))))
  (iterator:Sequence nil this:Sequence)))

(define Sequence.push:Sequence (lambda (this:Sequence item)
  (append this:Sequence (list item))))
(define Sequence.reverse-fold-right-ver:Sequence (lambda (this:Sequence)
  ; (define op (lambda (x y)
  ;   (append y (list x))))
  (define op (lambda (current:Number result:Sequence)
    (Sequence.push:Sequence result:Sequence current:Number)))
  (fold-right op nil this:Sequence)))
(define cons-alter (lambda (a b)
  (cons b a)))
(define Sequence.reverse-fold-left-ver:Sequence (lambda (this:Sequence)
  (fold-left cons-alter nil this:Sequence)))

(define (test)
  (define sequence:Sequence (list 0 1 2 3))
  (print (Sequence.reverse:Sequence sequence:Sequence))
  (print (Sequence.reverse-fold-right-ver:Sequence sequence:Sequence))
  (print (Sequence.reverse-fold-left-ver:Sequence  sequence:Sequence)))

