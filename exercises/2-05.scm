;;;; 2-05
;;;; 2-1-3
;;;; 2022/03/27

;;; Show that we can represent pares of nonnegative integers using only numbers
;;; and arithmetic operations if we represent the pair `a` and `b` as the
;;; integer that is the product `2 ** a * 3 ** b`. Give the corresponding
;;; corresponding definitions of the procedures `cons`, `car`, and `cdr`.

(define my-cons (lambda (a b)
  (* (expt 2 a) (expt 3 b))))
(define iterator-factory (lambda (root)
  (define iterator (lambda (i result)
    (define next (/ result root))
    (if (integer? next)
      (iterator (1+ i) next)
      i)))
  iterator))
(define my-car (lambda (pair)
  (define this-iterator (iterator-factory 2))
  (this-iterator 0 pair)))
(define my-cdr (lambda (pair)
  (define this-iterator (iterator-factory 3))
  (this-iterator 0 pair)))

(define test (lambda ()
  (define a 0)
  (define b 99)
  (define ab (my-cons a b))
  (display (my-car ab))
  (newline)
  (display (my-cdr ab))
  (newline)))

