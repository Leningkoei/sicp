;;;; 2-14
;;;; 2-1-4
;;;; 2022/03/29

;;; Demonstrate that Lem is right. Investigate the behavior of the system on a
;;; variety of arithmetic expressions. Make some intervals `A` and `B`, and use
;;; them in computing the expression `A/A` and `A/B`. You will get the most
;;; insight by using intervals whose width is a small percentage of the center
;;; value. Examine the results of the computation in center-percent form (see
;;; exercise 2.12).

(load "2-12.scm")

(define par1 (lambda (r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2))))
(define par2 (lambda (r1 r2)
  (define one (make-interval 1 1))
  (div-interval one
                (add-interval (div-interval one r1)
                              (div-interval one r2)))))

(define (test)
  (define r1 (make-interval 1 3))
  (define r2 (make-interval 3 5))
  (display (par1 r1 r2))
  (newline)
  (display (par2 r1 r2))
  (newline)
  (define _r1 (make-center-percent 2 50))
  (define _r2 (make-center-percent 4 25))
  (display (par1 r1 r2))
  (newline)
  (display (par2 r1 r2))
  (newline))

