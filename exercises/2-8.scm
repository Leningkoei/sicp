;;;; 2-8
;;;; 2-1-4
;;;; 2022/03/29

;;; Using reasoning analogous to Alyssa's, describe how the difference of two
;;; intervals may be computed. Define a corresponding subtraction procedure,
;;; called `sub-interval`.

(load "2-7.scm")

(define sub-interval (lambda (x y)
  (define y.opposite (make-interval (- (upper-bound y))
                                    (- (lower-bound y))))
  (add-interval x y.opposite)))

(define test (lambda ()
  (define a (make-interval 1 2))
  (define b (make-interval 3 4))
  (display (sub-interval a b))
  (newline)))

