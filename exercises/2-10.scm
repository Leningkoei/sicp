;;;; 2-10
;;;; 2-1-4
;;;; 2022/03/29

;;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder
;;; and comments that it is not clear what it means to divide by an interval
;;; that spans zero. Modify Alyssa's code to check for this condition and to
;;; signal an error if it occurs.

(load "2-08.scm")

(define save-div-interval (lambda (a b)
  (define b.lower (lower-bound b))
  (define b.upper (upper-bound b))
  (define get-b.reciprocal (lambda ()
    (make-interval (/ 1.0 b.upper)
                   (/ 1.0 b.lower))))
  (if (and (not (positive? b.lower))
           (not (negative? b.upper)))
    (error "You are trying to divide by an interval that spans zero!")
    (mul-interval a (get-b.reciprocal)))))

(define test (lambda ()
  (define a (make-interval 2 3))
  (define b (make-interval 0 1))
  (save-div-interval a b)))

