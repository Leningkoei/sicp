;;;; 2-1-4 Interval Arithmetic
;;;; 2022/03/29

;;;

(define add-interval (lambda (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y)))))
(define mul-interval (lambda (x y)
  (define x.lower (lower-bound x))
  (define y.lower (lower-bound y))
  (define x.upper (upper-bound x))
  (define y.upper (upper-bound y))
  (define p1 (* x.lower y.lower))
  (define p2 (* x.lower y.upper))
  (define p3 (* x.upper y.lower))
  (define p4 (* x.upper y.upper))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))
(define div-interval (lambda (x y)
  (define y.reciprocal (make-interval (/ 1.0 (upper-bound y))
                                      (/ 1.0 (lower-bound y))))
  (mul-interval x y.reciprocal)))

;;;

(define make-center-width (lambda (c w)
  (make-interval (- c w) (+ c w))))
(define center (lambda (i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)))
(define width (lambda (i)
  (/ (- (upper-bound i) (lower-bound i)) 2)))

