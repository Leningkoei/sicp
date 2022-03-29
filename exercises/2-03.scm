;;;; 2-03
;;;; 2-1-2
;;;; 2022/03/27

;;; Implement a representation for rectangles in a plane. (Hint: You may want to
;;; make use of exercise 2.2.) In terms of your constructors and selectors,
;;; create procedures that compute the perimeter and the area of a given
;;; rectangle. Now implement a different representation for rectangles. Can you
;;; design your system with suitable abstraction barriers, so that the same
;;; perimeter and area procedures will work using either representation?

(define print (lambda (content)
  (display content)
  (newline)))
(define distance (lambda (a b)
  (abs (- a b))))

(define make-point (lambda (x-point y-point)
  (cons x-point y-point)))
(define x-point (lambda (point)
  (car point)))
(define y-point (lambda (point)
  (cdr point)))
(define point-distance (lambda (a b)
  (define a-x (x-point a))
  (define a-y (y-point a))
  (define b-x (x-point b))
  (define b-y (y-point b))
  (define x-distance (distance a-x b-x))
  (define y-distance (distance a-y b-y))
  (sqrt (+ (square x-distance) (square y-distance)))))

(define make-segment (lambda (start-point end-point)
  (cons start-point end-point)))
(define start-point (lambda (segment)
  (car segment)))
(define end-point (lambda (segment)
  (cdr segment)))

;;; d----c
;;; |    |
;;; a----b

(define make-rectangle (lambda (a b c d)
  (define ab (cons a b))
  (define cd (cons c d))
  (cons ab cd)))
(define get-a (lambda (rectangle)
  (define ab (car rectangle))
  (car ab)))
(define get-b (lambda (rectangle)
  (define ab (car  rectangle))
  (cdr ab)))
(define get-c (lambda (rectangle)
  (define cd (cdr rectangle))
  (car cd)))
(define get-d (lambda (rectangle)
  (define cd (cdr rectangle))
  (cdr cd)))
(define perimeter (lambda (rectangle)
  (define a (get-a rectangle))
  (define b (get-b rectangle))
  (define c (get-c rectangle))
  (define d (get-d rectangle))
  (define ab (point-distance a b))
  (define bc (point-distance b c))
  (define cd (point-distance c d))
  (define da (point-distance d a))
  (+ ab bc cd da)))
(define area (lambda (rectangle)
  (define a (get-a rectangle))
  (define b (get-b rectangle))
  (define d (get-d rectangle))
  (define ab (point-distance a b))
  (define ad (point-distance a d))
  (* ab ad)))

(define test (lambda ()
  (define a (make-point 0 0))
  (define b (make-point 2 0))
  (define c (make-point 2 2))
  (define d (make-point 0 2))
  (define abcd (make-rectangle a b c d))
  (print (perimeter abcd))
  (print (area abcd))))

