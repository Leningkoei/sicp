;;;; 2-46
;;;; 2-2-4
;;;; 2022/04/11

(load "../stdp.scm")
(define (reload)
  (load "2-46.scm"))

;;; A two-dimensional vector v running from the origin to a point can be
;;; represented as a pair consisting of an `x`-coordinate and a `y`-coordinate.
;;; Implement a data abstraction for vectors by giving a constructor `make-vect`
;;; and corresponding selectors `xcor-vect` and `ycor-vect`. In terms of your
;;; selectors and constructor, implement procedures `add-vect`, `sub-vect`, and
;;; `scale-vect` that perform the operations vector addition, vector
;;; subtraction, and multiplying a vector by a scalar:
;;; (x_1, y_1) + (x_2, y_2) = (x_1 + x_2, y_1 + y_2)
;;; (x_1, y_1) - (x_2, y_2) = (x_1 - x_2, y_1 - y_2)
;;;              s * (x, y) = (s x, s y)

(define make-vect (lambda (xcor-vect ycor-vect)
  (cons xcor-vect ycor-vect)))

(define xcor-vect (lambda (vect)
  (car vect)))
(define ycor-vect (lambda (vect)
  (cdr vect)))

(define add-vect (lambda (vect1 vect2)
  (define vect1.x (xcor-vect vect1))
  (define vect1.y (ycor-vect vect1))
  (define vect2.x (xcor-vect vect2))
  (define vect2.y (ycor-vect vect2))
  (make-vect (+ vect1.x vect2.x) (+ vect1.y vect2.y))))
(define sub-vect (lambda (vect1 vect2)
  (define vect1.x (xcor-vect vect1))
  (define vect1.y (ycor-vect vect1))
  (define vect2.x (xcor-vect vect2))
  (define vect2.y (ycor-vect vect2))
  (make-vect (- vect1.x vect2.x) (- vect1.y vect2.y))))
(define scale-vect (lambda (s vect)
  (define vect.x (xcor-vect vect))
  (define vect.y (ycor-vect vect))
  (make-vect (* s vect.x) (* s vect.y))))

(define (test)
  (define vect1 (make-vect 1 2))
  (define vect2 (make-vect 3 4))
  (print (add-vect vect1 vect2))
  (print (sub-vect vect1 vect2))
  (print (scale-vect 2 vect1)))

