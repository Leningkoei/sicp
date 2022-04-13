(defpackage Vector
  (:use common-lisp)
  (:export new get-x get-y add sub scale))
(in-package Vector)

(defun new (x y)
  "Number -> Number -> Vector"
  (cons x y))
(defun get-x (this)
  "Vector -> Number"
  (car this))
(defun get-y (this)
  "Vector -> Number"
  (cdr this))
(defun add (a b)
  "Vector -> Vector -> Vector"
  ((lambda (a.x a.y b.x b.y)
    (new (+ a.x b.x) (+ a.y b.y)))
   (get-x a) (get-y a) (get-x b) (get-y b)))
(defun sub (a b)
  "Vector -> Vector -> Vector"
  ((lambda (a.x a.y b.x b.y)
    (new (- a.x b.x) (- a.y b.y)))
   (get-x a) (get-y a) (get-x b) (get-y b)))
(defun scale (param vector)
  "Number -> Vector -> Vector"
  ((lambda (vector.x vector.y)
    (new (* param vector.x) (* param vector.y)))
   (get-x vector) (get-y vector)))

