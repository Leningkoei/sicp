(defpackage Frame
  (:use common-lisp)
  (:export new origin a b))

(defun new (origin a b)
  "Number -> Number -> Number -> Frame"
  ((lambda (a-b)
    (cons origin a-b))
   (cons a b)))
(defun get-origin (this)
  "Frame -> Number"
  (car this))
(defun get-a (this)
  "Frame -> Number"
  (cadr this))
(defun get-b (this)
  "Frame -> Number"
  (cddr this))

