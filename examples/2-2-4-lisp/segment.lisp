(defpackage Segment
  (:use common-lisp)
  (:export new get-start get-end))
(in-package Segment)

(defun new (start end)
  "Vector -> Vector -> Segment"
  (cons start end))
(defun get-start (this)
  "Segment -> Vector"
  (car this))
(defun get-end (this)
  "Segment -> Vector"
  (cdr this))

