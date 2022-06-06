(defpackage :agenda
  (:use :common-lisp)
  (:export
   :todo))
(in-package :agenda)

(defun make ()
  "MAKE
  nil -> agenda
  Returns a new empty agenda."
  (cons 0 nil))
(defun current-time (this)
  "CURRENT-TIME
  agenda -> number
  is true if the specified agenda is empty."
  (car this))
(defun set-current-time! (this time)
  "SET-CURRENT-TIME!
  agenda -> number -> agenda"
  (rplaca this time)
  this)
(defun segment (this)
  "SEGMENT
  agenda -> list<??>"
  (cdr this))
(defun set-segment! (this segment)
  "SET-SEGMENT!
  agenda -> list<??> -> agenda"
  (rplacd this segment)
  agenda)
(defun first-segment (this)
  "FIRST-SEGMENT
  agenda -> ??
  Returns the first item on the agenda."
  (car (segment this)))
(defun rest-segment (this)
  "REST-SEGMENT
  agenda -> list<??>
  Returns the list of rest items on the agenda."
  (cdr (segment this)))
