;;; The agenda is made up of `time segments`. Each time segment is a pair
;;; consisting of a number (the time) and a queue (see exercise 3.32) that holds
;;; the procedures that are scheduled to run during that time segment.
;;;   scheduled: Included in or planned according to a schedule.
;;;     schedule: A plan for carrying out a process or procedure, giving lists
;;;       of intended events and times.

(in-package :common-lisp-user)
(defpackage :time-segment
  (:use common-lisp))
(in-package :time-segment)

(defun make-time-segment (time queue)
  (cons time queue))
(defun time-segment-time (this)
  (car this))
(defun time-segment-queue (this)
  (cdr this))

(export 'make-time-segment)
(export 'time-segment-time)
(export 'time-segment-queue)
