(load "3-3-4.queue.lisp")
(load "3-3-4.time-segment.lisp")

(defpackage :agenda
  (:use :common-lisp))
(in-package :agenda)

;; (use-package :time-segment)
;; (use-package :queue)

(import 'time-segment:make-time-segment)
(import 'time-segment:time-segment-time)
(import 'time-segment:time-segment-queue)

(import 'queue:make-queue)
(import 'queue:queue-empty?)
(import 'queue:queue-front)
(import 'queue:queue-insert!)
(import 'queue:queue-delete!)

(defun make ()
  "MAKE
  _ -> agenda
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
(defun time-segments (this)
  "TIME-SEGMENTS
  agenda -> sequence<time-segment>"
  (cdr this))
(defun set-time-segments! (this time-segments)
  "SET-TIME-SEGMENTS!
  agenda -> sequence<time-segment> -> agenda"
  (rplacd this time-segments)
  this)
(defun first-time-segment (this)
  "FIRST-SEGMENT
  agenda -> time-segment
  Returns the first item on the agenda."
  (car (time-segments this)))
(defun rest-time-segments (this)
  "REST-TIME-SEGMENTS
  agenda -> sequence<time-segment>
  Returns the list of rest items on the agenda."
  (cdr (time-segments this)))
(defun agenda-empty? (this)
  (null (time-segments this)))

(defun add-to-agenda! (this time action)
  "ADD-TO-AGENDA
  agenda -> number -> procedure
  To add an action to an agenda, we first check if the agenda is empty. If so,
  we create a time segment for the action and install this in the
  agenda. Otherwise, we scan the agenda, examining the time of each segment. If
  we find a segment for our appointed time, we add the action to the associated
  queue. If we reach a time later than the one to which we are appointed, we
  insert a new time segment into the agenda just before it. If we reach the end
  of the agenda, we must create a new time segment at the end."
  (flet ((belongs-before? (time-segments)
           (or (null time-segments)
               (< time (time-segment-time (car time-segments)))))
         (make-new-time-segment (time action)
           (let ((q (make-queue)))
             (queue-insert! q action)
             (make-time-segment time q))))
    (labels ((add-to-time-segments! (time-segments)
               (let ((current-time-segment (car time-segments)))
                 (if (= time (time-segment-time current-time-segment))
                     (queue-insert! (time-segment-queue current-time-segment)
                                    action)
                     (let ((rest-time-segments (cdr time-segments)))
                       (if (belongs-before? rest-time-segments)
                           (rplacd time-segments
                                   (cons (make-new-time-segment time action)
                                         (cdr time-segments)))
                           (add-to-time-segments! rest-time-segments)))))))
      (let ((time-segments (time-segments this)))
        (if (belongs-before? time-segments)
            (set-time-segments! this (cons (make-new-time-segment time action)
                                           time-segments))
            (add-to-time-segments! time-segments))))))
(defun remove-first-agenda-item! (this)
  "REMOVE-FIRST-AGENDA-ITEM
  agenda -> agenda
  Remove the first item from the agenda deletes the item at the front of the
  queue in the first time segment. If this deletion makes the time segment
  empty, we remove it from the list of segments."
  (let ((q (time-segment-queue (first-time-segment this))))
    (queue-delete! q)
    (if (queue-empty? q)
        (progn (set-time-segments! this (rest-time-segments this)) this)
        this)))
(defun frist-agenda-item (this)
  "FIRST-AGENDA-ITEM
  agenda -> procedure"
  (if (agenda-empty? this)
      (error (format '() "Agenda is empty -- FIRST-AGENDA-ITEM"))
      (let ((first-segment (first-time-segment this)))
        (set-current-time! this (time-segment-time first-segment))
        (queue-front (time-segment-queue first-segment)))))
