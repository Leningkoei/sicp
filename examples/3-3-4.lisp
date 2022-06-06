;;;; 3-3-4
;;;; 2022/06/01

(defun make-wire () 'ok)

(defparameter a (make-wire))
(defparameter b (make-wire))
(defparameter c (make-wire))
(defparameter d (make-wire))
(defparameter e (make-wire))
(defparameter s (make-wire))

(defun or-gate (wire-1 wire-2 wire-3) (declare (ignore wire-1 wire-2 wire-3))
  'ok)
(defun and-gate (wire-1 wire-2 wire-3) (declare (ignore wire-1 wire-2 wire-3))
  'ok)
(defun inverter (wire-1 wire-2) (declare (ignore wire-1 wire-2))
  'ok)

(defun half-adder (a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    ( or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a c1 sum c2)
    (and-gate c1 c2 c-out)
    'ok))

;;; Primitive function boxes

;; (get-signal <wire>)
;; >> returns the current value of the signal on the wire.

;; (set-signal! <wire> <new-value>)
;; >> changes the value of the signal on the wire to the new value.

;; (add-action! <wire> <procedure of no arguments>)
;; >> asserts that the designated procedure should be run when ever the signal
;;    on the wire changes value. Such procedures are the vehicles by which
;;    changes in the signal value on the wire are communicated to other wires.

(in-package :common-lisp-user)
(defpackage implement
  (:use :common-lisp)
  (:export inverter))

(defun get-signal (wire) (declare (ignore wire)) 'ok)
(defun set-signal! (wire new-value) (declare (ignore wire new-value)) 'ok)
(defun add-action! (wire p) (declare (ignore wire p)) 'ok)

(defun after-delay (delay procedure) (declare (ignore delay procedure))
  'ok)
(defparameter inverter-delay 0)
(defparameter and-gate-delay 0)
(defparameter  or-gate-delay 0)

(defun logical-not (s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (t (error (format nil "Invalid signal -- logical-not ~A" s)))))
(defun logical-and (s1 s2)
  (cond ((= s1 0) 0)
        ((= s2 0) 0)
        ((not (= s1 1))
         (error (format nil "Invalid signal -- logical-not ~A" s1)))
        ((not (= s2 1))
         (error (format nil "Invalid signal -- logical-not ~A" s2)))
        (t 1)))
(defun logical-or (s1 s2)
  (cond ((= s1 1) 1)
        ((= s2 1) 1)
        ((not (= s1 0))
         (error (format nil "Invalid signal -- logical-not ~A" s1)))
        ((not (= s2 0))
         (error (format nil "Invalid signal -- logical-not ~A" s2)))
        (t 0)))

(defun inverter (input output)
  (flet ((invert-input ()
           (let ((new-value (logical-not (get-signal input))))
             (after-delay inverter-delay
                          (lambda () (set-signal! output new-value))))))
    (add-action! input (lambda () (invert-input)))
    'ok))
(defun and-gate (a1 a2 output)
  (flet ((and-action-procedure ()
           (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
             (after-delay and-gate-delay
                          (lambda () (set-signal! output new-value))))))
    (add-action! a1 (lambda () (and-action-procedure)))
    (add-action! a2 (lambda () (and-action-procedure)))
    'ok))
(defun or-gate (a1 a2 output)
  (flet ((or-action-procedure ()
           (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
             (after-delay or-gate-delay
                          (lambda () (set-signal! output new-value))))))
    (add-action! a1 (lambda () (or-action-procedure)))
    (add-action! a2 (lambda () (or-action-procedure)))
    'ok))

;;; Representing wires

(in-package :common-lisp-user)
(defpackage represent
  (:use :common-lisp)
  (:export :todo))
(in-package :represent)

(defun todo () "Do something, plz")

(defun call-each (procedures)
  "CALL-EACH
  Calls each of the items in a list of no-argument procedure."
  (if procedures
      (progn (funcall (car procedures))
             (call-each (cdr procedures)))
      'done))
(defun make-wire ()
  (let ((signal-value 0)
        (action-procedures nil))
    (flet ((set-my-signal! (new-value)
             "SET-MY-SIGNAL!
  Tests whether the new signal value changes the signal on the wire. If so, it
  runs each of the action procedures, using the procedure `call-each`, which
  calls each of the items in a list of no-argument procedure."
             (if (not (= signal-value new-value))
                 (progn (setf signal-value new-value)
                        (call-each action-procedures))
                 'done))
           (accept-action-procedure! (procedure)
             "ACCEPT-ACTION-PROCEDIRE
  Adds the given procedure to list of procedures to be run, and then runs the
  new procedure once. (See exercise 3.31.)"
             (setf action-procedures (cons procedure action-procedures))
             (funcall procedure)))
      (let ((dispatch (lambda (operation)
                        (cond ((equal operation 'get-signal) signal-value)
                              ((equal operation 'set-signal!)
                               (lambda (new-value) (set-my-signal! new-value)))
                              ((equal operation 'add-actions!)
                               (lambda (procedure)
                                 (accept-action-procedure! procedure)))
                              ('t (error (format nil
                                                 "Unknown operation -- WIRE ~A"
                                                 operation)))))))
        dispatch))))

(defun get-signal (wire)
  (funcall wire 'get-signal))
(defun set-signal! (wire new-value)
  (funcall (funcall wire 'set-signal!) new-value))
(defun add-action! (wire action-procedure)
  (funcall (funcall wire 'add-action!) action-procedure))

(in-package common-lisp-user)
(defpackage agenda
  (:use :common-lisp)
  (:export todo))

(defun todo () "Do something, plz")

(defun make-agenda ()
  "MAKE-AGENDA
  nil -> agenda
  Returns a new empty agenda."
  'todo)
(defun empty-agenda? (agenda)
  "EMPTY-AGENDA?
  agenda -> boolean
  Is true if the specified agenda is empty."
  'todo)
(defun first-agenda-item (agenda)
  "FIRST-AGENDA-ITEM
  agenda -> todo
  Returns the first item on the agenda."
  'todo)
(defun remove-first-agenda-item! (agenda)
  "REMOVE-FIRST-AGENDA-ITEM
  agenda -> todo
  Modifies the agenda by removing the first item."
  'todo)
(defun add-to-agenda! (agenda time action)
  "ADD-TO-AGENDA!
  agenda -> todo -> todo
  Modifies the agenda by adding the given action procedure to be run at the
  specified time."
  'todo)
(defun current-time (agenda)
  "CURRENT-TIME
  agenda -> todo
  Returns the current simulation time."
  'todo)

(defun after-delay (delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action the-agenda))
(defun propagate ()
  "PROPAGATE
  nil -> 'done
  Operates on `the-agenda`, executing each procedure on the agenda in
  sequence. In general, as the simulation runs, new items will be added to the
  agenda, and `propagate` will continue the simulation as long as there are
  items on the agenda."
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;;; A sample simulation

(defun probe (name wire)
  (add-action! wire
               (lambda ()
                 (format t "~A ~A New-value = ~A"
                         name (current-time the-agenda) (get-signal wire))
                 (fresh-line))))

(defparameter the-agenda (make-agenda))
(defparameter inverter-delay 2)
(defparameter and-gate-delay 3)
(defparameter or-gate-delay 5)

;;; Continue in 3-3-4-2.lisp
