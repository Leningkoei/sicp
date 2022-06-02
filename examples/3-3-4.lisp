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
    (or-get a b d)
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
