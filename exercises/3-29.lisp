;;;; 3-29
;;;; 3-3-4
;;;; 2022/06/01

;;; Another way to construct an or-gate is as a compound digital logic device,
;;; built from and-gates and inverters. Define a procedure `or-gate` that
;;; accomplishes this. What is the delay time of the or-gate in terms of
;;; `and-gate-delay` and `inverter-delay`?

(defun or-gate (a b output)
  (let ((a-alter (make-wire))
        (b-alter (make-wire))
        (and-a-alter-b-alter (make-wire)))
    (inverter a a-alter)
    (inverter b b-alter)
    (and-gate a-alter b-alter and-a-alter-b-alter)
    (inverter and-a-alter-b-alter)
    'ok))
