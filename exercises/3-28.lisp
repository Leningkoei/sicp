;;;; 3-28
;;;; 3-3-4
;;;; 2022/06/01

;;; Define an or-gate as a primitive function box. Your `or-gate` constructor
;;; should be similar to `and-gate`.

(defun or-gate (a1 a2 output)
  (flet ((or-action-procedure ()
           (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
             (after-delay or-get-delay
                          (lambda () (set-signal! output new-value))))))
    (or-action-procedure a1 (lambda () (or-action-procedure)))
    (or-action-procedure a2 (lambda () (or-action-procedure)))
    'ok))
