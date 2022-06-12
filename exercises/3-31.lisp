;;;; 3-31
;;;; 3-3-4
;;;; 2022/06/12

;;; The internal procedure `accept-action-procedure!` defined in `make-wire`
;;; specifies that when a new action procedure is added to a wire, the procedure
;;; is immediately run. Explain why this initialization is necessary. In
;;; particular, trace through the half-adder example in the paragraphs above and
;;; say how the system's would differ if we had defined
;;; `accept-action-procedure!` as
;;; (define (accept-action-procedure! proc)
;;;   (set! action-procedures (cons proc action-procedures)))
;;; 
;;; (define (accept-action-procedure! proc)
;;;   (set! action-procedures (cons proc action-procedures))
;;;   (proc))

(defun half-adder (a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate  a b c)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun or-gate (a1 a2 output)
  (flet ((or-action-procedure ()
           (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
             (after-delay or-gate-delay
                          (lambda () (set-signal! output new-value))))))
    (add-action! a1 (lambda () (or-action-procedure)))
    (add-action! a2 (lambda () (or-action-procedure)))))

;;; If you don't run the initialization immediately, you would not get the first
;;; state of the output.
;;; Maybe.
