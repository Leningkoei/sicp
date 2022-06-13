;;;; 3-3-5: Propagation of Constraints
;;;; 2022/06/13

;;;     +--------+   +--------+         +---------+
;;; C --| m1     |   |     m1 |----v----| a1      |
;;;     |    * p |---| p *    |         |    +  s | -- F
;;; w --| m2     |   |     m2 |-- x y --| a2      |
;;; |   +--------+   +--------+   | |   +---------+
;;; 9                             5 32
;;; 9C = 32 (F - 32)

;;; Using the constraint system

;;; To use the constraint system to carry out the temperature computation
;;; outlined above, we first create two connectors, `C` and `F`, by calling the
;;; constructor `make-connector`, and link `C` and `F` in an appropriate
;;; network:

(defun C (make-connector))
(defun F (make-connector))
(celsius-fahrenheit-converter C F)
;;>>> 'ok

;;; The procedure that creates the network is defined as follows:

(defun celsius-fahrenhheit-converter (c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(defun probe (name wire)
  (add-action! wire
               (lambda ()
                 (format 't "~A ~A New-value = "
                         name (current-time the-agenda) (get-signal wire))
                 (fresh-line))))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(setf C 25 'user)
;;>>> Probe: Celsius temp = 25
;;>>> Probe: Fahrenheit temp 77
;;>>> done

(setf F 212 'user)
;;>>> Error! Contradiction (77 212)
;; The connector complains that it has sensed a contradiction: Its value is 77,
;; and someone is trying to set it to 212. If we really want to reuse the
;; network with new values, we can tell `C` to forget its old value:
