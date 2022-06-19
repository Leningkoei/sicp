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
;;>>> Probe: Fahrenheit temp = 77
;;>>> done

(setf F 212 'user)
;;>>> Error! Contradiction (77 212)

;; The connector complains that it has sensed a contradiction: Its value is 77,
;; and someone is trying to set it to 212. If we really want to reuse the
;; network with new values, we can tell `C` to forget its old value:

(forget-value! C 'user)
;;>>> Probe: Celsius temp = ?
;;>>> Probe: Fahrenheit temp = ?
;;>>> done

(set-value! F 212 'user)
;;>>> Probe: Fahrenheit temp = 212
;;>>> Probe: Celsius temp = 100
;;>>> done

;;; Implementing the constraint system

(defun has-value? (connector)
  "tells whether the connector has a value."
  'todo)
(defun get-value (connector)
  "returns the connector's current value."
  'todo)
(defun set-value! (connector new-value)
  "indicates that the informant is requesting the connector to set its value to
  the new value."
  'todo)
(defun forget-value! (connector retractor)
  "tells the connector that the retractor is requesting it to forget its value."
  'todo)
(defun connect (connector new-constraint)
  "tells the connector to participate in the new constraint."
  'todo)

(defun adder (a1 a2 sum)
  "connects the new adder to the designated connectors and returns it as its
  value. The procedure `me`, which represents the adder, acts as a dispatch to
  the local procedures."
  (let ((process-new-value
          (lambda (me)
            (cond ((and (has-value? a1) (has-value? a2))
                   (set-value! sum (+ (get-value a1) (get-value a2)) me))
                  ((and (has-value? a1) (has-value? sum))
                   (set-value! a2 (- (get-value sum) (get-value a1)) me))
                  ((and (has-value? a2) (has-value? sum))
                   (set-value! a1 (- (get-value sum) (get-value a2)) me))))))
    (let ((process-forget-value
            (lambda (me)
              (forget-value sum me)
              (forget-value a1 me)
              (forget-value a2 me)
              (funcall process-new-value me))))
      (let ((me
              (lambda (request me)
                (cond ((equal request 'I-have-a-value)
                       (funcall process-new-value me))
                      ((equal request 'I-lose-my-value)
                       (funcall process-forget-value me))
                      ('t (error (format nil "Unknown request -- ADDER ~A"
                                         request)))))))
        (connect a1 me)
        (connect a2 me)
        (connect sum me)
        me))))

(defun multiplier (m1 m2 product)
  (let ((process-new-value
          (lambda (me)
            (cond ((or (and (has-value? m1) (= (get-value m1) 0))
                       (and (has-value? m2) (= (get-value m2) 0)))
                   (set-value! product 0 me))
                  ((and (has-value? m1) (has-value? m2))
                   (set-value! product (* (get-value m1) (get-value m2)) me))
                  ((and (has-value? m1) (has-value? product))
                   (set-value! m2 (/ (get-value product) (get-value m1)) me))
                  ((and (has-value? m2) (has-value? product))
                   (set-value! m1 (/ (get-value product) (get-value m2)) me))
                  ))))
    (let ((process-forget-value
            (lambda (me)
              (forget-value! product me)
              (forget-value! m1 me)
              (forget-value! m2 me)
              (funcall process-new-value me))))
      (let ((me (lambda (request me)
                  (cond ((equal request 'I-have-a-value)
                         (funcall process-new-value me))
                        ((equal request 'I-lost-my-value)
                         (funcall process-forget-value me))
                        ('t (error (format nil "Unknown mark -- MULTIPLIER ~A"
                                           request)))))))
        (connect m1 me)
        (connect m2 me)
        (connect product me)
        me))))

;;; syntax interfaces
(defun inform-about-value (constraint)
  (funcall constraint 'I-have-a-value constraint))
(defun inform-about-no-value (constraint)
  (funcall constraint 'I-lost-my-value constraint))

(defun constant (value connector)
  "Simply sets the value of the designated connector. Any `I-have-a-value` or
  `I-lost-my-value` message sent to the constant box will produce an error."
  (let ((me (lambda (request)
              (error (format nil "Unknown request -- CONSTANT ~A" request)))))
    (connect connector me)
    (set-value! connector value me)
    me))

(defun probe (name connector)
  (labels ((print-probe (value)
             (format t "Probe: ~A = ~A" name value))
           (process-new-value ()
             (print-probe (get-value connector)))
           (proess-forget-value ()
             (print-probe "?"))
           (me (request)
             (cond ((equal request 'I-have-a-value) (process-new-value))
                   ((equal request 'I-lost-my-value) (process-forget-value))
                   ('t (error (format '() "Unknown request -- PROBE ~A"
                                      request))))))
    (let ((me (lambda (request) (me request))))
      (connect connector me)
      me)))

;;; Representing connectors

(defun make-connect ()
  (let ((value       '())
        ;; the current value of the connector
        (informant   '())
        ;; the object that set the connector's value
        (constraints '())
        ;; a list of the constraints in which the connector participates
        )
    (labels ((set-my-value (me new-value setter)
               (cond ((not (has-value? me))
                      (setf value new-value)
                      (setf informant setter)
                      (for-each-except setter inform-about-value constraints)
                      ;; ???
                      )
                     ((not (= value new-value))
                      (error (format '() "Contradiction ~A"
                                     (list value new-value))))
                     ('t 'ignored)))
             (forget-my-value (retractor)
               (if (equal retractor informant)
                   (progn (setf informant '())
                          (for-each-except retractor inform-about-no-value
                                           constraints))
                   'ignored))
             (connect (me new-constraint)
               (if (not (member new-constraint constraints))
                   (setf constraints (cons new-constraint constraints)))
               (if (has-value? me)
                   (inform-about-value new-constraint))
               'done)
             (me (me request)
               (cond ((equal request 'has-value?) (if informant 't '()))
                     ((equal request 'value) value)
                     ((equal request 'set-value!)
                      (lambda (new-value setter)
                        (set-my-value me new-value setter)))
                     ((equal request 'forget)
                      (lambda (retractor)
                        (forget-my-value retractor)))
                     ((equal request 'connect)
                      (lambda (new-constraint)
                        (connect me new-constraint)))
                     ('t (error (format '() "Unknown operation -- CONNECTOR ~A"
                                        request))))))
      (lambda (me request) (me me request)))))

(defun for-each-except (exception procedure list)
  (labels ((iterator (items)
             (cond ((null items) 'done)
                   ((eq (car items) exception) (iterator (cdr items)))
                   ('t (procedure (car items)) (iterator (cdr items))))))
    (iterator list)))

;;; syntax interface
(defun has-value? (connector)
  (funcall connector connector 'has-value))
(defun get-value? (connector)
  (funcall connector connector value))
(defun set-value! (connector new-value informant)
  (funcall (funcall connector connector 'set-value!) new-value informant))
(defun forget (connector retractor)
  (funcall (funcall connector connector 'forget) retractor))
(defun connect (connector new-constraint)
  (funcall (funcall connector connector 'connect) new-constraint))
