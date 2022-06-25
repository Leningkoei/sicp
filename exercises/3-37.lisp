;;;; 3-37
;;;; 3-3-3
;;;; 2022/06/25

;;; The `celsius-fahrenheit-converter` procedure is cumbersome when compared
;;; with a more expression-oriented style of definition, such as

;; (defun celsius-fahrenheit-converter (x)
;;   (c+ (c* (c/ (cv 9) (cv 5)) x) (cv 32)))
;; (defparameter C (make-connector))
;; (defparameter F (make-connector))

;;; Here `c+`, `c*`, etc. are the "constraint" versions of the arithmetic
;;; operations. For example, `c+` takes two connectors as arguments and returns
;;; a connector that is related to these by an adder constraint:

;; (defun c+ (x y)
;;   (let ((z (make-connector)))
;;     (adder x y z)
;;     z))

;;; Define analogous procedures `c-`, `c*`, `c/`, and `cv` (constant value) that
;;; enable us to define compound constraints as in the converter example above.

;;; syntax interface

(defun has-value? (connector)
  (funcall connector connector 'has-value?))
(defun get-value (connector)
  (funcall connector connector 'value))
(defun set-value! (connector new-value informant)
  (funcall (funcall connector connector 'set-value!) new-value informant))
(defun forget-value! (connector retractor)
  (funcall (funcall connector connector 'forget!) retractor))
(defun connect (connector new-constraint)
  (funcall (funcall connector connector 'connect) new-constraint))

(defun inform-about-value (constraint)
  "Tells the given constraint that the connector has a value."
  (funcall constraint constraint 'I-have-a-value))
(defun inform-about-no-value (constraint)
  "Tells the given constraint that the connector has lost its value."
  (funcall constraint constraint 'I-lost-my-value))

;;;

(defun for-each-except (exception procedure list)
  (labels ((iterator (items)
             (cond ((null items) 'done)
                   ((eq (car items) exception) (iterator (cdr items)))
                   ('t (funcall procedure (car items))
                       (iterator (cdr items))))))
    (iterator list)))

(defun make-connector ()
  (let ((value '())
        (informant '())
        (constraints '()))
    (labels ((set-my-value (me new-value setter)
               (cond ((not (has-value? me))
                      (setf value new-value)
                      (setf informant setter)
                      (for-each-except setter 'inform-about-value constraints))
                     ((not (= value new-value))
                      (error (format '() "Contradiction ~A"
                                     (list value new-value))))
                     ('t 'ignore)))
             (forget-my-value (retractor)
               (if (equal retractor informant)
                   (progn (setf informant '())
                          (for-each-except retractor 'inform-about-no-value
                                           constraints))
                   'ignore))
             (connect (me new-constraint)
               (if (not (member new-constraint constraints))
                   (setf constraints (cons new-constraint constraints)))
               (if (has-value? me)
                   (funcall 'inform-about-value new-constraint))
               'done)
             (me (me request)
               (cond ((equal request 'has-value?)
                      (if informant 't '()))
                     ((equal request 'value)
                      value)
                     ((equal request 'set-value!)
                      (lambda (new-value setter)
                        (set-my-value me new-value setter)))
                     ((equal request 'forget!)
                      (lambda (retractor)
                        (forget-my-value retractor)))
                     ((equal request 'connect)
                      (lambda (new-constraint)
                        (connect me new-constraint)))
                     ('t (error (format '() "Unknown operation -- CONNECTOR ~A"
                                        request))))))
      (lambda (me request) (me me request)))))

;;; constraints

(defun constant (value connector)
  (let ((me (lambda (request)
              (error (format '() "Unknown request -- CONSTANT ~A" request)))))
    (connect connector me)
    (set-value! connector value me)
    me))

(defun adder (a1 a2 sum)
  (labels ((process-new-value (me)
             (cond ((and (has-value? a1) (has-value? a2))
                    (set-value! sum (+ (get-value a1) (get-value a2)) me))
                   ((and (has-value? a1) (has-value? sum))
                    (set-value! a2 (- (get-value sum) (get-value a1)) me))
                   ((and (has-value? a2) (has-value? sum))
                    (set-value! a1 (- (get-value sum) (get-value a2)) me))))
           (process-forget-value (me)
             (forget-value! sum me)
             (forget-value! a1 me)
             (forget-value! a2 me)
             (process-new-value me))
           (me (me request)
             (cond ((equal request 'I-have-a-value)
                    (process-new-value me))
                   ((equal request 'I-lost-my-value)
                    (process-forget-value me))
                   ('t (error (format '() "Unknown request -- ADDER ~A"
                                      request))))))
    (let ((me (lambda (me request) (me me request))))
      (connect a1 me)
      (connect a2 me)
      (connect sum me)
      me)))

(defun multiplier (m1 m2 product)
  (labels ((process-new-value (me)
             (cond ((or (and (has-value? m1) (= (get-value m1) 0))
                        (and (has-value? m2) (= (get-value m2) 0)))
                    (set-value! product 0 me))
                   ((and (has-value? m1) (has-value? m2))
                    (set-value! product (* (get-value m1) (get-value m2)) me))
                   ((and (has-value? m1) (has-value? product))
                    (set-value! m2 (/ (get-value product) (get-value m1)) me))
                   ((and (has-value? m2) (has-value? product))
                    (set-value! m1 (/ (get-value product) (get-value m2)) me))))
           (process-forget-value (me)
             (forget-value! product me)
             (forget-value! m1 me)
             (forget-value! m2 me)
             (process-new-value me))
           (me (me request)
             (cond ((equal request 'I-have-a-value)
                    (process-new-value me))
                   ((equal request 'I-lost-my-value)
                    (process-forget-value me))
                   ('t (error (format '() "Unknown request -- MULTIPLIER ~A"
                                      request))))))
    (let ((me (lambda (me request) (me me request))))
      (connect m1 me)
      (connect m2 me)
      (connect product me)
      me)))

;;; analogous procedures

(defun cv (value)
  (let ((x (make-connector)))
    (constant value x)
    x))

(defun c+ (x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(defun c- (x y)
  "Returns `x - y`."
  (let ((z (make-connector)))
    (adder z y x)
    z))

(defun c* (x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(defun c/ (x y)
  "Returns ` x / y`."
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

;;;

(defun celsius-fahrenheit-converter (x)
  (c+ (c* (c/ (cv 9) (cv 5)) x) (cv 32)))

(defun test ()
  (let ((C (make-connector)))
    (let ((F (celsius-fahrenheit-converter C)))
      (cons C F))))
