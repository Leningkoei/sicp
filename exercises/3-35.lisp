;;;; 3-35
;;;; 3-3-3
;;;; 2022/06/24

;;; Ben Bitdiddle tells Louis that one way to avoid the trouble in exercise 3.34
;;; is to define a squarer as a new primitive constraint. Fill the missing
;;; portions in Ben's outline for a procedure to implement such a constraint:

;; (defun square (a b)
;;   (labels ((process-new-value ()
;;              (if (has-value? b)
;;                  (if (< (get-value b) 0)
;;                      (error (format '() "square less than 0 -- SQUARE ~A"
;;                                     (get-value b)))
;;                      <alternative1>)
;;                  <alternative2>
;;                  ))
;;            (process-forget-value ()
;;              <body1>)
;;            (me (request)
;;              <body2>))
;;     (lambda (me request) (declare (ignore me)) (me me request))))

;;; syntax interface

(defun has-value? (connector)
  (funcall connector connector 'has-value?))
(defun get-value (connector)
  (funcall connector connector 'value))
(defun set-value! (connector new-value informant)
  (funcall (funcall connector connector 'set-value!) new-value informant))
(defun forget-value! (connector retractor)
  (funcall (funcall connector connector 'forget) retractor))
(defun connect (connector new-constraint)
  (funcall (funcall connector connector 'connect) new-constraint))

(defun inform-about-value (constraint)
  "Tells the given constraint that the connector has a value."
  (funcall constraint constraint 'I-have-a-value))
(defun inform-about-no-value (constraint)
  "Tells the given constraint that he connector has lost its value."
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
                      ;; interface: has-value?
                      (if informant 't '()))
                     ((equal request 'value)
                      ;; interface: get-value
                      value)
                     ((equal request 'set-value!)
                      ;; interface: set-value!
                      (lambda (new-value setter)
                        (set-my-value me new-value setter)))
                     ((equal request 'forget)
                      ;; interface: forget-value!
                      (lambda (retractor)
                        (forget-my-value retractor)))
                     ((equal request 'connect)
                      ;; interface: connect
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

(defun squarer (a b)
  (labels ((process-new-value (me)
             (if (has-value? b)
                 (if (< (get-value b) 0)
                     (error (format '() "Square less than 0 -- SQUARER ~A"
                                    (get-value b)))
                     (set-value! a (sqrt (get-value b)) me))
                 (if (has-value? a)
                     (set-value! b (expt (get-value a) 2) me))))
           (process-forget-value (me)
             (forget-value! a me)
             (forget-value! b me)
             (process-new-value me))
           (me (me request)
             (cond ((equal request 'I-have-a-value)
                    (process-new-value me))
                   ((equal request 'I-lost-my-value)
                    (process-forget-value me))
                   ('t (error (format '() "Unknown request -- SQUARER ~A"
                                      request))))))
    (let ((me (lambda (me request) (me me request))))
      (connect a me)
      (connect b me)
      me)))

;;; 

;; probe
(defun probe (name connector)
  (labels ((print-probe (value)
             (format 't "Probe: ~A = ~A~%" name value))
           (process-new-value ()
             (print-probe (get-value connector)))
           (process-forget-value ()
             (print-probe "?"))
           (me (request)
             (cond ((equal request 'I-have-a-value)
                    (process-new-value))
                   ((equal request 'I-lost-my-value)
                    (process-forget-value))
                   ('t (error (format '() "Unknown request -- PROBE ~A"
                                      request))))))
    (let ((me (lambda (me request) (declare (ignore me)) (me request))))
      (connect connector me)
      me)))
