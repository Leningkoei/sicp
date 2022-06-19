;;;; 3-33
;;;; 3-3-4
;;;; 2022/06/19

;;; Using primitive multiplier, adder, and constant constraints, define a
;;; procedure `averager` that takes three connectors `a`, `b`, and `c` as inputs
;;; and establishes the constraint that the value of `c` is the average of `a`
;;; and `b`.

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
  (funcall (funcall connector connector) new-constraint))

(defun for-each-except (exception procedure list)
  (labels ((iterator (items)
             (cond ((null items) 'done)
                   ((eq (car items) exception) (iterator (cdr items)))
                   ('t (funcall procedure (car items))
                       (iterator (cdr items))))))
    (iterator list)))

;;;

(defun make-connector ()
  (let ((value '())
        (informant '())
        (constraints '())
        (inform-about-value 'todo)
        ;; `inform-about-value` ??
        (inform-about-no-value 'todo)
        ;; `inform-about-no-value` ??
        )
    (labels ((set-my-value (me new-value setter)
               (cond ((not (has-value? me))
                      (setf value new-value)
                      (setf informant setter)
                      (for-each-except setter inform-about-value constraints))
                     ((not (= value new-value))
                      (error (format '() "Contradiction ~A"
                                     (list value new-value))))
                     ('t 'ignore)))
             (forget-my-value (retractor)
               (if (equal retractor informant)
                   (progn (setf informant '())
                          (for-each-except retractor inform-about-no-value
                                           constraints))
                   'ignore))
             (connect (me new-constraint)
               (if (not (member new-constraint constraints))
                   (setf constraints (cons new-constraint constraints)))
               (if (has-value? me)
                   (funcall inform-about-value new-constraint))
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
