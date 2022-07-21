;;;; 2-84
;;;; 2-5-2
;;;; 2022/07/21

;;; Using the `raise` operation of exercise 2.83, modify the `apply-generic`
;;; procedure so that it coerces its arguments to have the same type by the
;;; method of successive raising, as discussed in this section. You will need to
;;; devise a way to test which of two types is higher in the tower. Do this in a
;;; manner that is "compatible" with the rest of the system and will not lead to
;;; problems in adding new levels to the tower.

(defun raise-to (number target-type)
  (cond ((equal (type-tag number) target-type))
        ((get 'raise (type-tag number))
         (apply (get 'raise (type-tag number)) number))
        ('t '())))

(defun level<= (lower? higher?)
  (raise-to lower? higher?))

(defun get-highest-level (type-tags)
  (labels ((iterator (highest-level rest-type-tags)
             (if rest-type-tags
                 (iterator (if (level<= (car rest-type-tags) highest-level)
                               highest-level
                               (car rest-type-tags))
                           (cdr rest-type-tags))
                 highest-level)))
    (iterator (car type-tags) (cdr type-tags))))

(defun apply-generic (op &rest args)
  (let ((type-tags (map 'list #'type-tag args)))
    (let ((target-type (get-highest-level type-tags)))
      (let ((fixed-args (map 'list
                             (lambda (current-arg)
                               (raise-to current-arg target-type))
                             args))
            (fixed-type-tags (map 'list
                                  (lambda (current-type-tag)
                                    (declare (ignore current-type-tag))
                                    target-type)
                                  type-tags)))
        (let ((proc (get op fixed-type-tags)))
          (if proc
              (apply proc (map 'list #'content fixed-args))
              (error (format '() "No method for these types ~A"
                             `(,op ,fixed-type-tags)))))))))
