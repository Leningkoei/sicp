;;;; 3-25
;;;; 3-3-3
;;;; 2022/05/30

;;; Generalizing one- and two-dimensional tables, show how to implement a table
;;; in which values are stored under an arbitrary number of keys and different
;;; values may be stored under different numbers of keys. The `lookup` and
;;; `insert!` procedures should takes as input a list of keys used to access the
;;; table.

(defun make-table (test)
  (let ((table (list '*table*)))
    (labels
      ((*assoc (key records)
         (cond ((null records) nil)
               ((and (listp (car records)) (funcall test key (caar records)))
                (car records))
               (t (*assoc key (cdr records)))))
       (lookup (keys table)
         (if keys
             (let ((subtable (*assoc (car keys) (cdr table))))
               (if subtable
                   (lookup (cdr keys) subtable)
                   nil))
             table))
       ;; (lookup (&rest blabla) (declare (ignore blabla))
       ;;   table)
       (make-the-list (value keys)
         (if keys
             (list (car keys) (make-the-list value (cdr keys)))
             value))
       (insert! (value keys table)
         (if keys
             (let ((subtable (*assoc (car keys) (cdr table))))
               (if subtable
                   (insert! value (cdr keys) subtable)
                   (progn (rplacd table (cons (make-the-list value keys)
                                              (cdr table)))
                          'ok)))
             (progn (rplacd table (list value)) 'ok))))
      (let ((dispatch
              (lambda (operation)
                (cond ((equal operation 'lookup)
                       (lambda (&rest keys) (lookup keys table)))
                      ((equal operation 'insert!)
                       (lambda (value &rest keys)
                         (insert! value keys table)))
                      (t (error (format nil "Unknown operation -- TABLE ~A"
                                        operation)))))))
        dispatch))))

(defun test ()
  (let ((table (make-table 'equal)))
    (funcall (funcall table 'insert!) 1 'a)
    (funcall (funcall table 'insert!) 2 'a 'alpha)
    (format t "~A" (funcall (funcall table 'lookup) 'a)) (fresh-line)
    (funcall (funcall table 'insert!) 3 'a 'alpha)
    (format t "~A" (funcall (funcall table 'lookup) 'a 'alpha)) (fresh-line)
    table))
