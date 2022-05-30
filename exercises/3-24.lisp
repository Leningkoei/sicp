;;;; 3-24
;;;; 3-3-3
;;;; 2022/05/30

;;; IN the table implementations above, the keys are tested for equality using
;;; `equal?` (called by `assoc`). This is not always the appropriate test. For
;;; instance, we might have a table with numeric keys in which we might have a
;;; table with numeric keys in which we don't need an exact match to the number
;;; we're looking up, but only a number within some tolerance of it. Design a
;;; table constructor `make-table` that takes as an argument a `same-key?`
;;; procedure that will be used to test "equality" of keys. `make-table` should
;;; return a `dispatch` procedure that can be used to access appropriate
;;; `lookup` and `insert!` procedures for a local table.

(defpackage local-table
  (:use :common-lisp)
  (:export make-table
           test))
(in-package :local-table)

(defun make-table (same-key?)
  (let ((table (list '*table*)))
    (labels ((*assoc (key records)
               (cond ((null records) nil)
                     ((funcall same-key? (caar records) key) (car records))
                     (t (*assoc key (cdr records))))))
      (flet ((lookup (key-1 key-2)
               (let ((subtable (*assoc key-1 (cdr table))))
                 (if subtable
                     (let ((record (*assoc key-2 (cdr subtable))))
                       (if record
                           (cdr record)
                           nil))
                     nil)))
             (insert! (key-1 key-2 value)
               (let ((subtable (*assoc key-1 (cdr table))))
                 (if subtable
                     (let ((record (*assoc key-2 (cdr subtable))))
                       (if record
                           (rplacd (car record) value)
                           (rplacd subtable (cons (cons key-2 value)
                                                  (cdr subtable)))))
                     (rplacd table (cons (list key-1 (cons key-2 value))
                                         (cdr table)))))))
        (let ((dispatch
                (lambda (operation)
                  (cond ((equal operation 'lookup)
                         (lambda (key-1 key-2) (lookup key-1 key-2)))
                        ((equal operation 'insert!)
                         (lambda (key-1 key-2 value)
                           (insert! key-1 key-2 value)))
                        (t (error (format nil "Unknown operation -- TABLE ~A"
                                          operation)))))))
          dispatch)))))

(defun test ()
  (let ((table (make-table 'equal)))
    (funcall (funcall table 'insert!) 'alpha 'a 1)
    (funcall (funcall table 'insert!) 'alpha 'b 2)
    (funcall (funcall table 'insert!) 'alpha 'c 3)
    (funcall (funcall table 'insert!) 'alpha 'd 4)
    (funcall (funcall table 'insert!) 'beta  'a 1)
    (funcall (funcall table 'insert!) 'beta  'b 2)
    (funcall (funcall table 'insert!) 'beta  'c 3)
    (funcall (funcall table 'insert!) 'beta  'd 4)
    (format t "~A" (funcall (funcall table 'lookup) 'alpha 'd))
    (fresh-line)
    (format t "~A" (funcall (funcall table 'lookup) 'beta 'c))
    (fresh-line)))
