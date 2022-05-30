;;;; 3-3-3: Representing Tables
;;;; 2022/05/29

(defpackage table
  (:use :common-lisp)
  (:export make-table
           look-up
           insert))
(in-package :table)

(defun assoc (key records)
  (cond ((null records) nil)
        ((equal key (carr records)) (car records))
        (t (assoc key (cdr records)))))
(defun lookup (key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        nil)))
(defun insert! (key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (rplacd record value)
        (rplacd table (cons (cons key value) (cdr table)))))
  'ok)
(defun make-table ()
  (list '*table*))

;;; Two-dimensional tables

(defpackage square-table
  (:use :common-lisp)
  (:export make-table
           look-up))
(in-package :square-table)

(defun assoc (key records)
  (cond ((null records) nil)
        ((equal key (carr records) (car records)))
        (t (assoc key (cdr records)))))
(defun lookup (keyp-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              nil))
        nil)))
(defun insert! (key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (rplacd record value)
              (rplacd subtable (cons (cons key-2 value)
                                     (cdr subtable)))))
        (rplacd table (cons (list key-1 (cons key-2 value))
                            (cdr table)))))
  'ok)

;;; Creating local tables

(defpackage local-square-table
  (:use :common-lisp)
  (:export make-table))

(defun assoc (key records)
  (cond ((null records) nil)
        ((equal (caar records) key) (car records))
        (t (assoc key (cdr records)))))
(defun make-table ()
  (let ((local-table (list *table*)))
    (labels ((lookup (key-1 key-2)
               (let ((subtable (assoc key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (assoc key-2 (cdr subtable))))
                       (if record
                           (cdr record)
                           nil))
                     nil)))
             (insert! (key-1 key-2 value)
               (let ((subtable (assoc key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (assoc key-2 (cdr subtable))))
                       (if record
                           (rplacd record value)
                           (rplacd subtable (cons (cons key-2 value)
                                                  (cdr subtable)))))
                     (rplacd local-table (cons (list key-1 (cons key-2 value))
                                               (cdr local-table)))))
               'ok))
      (let ((dispatch
              (lambda (m)
                (cond ((equal m 'lookup)
                       (lambda (key-1 key-2) (lookup key-1 key-2)))
                      ((equal m 'insert!)
                       (lambda (key-1 key-2 value) (insert! key-1 key-2 value)))
                      (t (error (format nil "Unknown operation -- TABLE ~A"
                                        m)))))))
        dispatch))))
