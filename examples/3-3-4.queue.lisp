(in-package :common-lisp-user)
(defpackage :queue
  (:use
   :common-lisp)
  (:export
   :make
   :empty?
   :body-current
   :body-rest
   :insert!
   :delete!))
(in-package :queue)

(defun make ()
  (cons nil nil))
(defun front-ptr (this)
  (car this))
(defun rear-ptr (this)
  (cdr this))
(defun set-front-ptr! (this item)
  (rplaca this item))
(defun set-rear-ptr! (this item)
  (rplacd this item))
(defun empty? (this)
  (null (front-ptr this)))
(defun front (this)
  (if (empty? this)
      (error (format nil "FRONT called with an empty queue ~A" this))
      (car (front-ptr this))))
(defun body-current (body)
  (car body))
(defun body-rest (body)
  (cdr body))
(defun insert! (this item)
  (let ((new-pair (cons item nil)))
    (if (empty? this)
        (progn (set-front-ptr! this new-pair)
               (set-rear-ptr! this new-pair)
               this)
        (progn (rplacd (rear-ptr this) new-pair)
               (set-rear-ptr! this new-pair)
               this))))
(defun delete! (this)
  (if (empty? this)
      (error (format nil "DELETE! called with an empty queue ~A" this))
      (progn (set-front-ptr! this (cdr (front-ptr this)))
             this)))
