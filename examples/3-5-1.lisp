;;;; 3-5-1: Streams Are Delayed Lists
;;;; 2022/08/22

(defun stream-ref (stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))
(defun stream-map (f stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream
       (apply f (list (stream-car stream)))
       (stream-map f (stream-cdr stream)))))
(defun stream-for-each (f stream)
  (if (stream-null? stream)
      'done
      (progn
        (apply f (list (stream-car stream)))
        (stream-for-each f (stream-cdr stream)))))
(defun print-line (line)
  (format 't "~A~%" line))
(defun display-stream (stream)
  (stream-for-each
   #'(lambda (current) (print-line current))
   stream))

;;; evaluating `(delay <exp>)` does not evaluate the expression `<exp>`, but
;;; rather returns a so-called `delayed object`, which we can think of as a
;;; "promise" to evaluate `<exp>` at some future time. As a companion to
;;; `delay`, there is a procedure called `force` that takes a delayed object as
;;; argument and performs the evaluation -- in effect, forcing the `delay` to
;;; fulfill its promise.

;;; use macro to avoid evaluate cdr
(defmacro cons-stream (car cdr)
  `(cons ,car (delay ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (force (cdr stream)))
(defparameter the-empty-stream nil)
(defun stream-null? (stream)
  (null stream))

;;; the stream implementation in action

(defun stream-enumerate-interval (begin end)
  (if (= begin end)
      the-empty-stream
      (cons-stream begin (stream-enumerate-interval (+ 1 begin) end))))
(defun stream-filter (test stream)
  (if (stream-null? stream)
      the-empty-stream
      (if (apply test (list (stream-car stream)))
          (cons-stream
           (stream-car stream)
           (stream-filter test (stream-cdr stream)))
          (stream-filter test (stream-cdr stream)))))

(defparameter result
  (stream-car
   (stream-cdr
    (stream-filter #'evenp (stream-enumerate-interval 1 100000000000000)))))

;;; Implementing `delay` and `force`

(defmacro delay (procedure)
  `(lambda () ,procedure))
(defun force (delayed-procedure)
  (apply delayed-procedure (list)))
