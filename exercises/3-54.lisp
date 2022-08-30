;;;; 3-54
;;;; 3-5-2
;;;; 2022/08/30

;;; Define a procedure `mul-streams`, analogous to `add-streams`, that produces
;;; the elementwise  product of its two input streams. Use this together with
;;; the stream of `integers` to complete the following definition of the stream
;;; whose `n`th element (counting from 0) is `n` + 1 factorial:

;; (defparameter factorials (cons-stream 1 (mul-streams <??> <??>)))

(defmacro cons-stream (car cdr)
  `(cons ,car (lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defun stream-null? (stream)
  (null stream))
(defparameter the-empty-stream nil)

(defun stream-map (f &rest streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (cons-stream
       (apply f (map 'list #'stream-car streams))
       (apply #'stream-map (cons f (map 'list #'stream-cdr streams))))))
(defun add-streams (&rest streams)
  (apply #'stream-map (cons #'+ streams)))
(defun mul-streams (&rest streams)
  (apply #'stream-map (cons #'* streams)))
(defparameter ones (cons-stream 1 ones))
(defparameter integers
  (cons-stream 1 (add-streams integers ones)))

(defparameter factorials (cons-stream 1 (mul-streams factorials integers)))
