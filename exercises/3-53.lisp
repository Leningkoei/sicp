;;;; 3-53
;;;; 3-5-2
;;;; 2022/08/30

;;; Without running the program, describe the elements of the stream defined by

;;; answer: 1 2 4 8 ...

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

(shadow 'stream)
(defparameter stream
  (cons-stream 1 (add-streams stream stream)))

;;; answer is right.
