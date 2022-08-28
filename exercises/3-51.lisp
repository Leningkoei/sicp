;;;; 3-51
;;;; 3-5-1
;;;; 2022/08/28

;;; In order to take a closer look at delayed evaluation, we will use the
;;; following procedure, which simply returns its argument after printing it:

;; (define (show x)
;;   (display-line x)
;;   x)

;;; What does the interpreter print in response to evaluating each expression in
;;; the following sequence?

;; (define x (stream-map show (stream-enumerate-interval 0 10)))
;; (stream-ref x 5)
;; (stream-ref x 7)

(defmacro cons-stream (car cdr)
  `(cons ,car (lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defparameter the-empty-stream nil)
(defun stream-null? (stream)
  (null stream))

(defun stream-map (f stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream
       (apply f (list (stream-car stream)))
       (stream-map f (stream-cdr stream)))))
(defun stream-enumerate-interval (begin end)
  (if (= begin end)
      the-empty-stream
      (cons-stream begin (stream-enumerate-interval (+ begin 1) end))))
(defun stream-ref (stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(defun show (x)
  (print x))

(defparameter x (stream-map #'show (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)

;;; if you have cache: 0 1 2 3 4 5 6 7
;;; else: 0 0 1 2 3 4 5 0 1 2 3 4 5 6 7
;;; I don't have cache
