;;;; 3-50
;;;; 3-5-1
;;;; 2022/08/28

;;; Complete the following definition, which generalizes `stream-map` to allow
;;; procedures that take multiple arguments, analogous to `map` in section
;;; 2.2.3, footnote 12.

;; (map 'list #'+ (list 1 2 3) (list 40 50 60) (list 700 800 900))
;; (741 852 963)

(defmacro cons-stream (car cdr)
  `(cons ,car (lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defparameter the-empty-stream nil)
(defun stream-null? (stream)
  (null stream))

(defun stream-enumerate-interval (begin end)
  (if (= begin end)
      the-empty-stream
      (cons-stream begin (stream-enumerate-interval (+ 1 begin) end))))

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

(defun stream-map (f &rest argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply f (map 'list #'stream-car argstreams))
       (apply #'stream-map
              (cons f (map 'list #'stream-cdr argstreams))))))

(defun test ()
  (display-stream
   (stream-map
    #'+
    (stream-enumerate-interval 1 4)
    (stream-enumerate-interval 4 8)
    (stream-enumerate-interval 8 12))))
