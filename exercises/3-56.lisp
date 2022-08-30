;;;; 3-56
;;;; 3-5-2
;;;; 2022/08/30

;;; A famous problem, first raised by R. Hamming, is to enumerate, in ascending
;;; order with no repetitions, all positive integers with no prime factors other
;;; than 2, 3, or 5. One obvious way to do this is to simply test each integer
;;; in turn to see whether it has any factors other than 2, 3, and 5. But this
;;; is very inefficient, since, as the integers get larger, fewer and fewer of
;;; them fit the requirement. As an alternative, let us call the required stream
;;; of numbers `s` and notice the following facts about it.

;;; * `S` begins with 1.

;;; * The elements of `(scale-stream S 2)` are also elements of `S'

;;; * The same is true for `(scale-stream S 3)` and `(scale-stream S 5)`

;;; These are all the elements of `S`.

;;; Now all we have to do is combine elements from these sources. For this we
;;; define a procedure `merge` that combines two ordered streams into one
;;; ordered result stream, eliminating repetitions:

(shadow 'merge)
(defun merge (s1 s2)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    ('t
     (let ((s1car (stream-car s1))
           (s2car (stream-car s2)))
       (cond
         ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
         ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
         ('t (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

;;; Then the required stream may be constructed with `merge`, as follows:

;; (defparameter S (cons-stream 1 (merge <??> <??>)))

;;; Fill in the missing expressions in the places marked <??> above.

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
(defun scale-stream (stream factor)
  (stream-map #'(lambda (current) (* factor current)) stream))

;;; answer:

(defparameter S
  (cons-stream
   1
   (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
