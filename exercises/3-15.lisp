;;;; 3-15
;;;; 3-3-1
;;;; 2022/05/24

;;; Draw box-and-pointer diagrams to explain the effect of `set-to-wow!` on the
;;; structures `z1` and `z2` above.

(defun set-car! (list car)
  (setf (car list) car))
(defun set-to-wow (x)
  (set-car! (car x) 'wow)
  x)

(defparameter x (list 'a 'b))
(defparameter z1 (cons x x))
(defparameter z2 (cons (list 'a 'b) (list 'a 'b)))

;; (set-to-wow z1)
;; >> ((wow b) (wow b))
;; (set-to-wow z2)
;; >> ((wow b) (a b))
