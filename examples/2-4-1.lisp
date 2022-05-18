;;;; 2-4-1
;;;; 2022/05/17

(defun square (x)
  (* x x))

;;; implement 1

(defun real-part (z)
  (car z))
(defun imag-part (z)
  (cdr z))
(defun magnitude (z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(defun angle (z)
  (atan (imag-part z) (real-part z)))
(defun make-from-real-imag (real-part imag-part)
  (cons real-part imag-part))
(defun make-from-mag-ang (magnitude angle)
  (cons (* magnitude (cos angle)) (* magnitude (sin angle))))

;;; implement 2

;; (defun magnitude (z)
;;   (car z))
;; (defun angle (z)
;;   (cdr z))
;; (defun real-part (z)
;;   (* (magnitude z) (cos (angle z))))
;; (defun imag-part (z)
;;   (* (magnitude z) (sin (angle z))))
;; (defun make-from-real-imag (real-part imag-part)
;;   (cons (sqrt (+ (square real-part) (square imag-part)))
;;         (atan imag-part real-part)))
;; (defun make-from-mag-ang (magnitude angle)
;;   (cons magnitude angle))

(defun add-complex (z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(defun sub-complex (z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(defun mul-complex (z1 z2)
  (make-from-mag-ang  (* (magnitude z1) (magnitude z2))
                      (+ (angle z1) (angle z2))))
(defun div-complex (z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
