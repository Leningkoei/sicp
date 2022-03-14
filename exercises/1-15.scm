;;;; 1-15
;;;; 1-2-3
;;;; 2022/03/14

;;; The sine of an angle (specified in radians) can be computed by making use of
;;; the approximation `sin(x) == x` if `x` is sufficiently small, and the
;;; trigonometric identity
;;; sin(x) = 3 * sin(x / 3) - 4 * sin(x / 3) ** 3
;;; to reduce the size of the argument of `sin`. (For purposes of this exercise
;;; an angle is considered "sufficiently small" if its magnitude is not greater
;;; than 0.1 radians.) These ideas are incorporated in the following procedures::

(define p (lambda (x)          ; 5
  (- (* 3 x) (* 4 (cube x)))))
(define sine (lambda (angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0))))))

