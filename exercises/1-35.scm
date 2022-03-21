;;;; 1-35
;;;; 1-3-3
;;;; 2022/03/21

;;; Show that the golden ratio Phi (section 1.2.2) is a fixed point of the
;;; transformation x -> 1 + 1 / x, and use this fact to compute Phi by means of
;;; the `fixed-point` procedure.

(define tolerance 0.00001)
(define fixed-point (lambda (f first-guess)
  (define close-enough? (lambda (v1 v2)
    (< (abs (- v1 v2)) tolerance)))
  (define try (lambda (guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next)))))
  (* (try first-guess) 1.0)))
  ; (let ((close-enough? (lambda (v1 v2)
  ;         (< (abs (v1 - v2)) tolerance)))
  ;       (try (lambda (guess)
  ;         (let ((next (f guess)))
  ;           (if (close-enough? guess next)
  ;             next
  ;             (try next)))))
  ;   (try first-guess)))))

(define get-Phi (lambda ()
  (fixed-point (lambda (x) (1+ (/ 1 x)))
               1)))

