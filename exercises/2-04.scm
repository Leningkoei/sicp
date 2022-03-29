;;;; 2-04
;;;; 2-1-3
;;;; 2022/03/27

;;; Here is an alternative procedural representation of pairs. For this
;;; representation, verify that `(car (cons x y))` yields `x` for any objects
;;; `x` and `y`.

(define my-cons (lambda (x y)
  (lambda (m) (m x y))))
(define my-car (lambda (z)
  (z (lambda (p q) p))))

;;; What is the corresponding definition of `cdr`? (Hint: To verify that this
;;; works, make use of the substitution model of section 1.1.5.)

; (define entry (cons "key" "value"))
; (define entry (lambda (m) (m "key" "value")))
; (car entry)
; ((lambda (z) (z (lambda (p q) p))) entry)
; (entry (lambda (p q) p))
; ((lambda (m) (m "key" "value")) (lambda (p q) p))
; ((lambda (p q) p) "key" "value")
; "key"

(define my-cdr (lambda (z)
  (z (lambda (p q) q))))

(define test (lambda ()
  (define entry (my-cons "key" "value"))
  (display (my-car entry))
  (newline)
  (display (my-cdr entry))
  (newline)))

