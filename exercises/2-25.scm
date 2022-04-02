;;;; 2-25
;;;; 2-2-2
;;;; 2022/04/02

(define (reload)
  (load "2-25.scm"))

;;; Give combinations of `car`s and `cdr`s that will pick 7 from each of the
;;; following lists:
;;; (1 3 (5 7) 9)
;;; ((7))
;;; (1 (2 (3 (4 (5 (6 7))))))

(define q0 (list 1 3 (list 5 7) 9))
(define q1 (list (list 7)))
(define q2 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define (a0)
  (cadr (caddr q0)))
(define (a1)
  (caar q1))
(define (a2)
  (cadr (cadr (cadr (cadr (cadr (cadr q2)))))))

(define (test)
  (a0)
  (a1)
  (a2))

