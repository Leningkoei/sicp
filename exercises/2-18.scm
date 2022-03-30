;;;; 2-18
;;;; 2-2-1
;;;; 2022/03/30

;;; Define a procedure `reverse` that takes a list as argument and returns a
;;; list of the same elements in reverse order:
;;; (reverse (list 1 4 9 16 25))
;;; (25 16 9 4 1)

(define my-reverse (lambda (items)
  (define iterator (lambda (items result)
    (if (null? items)
      result
      (iterator (cdr items) (cons (car items) result)))))
  (iterator items '())))

; (my-reverse (1 4 9 16 25))
; (iterator (1 4 9 16 25) '())
; (iterator (4 9 16 25) (1))
; (iterator (9 16 25) (4 1))
; ...

