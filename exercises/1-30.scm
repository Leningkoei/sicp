;;;; 1-30
;;;; 1-3-1
;;;; 2022/03/20

;;; The `sum` procedure above generates a linear recursion. The procedure can be
;;; rewritten so that the sum is performed iteratively. Show how to do this by
;;; filling in the missing expressions in the following definition:

; (define sum (lambda (term a next b)
;   (define iterator (lambda (a result)
;     (if <??>
;       <??>
;       (iterator <??> <??>))))
;   (iterator <??> <??>)))

(define sum (lambda (term a next b)
  (define iterator (lambda (a result)
    (if (< a b)
      (iterator (next a) (+ result (term a)))
      result)))
  (iterator 0 0)))

