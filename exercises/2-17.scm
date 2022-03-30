;;;; 2-17
;;;; 2-2-1
;;;; 2022/03/30

;;; Define a procedure `last-pair` that returns the list that contains only the
;;; last element of a given (nonempty) list:
;;; (last-pair (list 23 72 149 34))
;;; (34)

(define my-last-pair (lambda (items)
  (define last.index (-1+ (length items)))
  (define last.value (list-ref items last.index))
  (list last.value)))

(define equal-print (lambda (x y)
  (display (equal? x y))
  (newline)))
(define (test)
  (define a-list (list 0 1 2 3))
  (define a.last-pair (list 3))
  (equal-print (last-pair a-list) (last-pair a-list))
  (equal-print (last-pair a-list) a.last-pair)
  (equal-print (last-pair a-list) (my-last-pair a-list)))

