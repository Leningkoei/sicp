;;;; 2-21
;;;; 2-2-1
;;;; 2022/03/31

(define print (lambda (content)
  (display content)
  (newline)))

;;; The procedure `square-list` takes a list of numbers as argument and returns
;;; a list of the squares of those numbers.
;;; (square-list (list 1 2 3 4))
;;; (1 4 9 16)

;;; Here are two different definitions of `square-list`. Complete both of them
;;; by filling in the missing expressions:

; (define square-list (lambda (items)
;   (if (null? items)
;     nil
;     (cons <??> <??>))))
; (define square-list-kai (lambda (items)
;   (map <??> <??>)))

(define square-list (lambda (items)
  (if (null? items)
    '()
    (cons (square (car items)) (square-list (cdr items))))))
(define square-list-kai (lambda (items)
  (map square items)))

(define (test)
  (define items (list 0 1 2 3))
  (print (square-list items))
  (print (square-list-kai items)))

