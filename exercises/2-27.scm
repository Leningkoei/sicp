;;;; 2-27
;;;; 2-2-2
;;;; 2022/04/02

(define (reload)
  (load "2-27.scm"))
(define (print content)
  (display content)
  (newline))

;;; Modify your `reverse` procedure of exercise 2.18 to produce a `deep-reverse`
;;; procedure that takes a list as argument and returns as its value the list
;;; with its elements reversed and with all sublists deep-reversed as well. For
;;; example,

; (define x (list (list 1 2) (list 3 4)))
; (reverse x)         ; ((3 4) (1 2))
; (deep-reverse x)    ; ((4 3) (2 1))

(define (my-reverse items)
  (define (iterator items result)
    (if (null? items)
      result
      (iterator (cdr items) (cons (car items) result))))
  (iterator items '()))

(define (deep-reverse items)
  (define (iterator items result)
    (cond ((null? items) result)
          ((pair? items)
           (iterator (cdr items) (cons (deep-reverse (car items)) result)))
          (else items)))
  (iterator items '()))

(define (test)
  (define x (list (list (list 1 2) (list 3 4)) (list 3 4)))
  (print (my-reverse x))
  (print (deep-reverse x)))

;;; pizza

; (define pi 3.1415926535897)
; (define (area d)
;   (define r (/ d 2))
;   (* pi (square r)))
; (define (print content)
;   (display content)
;   (newline))
; (define (test)
;   (print (area 20.32))
;   (print (area 25.4)))

