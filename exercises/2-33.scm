;;;; 2-33
;;;; 2-3-3
;;;; 2022/04/07

(define (reload)
  (load "2-33.scm"))
(load "print.scm")

;;; Fill in the missing expressions to complete the following definitions of
;;; some basic list-manipulation operations as accumulations:

; (define map (lambda (p sequence)
;   (accumulate (lambda (x y) <??>)
;               '()
;               sequence)))
; (define append (lambda (seq1 seq2)
;   (accumulate cons <??> <??>)))
; (define length (lambda (sequence)
;   (accumulate <??> 0 sequence)))

(define accumulate (lambda (op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence))))))
(define my-map (lambda (p sequence)
  (define op (lambda (x y)
    (append (list (square x)) y)))
  (accumulate op '() sequence)))
(define my-append (lambda (seq1 seq2)
  (accumulate cons seq2 seq1)))
(define my-length (lambda (sequence)
  (define op (lambda (x y)
    (1+ y)))
  (accumulate op 0 sequence)))

(define (test)
  (print (my-map square (list 0 1 2 3)))
  (print (my-append (list 0 1 2 3) (list 4 5 6 7)))
  (print (my-length (list 0 1 2 3))))

