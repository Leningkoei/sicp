;;;; 2-36
;;;; 2-2-3
;;;; 2022/04/08

(load "print.scm")
(define (reload)
  (load "2-36.scm"))

;;; The procedure `accumulate-n` is similar to `accumulate` except that it takes
;;; as its third argument a sequence of sequences, which are all assumed to have
;;; the same number of elements. It applies the designated accumulation
;;; procedure to combine all the first elements of the sequences, all the second
;;; elements of the sequences, and so on, and returns a sequence of the results.
;;; For instance, if `s` is a sequence containing four sequences, `((1 2 3) (4 5
;;; 6) (7 8 9) (10 11 12)), then the value of `(accumulate-n + 0 s)` should be
;;; the sequence `(22 26 30)`. Fill in the missing expressions in the following
;;; definition of `accumulate-n`:

; (define accumulate-n (lambda (op init seqs)
;   (if (null? (car seqs))
;     '()
;     (cons (accumulate   op init <??>)
;           (accumulate-n op init <??>)))))

(load "accumulate.scm")

(define accumulate-n (lambda (op init seqs)
  ; (print seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate   op init (map (lambda (seq) (car seq)) seqs))
          (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs))))))

(define (test)
  (define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
  (print (accumulate-n + 0 s)))

