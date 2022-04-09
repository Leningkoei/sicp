;;;; 2-40
;;;; 2-2-3
;;;; 2022/04/09

;;; Define a procedure `unique-pairs` that, given an integer `n`, generates the
;;; sequence of pairs `(i, j)` with `1 <= j <= i <= n`. Use `unique-pairs` to
;;; simplify the definition of `prime-sum-pairs` given above.

(load "../stdp.scm")
(define (reload)
  (load "2-40.scm"))

(define flatmap (lambda (procedure sequence)
  (accumulate append '() (map procedure sequence))))
(define unique-pairs (lambda (n)
  (define flatmap.procedure:Sequence (lambda (current-i:Integer)
    (define map.procedure:Sequence (lambda (current-j:Integer)
      (list current-i:Integer current-j:Integer)))
    (map map.procedure:Sequence
         (enumerate-integer:Sequence<Integer> 1 current-i:Integer))))
  (flatmap flatmap.procedure:Sequence
           (enumerate-integer:Sequence<Integer> 1 n))))
(define prime-sum? (lambda (pair)
  (prime? (+ (car pair) (cadr pair)))))
(define make-pair-sum (lambda (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))))
(define prime-sum-pairs (lambda (n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n)))))

(define (test)
  (print (unique-pairs 4))
  (print (prime-sum-pairs 4)))

