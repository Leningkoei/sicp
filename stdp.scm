;;;; 2022/04/08

(define (print . contents)
  (for-each (lambda (content) (display content) (display " ")) contents)
  (newline))

(define nil '())

(define accumulate fold-right)

(define prime? ((lambda ()
  (define divides? (lambda (a b)
    (= (remainder b a) 0)))
  (define find-divisor (lambda (n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1))))))
  (define smallest-divisor (lambda (n)
    (find-divisor n 2)))
  (define prime? (lambda (n)
    (= n (smallest-divisor n))))
  prime?)))

;; `low` is open and `high` is close.
(define enumerate-interval (lambda (low:Integer high:Integer)
  (if (>= low:Integer high:Integer) '()
    (cons low:Integer (enumerate-interval (1+ low:Integer) high:Integer)))))
(define enumerate-integer:Sequence<Integer> enumerate-interval)

(define flatmap (lambda (procedure:type=> sequence:Sequence<type>)
  (accumulate append '() (map procedure:type=> sequence:Sequence<type>))))

