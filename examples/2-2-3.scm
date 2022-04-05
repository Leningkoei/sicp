;;;; 2-2-3: Sequences as Conventional Interfaces

(load "print.scm")

(define sum-odd-squares (lambda (tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
          (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree)))))))

(define even-fibs (lambda (n)
  (define next (lambda (k)
    (if (> k n)
      '()
      ((lambda ()
        (define f (fib k))
        (if (even? k)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))))
  (next 0)))
(define fib (lambda (n)
  (define iterator (lambda (a b p q count)
    (cond ((= count 0) b)
          ((even? count) (iterator a
                                   b
                                   (+ (square p) (square q))
                                   (+ (* 2 p q) (square q))
                                   (/ count 2)))
          (else (iterator (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1))))))
  (iterator 1 0 0 1 n)))

;;; Sequence Operations

(define Sequence.filter:Function (lambda (predicate:Function this:Sequence)
  (cond ((null? this:Sequence) '())
        ((predicate:Function (car this:Sequence))
          (cons (car this:Sequence)
                (Sequence.filter:Function predicate:Function
                                          (cdr this:Sequence))))
        (else (filter predicate:Function (cdr this:Sequence))))))
(define Sequence.accumulate:Function
  (lambda (operation:Function initial this:Sequence)
    (if (null? this:Sequence)
      initial
      (operation:Function (car this:Sequence)
                          (Sequence.accumulate:Function operation:Function
                                                        initial
                                                        (cdr this:Sequence))))))

(define (test)
  (print (sum-odd-squares (list 0 1 2 3)))
  (print (even-fibs 3))
  (print (Sequence.filter:Function odd? (list 0 1 2 3)))
  (print (Sequence.accumulate:Function + 0 (list 0 1 2 3))))

