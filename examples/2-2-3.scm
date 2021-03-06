;;;; 2-2-3: Sequences as Conventional Interfaces
;;;; 2022/04/06

(load "../stdp.scm")
(define (reload)
  (load "2-2-3.scm"))

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

(define Sequence.filter:Sequence (lambda (predicate:Function this:Sequence)
  (cond ((null? this:Sequence) '())
        ((predicate:Function (car this:Sequence))
          (cons (car this:Sequence)
                (Sequence.filter:Sequence predicate:Function
                                          (cdr this:Sequence))))
        (else (filter predicate:Function (cdr this:Sequence))))))
(define Sequence.accumulate:<operation:Function>
  (lambda (operation:Function initial this:Sequence)
    (if (null? this:Sequence)
      initial
      (operation:Function (car this:Sequence)
                          (Sequence.accumulate:<operation:Function>
                            operation:Function
                            initial
                            (cdr this:Sequence))))))
(define enumerate-interval:Sequence  (lambda (low:Interval high:Interval)
  (if (> low:Interval high:Interval)
    '()
    (cons low:Interval
          (enumerate-interval:Sequence (1+ low:Interval) high:Interval)))))
(define Tree.enumerate:Sequence (lambda (this:Tree)
  (cond ((null? this:Tree) '())
        ((not (pair? this:Tree)) (list this:Tree))
        (else (append (Tree.enumerate:Sequence (car this:Tree))
                      (Tree.enumerate:Sequence (cdr this:Tree)))))))
(define Tree.sum-odd-square:Number (lambda (this:Tree)
  (Sequence.accumulate:<operation:Function>
    + 0 (map square (Sequence.filter:Sequence
                      odd? (Tree.enumerate:Sequence this:Tree))))))

;;; Nested Mappings

(define flatmap (lambda (proc seq)
  (accumulate append nil (map proc seq))))
(define prime-sum? (lambda (pair)
  (prime? (+ (car pair) (cadr pair)))))
(define make-pair-sum (lambda (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))))
(define prime-sum-pairs (lambda (n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                          (enumerate-interval:Sequence 1 (-1+ i))))
                        (enumerate-interval:Sequence 1 n))))))
(define permutations (lambda (s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove (lambda (y) (= x y)) s))))
             s))))

(define (test)
  (print (sum-odd-squares (list 0 1 2 3)))
  (print (even-fibs 3))
  (print (Sequence.filter:Sequence odd? (list 0 1 2 3)))
  (print (Sequence.accumulate:<operation:Function> + 0 (list 0 1 2 3)))
  (print (enumerate-interval:Sequence 2 7))
  (print (Tree.enumerate:Sequence (list 1 (list 2 (list 3 4)) 5)))
  (print (flatmap (lambda (i) (map (lambda (j) (list i j))
                                   (enumerate-interval:Sequence 1 (-1+ i))))
                  (list 1 2 3 4 5 6)))
  (print (prime-sum-pairs 6))
  (print (permutations (list 0 1 2 3 4))))

