;;;; 2-41
;;;; 2-3-3
;;;; 2022/04/09

;;; Write a procedure to find all ordered triples of distinct positive integers
;;; `i`, `j`, and `k` less than or equal to a given integer `n` that sum to a
;;; given integer `s`.

;;; I hate opening right.

(load "../stdp.scm")
(define (reload)
  (load "2-41.scm"))

(define three-enumerate-pair-creator:Sequence<Sequence<Integer>>
  (lambda (low:Integer high:Integer)
    (define accumulate:Sequence<Integer> accumulate)
    (define map:Sequence<Sequence<Integer>> map)
    (define map.procedure:Sequence<Integer> (lambda (current-i:Integer)
      (define accumulate:Sequence<Integer> accumulate)
      (define map:Sequence<Sequence<Integer>> map)
      (define map.procedure:Sequence<Integer> (lambda (current-j:Integer)
        (define map:Sequence<Integer> map)
        (define map.procedure:Sequence<Integer> (lambda (current-k:Integer)
          (list current-i:Integer current-j:Integer current-k:Integer)))
        (map:Sequence<Integer>
            map.procedure:Sequence<Integer>
            (enumerate-integer:Sequence<Integer> low:Integer
                                                 current-j:Integer))))
      (accumulate:Sequence<Integer> append '()
        (map:Sequence<Sequence<Integer>> map.procedure:Sequence<Integer>
             (enumerate-integer:Sequence<Integer> low:Integer
                                                  current-i:Integer)))))
    (accumulate:Sequence<Integer> append '()
      (map:Sequence<Sequence<Integer>> map.procedure:Sequence<Integer>
           (enumerate-integer:Sequence<Integer> low:Integer high:Integer)))))

(define solution (lambda (n:Integer)
  (define test (lambda (current-three-pair:Sequence)
    (= n:Integer (+ (car current-three-pair:Sequence)
                    (cadr current-three-pair:Sequence)
                    (caddr current-three-pair:Sequence)))))
  (filter test (three-enumerate-pair-creator:Sequence<Sequence<Integer>>
                 1 n:Integer))))

(define (test)
  (print (three-enumerate-pair-creator:Sequence<Sequence<Integer>> 1 10))
  (print (solution 10)))

