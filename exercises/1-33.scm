;;;; 1-33
;;;; 1-3-1
;;;; 2022/03/20

(define reload (lambda ()
  (load "./1-33.scm")))

;;; You can obtain an even more general version of `accumulate` (exercise 1.32)
;;; by introducing the notion of a `filter` on the terms to be combined. That
;;; is, combine only those terms derived from values in the range that satisfy
;;; a specified condition. The resulting `filtered-accumulate` abstraction takes
;;; the same arguments as accumulate, together with an additional predicate of
;;; one argument that specifies the filter. Write `filtered-accumulate` as a
;;; procedure. Show how to express the following using `filtered-accumulate`:
;;; a. the sum of the squares of the prime numbers in the interval `a` to `b`
;;; (assuming that you have a `prime?` predicate already written)
;;; b. the product of all the positive integers less than `n` that are
;;; relatively prime to `n` (i.e., all positive integers `i < n` such that
;;; `GCD(i, n) = 1).

(define accumulate (lambda (combiner null-value term i next n)
  (if (< i n)
    (combiner (term i) (accumulate combiner null-value term (next i) next n))
    null-value)))
(define accumulate-kai (lambda (combiner null-value term i next n)
  (define iterator (lambda (i result)
    (if (< i n)
      (iterator (next i) (combiner result (term i)))
      result)))
  (iterator null-value null-value)))

(define filtered-accumulate (lambda (combiner null-value term i next n filter)
  (cond ((not (< i n)) null-value)
        ((not (filter i)) (filtered-accumulate combiner
                                               null-value
                                               term
                                               (next i)
                                               next
                                               n
                                               filter))
        (else (combiner (term i) (filtered-accumulate combiner
                                                      null-value
                                                      term
                                                      (next i)
                                                      next
                                                      n
                                                      filter))))))
(define filtered-accumulate-kai
  (lambda (combiner null-value term i next n filter)
    (define iterator (lambda (i result)
      (cond ((not (< i n)) result)
            ((not (filter i)) (iterator (next i) result))
            (else (iterator (next i) (combiner result (term i)))))))
    (iterator null-value null-value)))

(define filtered-sum (lambda (term i next n filter)
  (filtered-accumulate + 0 term i next n filter)))
(define filtered-sum-kai (lambda (term i next n filter)
  (filtered-accumulate-kai + 0 term i next n filter)))
(define filtered-product (lambda (term i next n filter)
  (filtered-accumulate-kai * 1 term i next n filter)))
(define filtered-product-kai (lambda (term i next n filter)
  (filtered-accumulate-kai * 1 term i next n filter)))

(define prime-only-sum (lambda (i n)
  (define term (lambda (i)
    i))
  (define next (lambda (i)
    (1+ i)))
  (define filter prime?)
  (filtered-sum-kai term i next n filter)))
(define prime-only-sum-kai (lambda (i n)
  (define term (lambda (i)
    i))
  (define next (lambda (i)
    (1+ i)))
  (define filter prime?)
  (filtered-sum-kai term i next n filter)))

(define function (lambda (n)
  (define term (lambda (i)
    i))
  (define next (lambda (i)
    (1+ i)))
  (define filter (lambda (i)
    (= 1 (gcd i n))))
  (filtered-product-kai term 0 next n filter)))

(define test (lambda ()
  (list (prime-only-sum     0 100)
        (prime-only-sum-kai 0 100))))

(define display-line (lambda (content)
  (display content)
  (newline)))
(define prime? (lambda (x)
  ; (display-line x)
  (if (<= x 1)
    #f
    (Miller-Rabin-test x))))
(define Miller-Rabin-test (lambda (n)
  (define iterator (lambda (a)
    (if (< a n)
      ; (define result (= 1 (expmod-and-check a (-1+ n) n)))
      (if (= 1 (expmod-and-check a (-1+ n) n))
        (iterator (1+ a))
        #f)
      #t)))
  (iterator 1)))
  ; (define try-it (lambda (a)
  ;   (= 1 (expmod-and-check a (-1+ n) n))))
  ; (try-it (1+ (random (-1+ n))))))
(define expmod-and-check (lambda (base exponent mod)
  (cond ((= 0 exponent) 1)
        ((even? exponent)
          (remainder-square-and-check (expmod-and-check base (/ exponent 2) mod)
                                      mod))
        (else (remainder (* base (expmod-and-check base (-1+ exponent) mod))
                         mod)))))
(define remainder-square-and-check (lambda (x mod)
  (if (and (not (or (= x 1)
                    (= x (-1+ mod))))
           (= 1 (remainder (square x) mod)))
    0
    (remainder (square x) mod))))

