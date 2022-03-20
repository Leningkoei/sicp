;;;; 1-32
;;;; 1-3-1
;;;; 2022/03/20

(define reload (lambda ()
  (load "./1-32.scm")))

(define common-i 0)
(define common-n 10)
(define common-term (lambda (i)
  i))
(define common-next (lambda (i)
  (1+ i)))
(define test (lambda ()
  (= (test-sum)
     (test-auto-sum)
     (test-sum-kai)
     (test-auto-sum-kai))))

;;; a. Show that `sum` and `product` (exercise 1.31) are both special cases of a
;;; still more general notion called `accumulate` that combines a collection of
;;; terms, using some general accumulation function:
;;; (accumulate combiner null-value term a next b)
;;; `Accumulate` takes as arguments the same term and range specifications as
;;; `sum` and `product` can both be defined as simple calls to `accumulate`.

(define sum (lambda (term i next n)
  (if (< i n)
    (+ (term i) (sum term (next i) next n))
    0)))
(define product (lambda (term i next n)
  (if (< i n)
    (* (term i) (product term (next i) next n))
    1)))
(define accumulate (lambda (combiner null-value term i next n)
  (if (< i n)
    (combiner (term i) (accumulate combiner null-value term (next i) next n))
    null-value)))
(define auto-sum (lambda (term i next n)
  (accumulate + 0 term (next i) next n)))

(define test-sum (lambda ()
  (sum common-term common-i common-next common-n)))
(define test-auto-sum (lambda ()
  (sum-auto common-term common-i common-next common-n)))

;;; b. If your `accumulate` procedure generates a recursive process, write one
;;; that generates an iterative process. If it generates an iterative process,
;;; write one that generates a recursive process.

(define sum-kai (lambda (term i next n)
  (define iterator (lambda (i result)
    (if (< i n)
      (iterator (next i) (+ result (term i)))
      result)))
  (iterator 0 0)))
(define product-kai (lambda (term i next n)
  (define iterator (lambda (i result)
    (if (< i n)
      (iterator (next i) (* result (term i)))
      result)))
  (iterator 0 1)))

(define accumulate-kai (lambda (combiner null-value term i next n)
  (define iterator (lambda (i result)
    (if (< i n)
      (iterator (next i) (combiner result (term i)))
      result)))
  (iterator null-value null-value)))
(define auto-sum-kai (lambda (term i next n)
  (accumulate-kai + 0 term i next n)))

(define test-sum-kai (lambda ()
  (sum-kai common-term common-i common-next common-n)))
(define test-auto-sum-kai (lambda ()
  (auto-sum-kai common-term common-i common-next common-n)))

