;;;; 1-31
;;;; 1-3-1
;;;; 2022/03/20

;;; a. The sum procedure is only the simplest of a vast number of similar
;;; abstractions that can be captured as higher-order procedures. Write an
;;; analogous procedure called `product` that returns the product of the values
;;; of a function at points over a given range. Show how to define `factorial`
;;; in terms of `product`. Also use `product` to compute approximations to `pi`
;;; using the formula
;;; pi / 4 = 2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7 * ...

(define product (lambda (term i next n)
  (if (< i n)
    (* (term i)
       (product term (next i) next n))
    1)))
(define factorial (lambda (count)
  (define term (lambda (i)
    (cond ((even? i) (/ (+ i 2) (+ i 3)))
          ((odd?  i) (/ (+ i 3) (+ i 2))))))
  (define next (lambda (i)
    (1+ i)))
  (* (product term 0 next count) 4.0)))

;;; b. If your `procedure` procedure generates a recursive process, write one
;;; that generates an iterative process. If it generates an iterative process,
;;; write one that generates a recursive process.

(define product-kai (lambda (term i next n)
  (define iterator (lambda (i result)
    (if (< i n)
      (iterator (next i) (* result (term i)))
      result)))
  (iterator 0 1)))
(define factorial-kai (lambda (count)
  (define term (lambda (i)
    (cond ((even? i) (/ (+ i 2) (+ i 3)))
          ((odd?  i) (/ (+ i 3) (+ i 2))))))
  (define next (lambda (i)
    (1+ i)))
  (* (product-kai term 0 next count) 4.0)))

