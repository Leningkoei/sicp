;;;; 1-37
;;;; 1-3-3
;;;; 2022/03/23

;;; a. An infinite `continued fraction` is an expression of the form
;;;             N_1
;;; f = -------------------
;;;               N_2
;;;     D_1 + -------------
;;;                  N_3
;;;          D_2 + --------
;;;               D_3 + ...
;;; As an example, one can show that the infinite continued fraction expansion
;;; with the `N_i` and the `D_i` all equal to `1` produces `1 / Phi`, where
;;; `Phi` is the golden ratio (described in section 1.2.2). One way to
;;; approximate an infinite continued fraction is to truncate the expansion
;;; after a given number of terms. Such a truncation -- a so-called `k-term
;;; finite continued fraction` -- has the form
;;;           N_1
;;; f = ---------------
;;;              N_2
;;;     D_1 + ---------
;;;           .
;;;            .    N_k
;;;             . + ---
;;;                 D_k
;;; Suppose that `n` and `d` are procedures of one argument (the term index `i`)
;;; that return the `N_i` and `D_i` of the terms of the continued fraction.
;;; Define a procedure `cont-frac` such that evaluating `(cont-frac n d k)`
;;; computes the value of the `k-term` finite continued fraction. Check your
;;; procedure by approximation `1 / Phi` using
;;; (cont-frac (lambda (i) 1.0)
;;;            (lambda (i) 1.0)
;;;            k)
;;; for successive values of `k`. How large must you make `k` in order to get an
;;; approximation that is accurate to 4 decimal places?

(define cont-frac (lambda (get-n get-d k)
  (let ((n (get-n k))
        (d (get-d k)))
    (if (> k 0)
      (/ n (+ d (cont-frac get-n get-d (- k 1))))
      (/ n d)))))

;;; b. If your `cont-frac` procedure generates a recursive process, write one
;;; that generates an iterative process. If it generates an iterative process,
;;; write one that generates a recursive process.

(define cont-frac-kai (lambda (get-n get-d k)
  (define iterator (lambda (i n/d)
    (let ((n (get-n i))
          (d (get-d i)))
      (if (> i 0)
        (iterator (-1+ i) (/ n (+ d n/d)))
        n/d))))
  (iterator k 0)))

(define cont-frac-kai-ni (lambda (get-n get-d)
  (define iterator (lambda (i n/d)
    (let ((n (get-n i))
          (d (get-d i)))
      (let ((result (/ n (+ d n/d))))
        (if (>= (abs (- result n/d)) 0.0001)
          (iterator (1+ i) result)
          (list result i))))))
  (iterator 0 0)))

