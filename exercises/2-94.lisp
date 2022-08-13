;;;; 2-94
;;;; 2-5-3
;;;; 2022/08/13

;;; Using `div-terms`, implement the procedure `remainder-terms` and use this to
;;; define `gcd-terms` as above. Now write a procedure `gcd-poly` that computes
;;; the polynomial GCD of two polys. (The procedure should signal an error if
;;; the two polys are not in the same variable.) Install in the system a generic
;;; operation `greatest-common-divisor` that reduces to `gcd-poly` for
;;; polynomials and to ordinary gcd for ordinary numbers. As a test, try

(defparameter p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(defparameter p2 (make-polynomial 'x '((3 1) (1 -1))))
(greatest-common-divisor p1 p2)

;;; and check your result by hand.

(defun remainder-terms (l1 l2)
  (cadr (div-terms l1 l2)))

(defun gcd-terms (l1 l2)
  (if (empty-termlist? l2)
      l1
      (gcd-terms l2 (remainder-terms l1 l2))))

(defun gcd-poly (p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (gcd-terms (term-list p1) (term-list p2))
      (error
       (format '() "Polys are not in same var -- GCD-POLY ~A" `(,p1 ,p2)))))
