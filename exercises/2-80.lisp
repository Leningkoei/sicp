;;;; 2-80
;;;; 2-5-1
;;;; 2022/07/14

;;; Define a generic predicate `=zero?` that tests if its argument is zero, and
;;; install it in the generic arithmetic package. This operation should work for
;;; ordinary numbers, rational numbers, and complex numbers.

(defun =zero? (x)
  (apply-generic '=zero x))

(put '=zero? '(scheme-number)
     (lambda (x) (= x 0)))
(put '=zero? '(rational)
     (lambda (x) (= (numerator x) 0)))
(put '=zero? '(complex)
     (lambda (z)
       (and (= (real-part z) 0)
            (= (imag-part z) 0))))
