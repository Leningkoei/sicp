;;;; 2-88
;;;; 2-5-3
;;;; 2022/08/09

;;; Extend the polynomial system to include subtraction of polynomials. (Hint:
;;; You may find it helpful to define a generic negation operation.)

(defun sub-poly (p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (add-poly p1 (negate p2))
      (error (format '() "Polys not in same var -- SUB-POLY ~A" `(,p1 ,p2)))))

(defun negate (x)
  (apply-generic 'negate x))

(put 'negate '(integer)
     #'-)
(put 'negate '(rational)
     #'(lambda (q)
         (make-rational (- (numer q) (denom q)))))
(put 'negate '(real)
     #'-)
(put 'negate '(complex)
     #'(lambda (z)
         (make-complex-from-real-imag (negate (real-part z))
                                      (negate (imag-part z)))))
(put 'negate '(polynomial)
     #'(lambda (p)
         (make-poly (variable p) (negate-term-list (term-list p)))))

(defun negate-term-list (term-list)
  (adjoin-term (negate-term      (first-term term-list))
               (negate-term-list (rest-terms term-list))))
(defun negate-term (term)
  (make-term (order term) (negate (coeff term))))
