;;;; 2-92
;;;; 2-5-3
;;;; 2022/08/11

;;; By imposing an ordering on variables, extend the polynomial package so that
;;; addition and multiplication of polynomials works for polynomials in
;;; different variables. (This is not easy!)

;;; `make-polynomial` will fix variables order recursively.

(defun variable> (v1 v2)
  (string> (string v1) (string v2)))
(defun variable< (v1 v2)
  (variable< v2 v1))

(defun normalize (poly v)
  (make-variable v (adjoin-term (make-term 0 poly) (empty-term-list))))

(defun add-poly (p1 p2)
  (cond ((same-variable? (variable p1) (variable p2))
         (make-poly (variable p1) (add-terms (term-list p1) (term-list p2))))
        ((variable> (variable p1) (variable p2))
         (let ((p2-kai (normalize p2)))
           ;; (make-poly (variable p1)
           ;;            (add-terms (term-list p1) (term-list p2-kai)))))
           (add-poly p1 p2-kai)))
        ((variable< (variable p1) (variable p2))
         (let ((p1-kai (normalize p1)))
           ;; (make-poly (variable p2)
           ;;            (add-terms (term-list p1-kai) (term-list p2)))))))
           (add-poly p1-kai p2)))))
(defun mul-poly (p1 p2)
  (cond ((same-variable? (variable p1) (variable p2))
         (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2))))
        ((variable> (variable p1) (variable p2))
         (let ((p2-kai (normalize p2)))
           ;; (make-poly (variable p1)
           ;;            (mul-terms (term-list p1) (term-list p2-kai)))))
           (mul-poly p1 p2-kai)))
        ((variable< (variable p1) (variable p2))
         (let ((p1-kai (normalize p2)))
           ;; (make-poly (variable p2)
           ;;            (mul-terms (term-list p1-kai) (term-list p2)))))))
           (mul-poly p1-kai p2)))))
