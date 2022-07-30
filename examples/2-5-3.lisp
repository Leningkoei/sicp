;;;; 2-5-3
;;;; 2022/07/30

(defun add-terms (l1 l2)
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l1)
        ('t
         (let ((t1 (first-term l1))
               (t2 (first-term l2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term t1 (add-terms (rest-terms l1) l2)))
                 ((< (order t1) (order t2))
                  (adjoin-term t2 (add-terms l1 (rest-terms l2))))
                 ('t
                  (adjoin-term
                   (make-term (order t1) (add (coeff t1) (coeff t2)))
                   (add-term (rest-terms l1) (rest-terms l2)))))))))
(defun mul-term-by-all-terms (t1 l)
  (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t2 (first-term l)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms l))))))
(defun mul-terms (l1 l2)
  (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
                 (mul-terms (rest-term l1) l2))))

(defun install-polynomial-package ()
  ;; internal procedures
  ;; representation of poly
  (labels ((make-poly (variale term-list)
             (cons variable term-list))
           (variable (p)
             (car p))
           (term-list (p)
             (cdr p))
           (variable? (x)
             (symbolp x))
           (same-variable? (v1 v2)
             (and (variable? v1)
                  (variable? v2)
                  (equal v1 v2)))
           ;; representation of terms and term lists
           ;; (adjoin-terms
           (add-ploy (p1 p2)
             (if (same-variable? (variable p1) (variable p2))
                 (make-ploy (variable p1)
                            (add-terms (term-list p1) (term-list p2)))
                 (error (format '()
                                "Polys not in same var -- ADD-PLOY ~A"
                                `(,p1 ,p2)))))
           (mul-ploy (p1 p2)
             (if (same-variable? (variable p1) (variable p2))
                 (make-ploy (variable p1)
                            (mul-terms (term-list p1) (term-list p2)))
                 (error (format '()
                                "Ploys not in same var -- MUL-PLOY ~A"
                                `(,p1 ,p2)))))
           ;; interface to rest of system
           (tag (p) (attach-tag 'polynomial p)))
    (put 'add '(polynomial polynomial)
         (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
         (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial
         (lambda (variable term-list) (tag (make-poly))))
    'done))
