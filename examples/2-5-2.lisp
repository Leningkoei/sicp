;;;; 2-5-2: Combining Data of Different Types
;;;; 2022/07/14

;; to be included in the complex package
(defun add-complex-to-schemenum (z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))
(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

;;; coercion

(defun scheme-number->complex (n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex #'scheme->complex)

(defun apply-generic (op &rest args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tages)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (argu1 (car args))
                    (argu2 (cadr args)))
                (let ((type1->type2 (get-coercion type1 type2))
                      (type2->type1 (get-coercion type2 type1)))
                  (cond ((type1->type2)
                         (apply-generic op (type1->type2 argu1) argu2))
                        ((type2->type1)
                         (apply-generic op argu1 (type2->type1 argu2)))
                        ('t (error (format '() "~A -- ~A ~A"
                                           "No method for these types"
                                           "APPLY-GENERIC"
                                           `(,op ,type-tages))))))))
          (error (format '() "~A -- ~A ~A"
                         "No method for these types"
                         "APPLY-GENERIC"
                         `(,op ,type-tages)))))))
