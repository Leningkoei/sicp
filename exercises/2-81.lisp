;;;; 2-81
;;;; 2-5-2
;;;; 2022/07/21

;;; Louis Reasoner has noticed that `apply-generic` may try to coerce the
;;; arguments to each other's type even if they already have the same
;;; type. Therefore, he reasons, we need to put procedures in the coercion table
;;; to "coerce" arguments of each type to their own type. For example, in
;;; addition to the `scheme-number->complex` coercion shown above, he would do:

(defun scheme-number->scheme-number (n) n)
(defun complex->complex (z) z)
(put-coercion 'scheme-number 'scheme-number
              #'scheme-number->scheme-number)
(put-coercion 'complex complex
              #'complex->complex)

;;; a. With Louis's coercion procedures installed, what happens if
;;; `apply-generic` is called with two arguments of type `scheme-number` or two
;;; arguments of type `complex` for an operation that is not found in the table
;;; for those types? For example, assume that we've defined a generic
;;; exponentiation operation:

(defun exp (x y) (apply-generic 'exp x y))

;;; and have put a procedure for exponentiation in the Scheme-number package but
;;; not in any other package:

;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y)))) ; using primitive expt

;;; What happens if we call `exp` with two complex numbers as arguments?

;; answer: from kana
;; result: will be infinite loops
;; when the `apply-generic` searching the coercion procedure for complex
;; number, it will get `complex->complex`, and it will search the coercion
;; procedure for new complex number again.

;;; b. Is Louis correct that something had to be done about coercion with
;;; arguments of the same type, or does `apply-generic` work correctly as is?

;; answer:
;; current `apply-generic` can't handle the coercion with arguments of the same
;; type.

;;; c. Modify `apply-generic` so that it doesn't try coercion if the two
;;; arguments of the same type.

(defun apply-generic (op &rest args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (argu1 (car args))
                    (argu2 (cadr args)))
                (if (equal type1 type2)
                    (error (format '() "No method for these types ~A"
                                   `(,op ,type-tags)))
                    (let ((type1->type2 (get-coercion type1 type2))
                          (type2->type1 (get-coercion type2 type1)))
                      (cond (type1->type2
                             (apply-generic op (type1->type2 argu1) argu2))
                            (type2->type1
                             (apply-generic op argu1 (type2->type1 argu2)))
                            (error (format '() "No method for these types ~A"
                                           `(,op ,type-tags)))))))
              (error (format '() "No method for these types ~A"
                             `(,op ,type-tags))))))))
