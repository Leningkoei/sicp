;;;; 2-89
;;;; 2-5-3
;;;; 2022/08/09

;;; Define procedures that implement the term-list representation described
;;; above as appropriate for dense polynomials.

(defun first-term (dense-term-list)
  (make-term (- (length dense-term-list) 1) (car dense-term-list)))
(defun rest-term (dense-term-list)
  (cdr dense-term-list))

;; ??
