;;;; 2-87
;;;; 2-5-3
;;;; 2022/08/09

;;; Install `=zero?` for polynomials in the generic arithmetic package. This
;;; will allow `adjoin-term` to work for polynomials with coefficients that are
;;; themselves polynomials.

(put '=zero? '(term)
     #'empty-termlist?)
