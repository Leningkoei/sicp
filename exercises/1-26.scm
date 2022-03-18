;;;; 1-26
;;;; 1-2-6
;;;; 2022/03/18

;;; Louis Reasoner is having great difficulty doing exercise 1.24. His
;;; `fast-prime?` test seems to run more slowly than his `prime?` test. Louis
;;; calls his friend Eva Lu Ator over to help. When they examine Louis's code,
;;; they find that he has rewritten the `expmod` procedure to use an explicit
;;; multiplication, rather than calling `square`:

(define expmod (lambda (base exponent mod)
  (cond ((= 0  exponent) 1)
        ((even? exponent) (remainder (* (expmod base (/ exponent 2) mod)
                                        (expmod base (/ exponent 2) mod))
                                     mod))
        (else (remainder (* base (expmod base (-1+ exponent) mod))
                         mod)))))

;;; Complex `expmod` in parameters is terrible.

