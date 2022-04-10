;;;; 2-45
;;;; 2-2-4
;;;; 2022/04/10

;;; `right-split` and `up-split` can be expressed as instances of a general
;;; splitting operation. Define a procedure `split` with the property that
;;; evaluating

(define right-split (split beside below))
(define up-split    (split below beside))

;;; produces procedures `right-split` and `up-split` with the same behaviors as
;;; the ones already defined.

(define split (lambda (combine-painter-smaller combine-smaller-smaller)
  (define ...-split (lambda (painter n)
    (if (= n 0) painter
      ((lambda ()
        (define smaller (...-split painter (- n 1)))
        (combine-painter-smaller painter
                                 (combine-smaller-smaller smaller
                                                          smaller)))))))))

