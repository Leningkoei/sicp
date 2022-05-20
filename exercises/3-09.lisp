;;;; 3-09
;;;; 3-2-2
;;;; 2022/05/20

;;; IN section 1.2.1 we used the substitution model to analyze two procedures
;;; for computing factorials, a recursive version
(defun factorial (n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(defun factorial-iter (n)
  (labels ((iterator (product counter)
             (if (> counter n)
                 product
                 (iterator (* counter product) (+ counter 1)))))
    (iterator 1 1)))
;;; Show the environment structures created by evaluating `(factorial 6)` using
;;; each version of the `factorial` procedure.
