;;;; 3-40
;;;; 3-4-2
;;;; 2022/06/29

;;; Give all possible values of `x` that can result from executing

;; (defparameter x 10)
;; (parallel-execute (lambda () (setf x (* x x)))
;;                   (lambda () (setf x (* x x x))))

;;; my answer:

;; a -> b           : 1,000,000
;; a1 -> b -> a2    :    10,000
;; b -> a           : 1,000,000
;; b1 -> a -> b2, 3 :   100,000
;; b1, 2 -> a -> b3 :    10,000
