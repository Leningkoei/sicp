;;;; 3-39
;;;; 3-4-2
;;;; 2022/06/29

;;; Which of the five possibilities in the parallel execution shown above remain
;;; if we instead serialize execution as follows:
;; (defparameter x 10)
;; (defparameter s (make-serializer))
;; (parallel-execute (lambda () (setf x
;;                                    (funcall (funcall #'s (lambda () (* x x))))))
;;                   (funcall #'s (lambda () (setf x (+ x 1)))))

;;; my answer:

;;; define step-A (* x x)
;;; define step-B (setf (eval step-A))
;;; define step-C (setf x (+ x 1))

;;      o
;;      ^
;;     / \
;;    A   C
;;    ^    \
;;   / \    |
;;  B   C   A
;;  |   |   |
;;  |   |   |
;;  C   B   B
;; 101 100 121

;; rest answer from internet:
;; A -> B -> x = 100
;;      C ----------> x = 11
