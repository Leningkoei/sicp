;;;; 3-08
;;;; 3-1-3
;;;; 2022/05/19

;;; When we defined the evaluation model in section 1.1.3, we side that the
;;; first step in evaluating an expression is to evaluate its
;;; subexpressions. But we never specified the order in which the
;;; subexpressions should be evaluate (e.g., left to right or right to
;;; left). When we introduce assignment, the order in which the arguments to a
;;; procedure are evaluated can make a difference to the result. Define a
;;; simple procedure `f` such that evaluating `(+ (f 0) (f 1))` will return `0`
;;; if the arguments to `+` are evaluated from left to right but will return `1`
;;; if the arguments are evaluated from right to left.

(let ((flag t))
  (defun f (x)
    (if flag
        (progn (setf flag nil) x)
        (progn (setf flag t) 0))))
