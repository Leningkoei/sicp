;;;; 3-1
;;;; 3-1-1
;;;; 2022/05/18

;;; An `accumulator` is a procedure that is called repeatedly with a single
;;; numeric argument and accumulates its arguments into a sum. Each time it is
;;; called, it returns the currently accumulated sum. Write a procedure
;;; `make-accumulator` that generates accumulators, each maintaining an
;;; independent sum. The input to `make-accumulator` should sqecify the initial
;;; value of the sum; for example
;;; (define A (make-accumulator 5))
;;; (A 10)
;;; >> 15
;;; (A 10)
;;; >> 25

(defun make-accumulator (init)
  (lambda (x)
    (setf init (+ init x))
    init))

(defun test ()
  (let ((accumulator (make-accumulator 5)))
    (format t "~A" (funcall accumulator 10))
    (fresh-line)
    (format t "~A" (funcall accumulator 10))
    (fresh-line)))
