;;;; 3-06
;;;; 3-1-2
;;;; 2022/05/19

;;; It is useful to be able to reset a random-number generator to produce a
;;; sequence starting from a given value. Design a new `rand` procedure that is
;;; called with an argument that is either the symbol `generate` or the symbol
;;; `reset` and behaves as follows: `(rand 'generate)` produces a new random
;;; number; `((rand 'reset) <new-value>)` resets the internal state variable to
;;; the designated `<new value>`. Thus, by resetting the state, one can
;;; generate repeatable sequences. These are very handy to have when testing
;;; and debugging programs that use random numbers.

;; (let ((x random-init))
;;   (defun rand ()
;;     (setf x (rand-update x))
;;     x))

(defun rand-update (x) (declare (ignore x))
  (random 10))
(defparameter random-init 0)
(let ((x random-init))
  (defun rand (request)
    (cond ((equal request 'generate)
           (setf x (rand-update x)) x)
          ((equal request 'reset)
           (lambda (new-value)
             (format t "Reset") (fresh-line)
             (setf x (rand-update new-value)) x)))))
(defun test ()
  (format t "~A" (rand 'generate)) (fresh-line)
  (format t "~A" (funcall (rand 'reset) 0)) (fresh-line))
