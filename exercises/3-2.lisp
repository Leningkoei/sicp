;;;; 3-2
;;;; 3-1-1
;;;; 2022/05/18

;;; In software-testing applications, it is useful to be able to count the
;;; number of times a given procedure is called during the course of a
;;; computation. Write a procedure `make-monitored that takes as input a
;;; procedure, `f`, that itself takes one input. The result returned by
;;; `make-monitored` is a third procedure, say `mf`, that keeps track of the
;;; number of times it has been called by maintaining an internal counter. If
;;; the input to `mf` is the special symbol `how-may-calls?`, then `mf` returns
;;; the value of the counter. If the input is the special symbol `reset-count`,
;;; then `mf` resets the counter to zero. For any other input, `mf` returns the
;;; result of calling `f` on that input and increments the counter. For
;;; instance,
;;; (define s (make-monitored sqrt))
;;; (s 100)
;;; >> 10
;;; (s 'how-many-calls?)
;;; >> 1

(defun make-moitored (func)
  (let ((count 0))
    (lambda (&rest args)
      (let ((first-arg (car args)))
        (if (equal 'how-many-calls first-arg)
            count
            (progn (setf count (+ count 1)) (apply func args)))))))

(defun test ()
  (let ((s (make-moitored 'sqrt)))
    (format t "~A" (funcall s 4))
    (fresh-line)
    (format t "~A" (funcall s 9))
    (fresh-line)
    (format t "~A" (funcall s 'how-many-calls))
    (fresh-line)))
