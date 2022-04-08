;;;; 2-38
;;;; 2-2-3
;;;; 2022/04/08

;;; The `accumulate` procedure is also known as `fold-right`, because it
;;; combines the first element of the sequence with the result of combining all
;;; the elements to the right. There is also a `fold-left`, which is similar to
;;; `fold-right`, except that it combines elements working in the opposite
;;; direction:

(define my-fold-left (lambda (op initial sequence)
  (define iterator (lambda (result rest)
    (if (null? rest)
      result
      (iterator (op result (car rest)) (cdr rest)))))
  (iterator initial sequence)))

;;; What are the values of
;;; (fold-right / 1 (list 1 2 3))
;;; (fold-left  / 1 (list 1 2 3))
;;; (fold-right list nil (list 1 2 3))
;;; (fold-left  list nil (list 1 2 3))

;;; (fold-right / 1 (1 2 3))
;;; (/ 1 (fold-right / 1 (2 3)))
;;; (/ 1 (/ 2 (fold-right / 1 (3))))
;;; (/ 1 (/ 2 (/ 3 (fold-right / 1 ()))))
;;; (/ 1 (/ 2 (/ 3 1)))
;;; (/ 1 (/ 2 3))
;;; (/ 1 2/3)
;;; 3/2

;;; (fold-left / 1 (1 2 3))
;;; (iterator 1 (1 2 3))
;;; (iterator (/ 1 1) (2 3))
;;; (iterator 1 (2 3))
;;; (iterator (/ 1 2) (3))
;;; (iterator (/ 1/2 3) ())
;;; 1/6

;;; (fold-right list () (1 2 3))
;;; (list 1 (fold-right list () (2 3)))
;;; (list 1 (list 2 (fold-right list () (3))))
;;; (list 1 (list 2 (list 3 (fold-right list () ()))))
;;; (list 1 (list 2 (list 3 ())))
;;; (list 1 (list 2 (3 ())))
;;; (list 1 (2 (3 ())))
;;; (1 (2 (3 ())))

;;; (fold-left list () (1 2 3))
;;; (iterator () (1 2 3))
;;; (iterator (list () 1) (2 3))
;;; (iterator (list (() 1) 2) (3))
;;; (iterator (list ((() 1) 2) 3) ())
;;; (((() 1) 2) 3)

