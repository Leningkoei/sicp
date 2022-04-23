;;;; 2-53
;;;; 2-3-1
;;;; 2022/04/23

;;; What would the interpreter print in response to evaluating each of the
;;; following expressions?

;;? (list 'a 'b 'c)
;;: (A B C)

;;? (list (list 'george))
;;: ((GEOREG))
;;? (cdr '((x1 x2) (y1 y2)))
;;: ((Y1 Y2))

;;? (cadr '((x1 x2) (y1 y2)))
;;: (Y1 Y2)
;;? (pair? (car '(a short list)))
;;: #f
;;? (memq 'red '((red shoes) (blue socks)))
;;: #f

;;? (memq 'red '(red shoes blue socks))
;;: #t
