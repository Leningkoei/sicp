;;;; 1-41
;;;; 1-3-4
;;;; 2022/03/25

;;; Define a procedure `double` that takes a procedure of one argument as
;;; argument and returns a procedure that applies the original procedure twice.
;;; For example, if `inc` is a procedure that adds `1` to its argument, than
;;; `(double inc)` should be a procedure that adds `2`. What value is returned
;;; by
;;; (((double (double double)) inc) 5)

(define double (lambda (single)
  (lambda (x)
    (single (single x)))))

(define inc (lambda (x)
  (1+ x)))
(define test (lambda ()
  (((double (double double)) inc) 5)))
; (((double (double double)) inc) 5)
; (((double (lambda (x) (double (double x)))) inc) 5)
; ; (define function (lambda (x) (double (double x))))
; (((double function) inc) 5)
; (((lambda (x) (function (function x))) inc) 5)
; ((function (function inc)) 5)
; ((function ((lambda (x) (double (double x))) inc)) 5)
; ((function (double (double inc))) 5)
; ((function (double (lambda (x) (inc (inc x))))) 5)
; ; (define f (lambda (x) (inc (inc x))))
; ((function (double f)) 5)
; ((function (lambda (x) (f (f x)))) 5)
; ; (define fu (lambda (x) (f (f x))))
; ((function fu) 5)
; (((lambda (x) (double (double x))) fu) 5)
; ((double (double fu)) 5)
; ((double (lambda (x) (fu (fu x)))) 5)
; ; (define fun (lambda (x) (fu (fu x))))
; ((double fun) 5)
; ((lambda (x) (fun (fun x))) 5)
; (fun (fun 5))
; (fun ((lambda (x) (fu (fu x))) 5))
; (fun (fu (fu 5)))
; (fun (fu ((lambda (x) (f (f x))) 5)))
; (fun (fu (f (f 5))))
; (fun (fu (f ((lambda (x) (inc (inc x))) 5))))
; (fun (fu (f (inc (inc 5)))))
; (fun (fu (f 7)))
; (fun (fu ((lambda (x) (inc (inc x))) 7)))
; (fun (fu (inc (inc 7))))
; (fun (fu 9))
; (fun ((lambda (x) (f (f x))) 9))
; (fun (f (f 9)))
; (fun (f (inc (inc 9))))
; (fun (f 11))
; (fun (inc (inc 9)))
; (fun 11)
; ; Where is wrong?
; ; 21

