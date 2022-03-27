;;;; 2-1-3 What Is Meant by Data?
;;;; 2022/03/27

(define my-cons (lambda (x y)
  (define dispatch (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m)))))
  dispatch))
(define my-car (lambda (z)
  (z 0)))
(define my-cdr (lambda (z)
  (z 1)))

(define entry (lambda (key value)
  (lambda (target)
    (cond ((= target 0) key)
          ((= target 1) value)))))
(define get-key (lambda (entry)
  (entry 0)))
(define get-value (lambda (entry)
  (entry 1)))

