;;;; 2-2-1 Representing Sequences
;;;; 2022/03/30

;;; List operations

(define my-list-ref (lambda (items n)
  (if (= n 0)
    (car items)
    (my-list-ref (cdr items) (-1+ n)))))
(define my-list-ref-kai (lambda (items n)
  (define iterator (lambda (result i)
    (define result.next (cdr result))
    (define i.next (1+ i))
    (if (< i n)
      (iterator result.next i.next)
      (car result))))
  (iterator items 0)))

(define my-length (lambda (items)
  (if (null? items)
    0
    (1+ (my-length (cdr items))))))
(define my-length-kai (lambda (items)
  (define iterator (lambda (result i)
    (define i.next (1+ i))
    (if (null? result)
      i
      (iterator (cdr result) i.next))))
  (iterator items 0)))

(define my-append (lambda (a b)
  (if (null? a)
    b
    (cons (car a) (my-append (cdr a) b)))))

(define combine-test-list-ref (lambda (items n)
  (display "my-list-ref: ")
  (display (my-list-ref items n))
  (newline)
  (display "my-list-ref-kai: ")
  (display (my-list-ref-kai items n))
  (newline)))
(define combine-test-length (lambda (items)
  (display "my-length: ")
  (display (my-length items))
  (newline)
  (display "my-length-kai: ")
  (display (my-length-kai items))
  (newline)))
(define (test)
  (define items (list 0 1 2 3))
  (define items-kai (list 0 1 2 3))
  (combine-test-length items)
  (combine-test-list-ref items 0)
  (combine-test-list-ref items 1)
  (combine-test-list-ref items 2)
  (combine-test-list-ref items 3)
  (display (my-append items items-kai))
  (newline))

