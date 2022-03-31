;;;; 2-2-1: Representing Sequences
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

;;; Mapping over lists

;;; One extremely useful operation is to apply some transformation to each
;;; element in a list and generate the list of results. For instance, the
;;; following procedure scales each number in a list by a given factor:

(define scale-list (lambda (items factor)
  (if (null? items)
    '()
    (cons (* (car items) factor)
          (scale-list (cdr items) factor)))))
;;; (scale-list (list 1 2 3 4 5) 10)
;;; (10 20 30 40 50)

;;; We can abstract this general idea and capture it as a common pattern
;;; expressed as a higher-order procedure, just as in section 1.3. The
;;; higher-order procedure here is called `map`. `map` takes as arguments a
;;; procedure of one argument and a list, and returns a list of the results
;;; produced by applying the procedure to each element in the list:

(define my-map (lambda (proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (map proc (cdr items))))))
;;; (my-map square (list 1 2 3 4))
;;; (1 4 9 16)

;;; Now we can give a new definition of `scale-list` in term of `map`:

(define new-scale-list (lambda (items factor)
  (map (lambda (x) (* x factor)) items)))

;;; `map` is an important construct, not only because it captures a common
;;; pattern, but because it establishes a higher level of abstraction in dealing
;;; with lists. In the original definition of `scale-list`, the recursive
;;; recursive structure of the program draws attention to the element-by-element
;;; processing of the list. Defining `scale-list` in terms of `map` suppresses
;;; level of detail and emphasizes that scaling transforms a list of elements to
;;; a list of results. The difference between the two definitions is not that
;;; the computer is performing a different process (it isn't) but that we think
;;; about the process differently. In effect, `map` helps establish an
;;; abstraction barrier that isolates the implementation of procedures that
;;; transform lists from the details of how the elements of the list are
;;; extracted and combined. Like the barriers shown in figure 2.1, this
;;; abstraction gives us the flexibility to change the low-level details of how
;;; sequences are implemented, while preserving the conceptual framework of
;;; operations that transform sequences to sequences. Section 2.2.3 expands on
;;; this use of sequences as a framework for organizing programs.

