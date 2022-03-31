;;;; 2-23
;;;; 2-2-1
;;;; 2022/03/31

;;; The procedure `for-each` is similar to `map`. It takes as arguments a
;;; procedure and a list of elements. However, rather than forming a list of the
;;; results, `for-each` just applies the procedure to each of the elements in
;;; turn, from left to right. The values returned by applying the procedure to
;;; the elements are not used at all -- `for-each` is used with procedures that
;;; perform an action, such as printing. For example,
;;; (for-each (lambda (x)
;;;             (newline)
;;;             (display x))
;;;           (list 57 321 88))
;;; 57
;;; 321
;;; 88
;;; The value returned by the call to `for-each` (not illustrated above) can be
;;; something arbitrary, such as true. Given an implementation of `for-each`.

(define my-map (lambda (procedure items)
  (if (null? items)
    '()
    (cons (procedure (car items))
          (my-map procedure (cdr items))))))

(define my-for-each (lambda (procedure items)
  (define continue (lambda (items)
    (procedure (car items))
    (iterator (cdr items))))
  (define iterator (lambda (items)
    (if (not (null? items))
      (continue items))))
  (iterator items)))

(define (test)
  (define items (list 0 1 2 3))
  (define print (lambda (content)
    (display content)
    (newline)))
  (print (my-map square items))
  (my-for-each print items))

