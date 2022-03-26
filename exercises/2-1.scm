;;;; 2-1
;;;; 2-1-1
;;;; 2022/03/26

;;; Define a better version of `make-rat` that handles both positive and
;;; negative arguments. `make-rat` should normalize the sign so that if the
;;; rational number is positive, both the numerator and denominator are
;;; positive, and if the rational number is negative, only the numerator is
;;; negative.

(define make-rat (lambda (n d)
  (define g (gcd n d))
  (cons (/ n g) (/ d g))))
(define new-rat (lambda (n d)
  (define g (gcd n d))
  (if (or (and (positive? n) (negative? d))
          (and (negative? n) (negative? d)))
    (cons (/ (- n) g) (/ (- d) g))
    (cons (/ n g) (/ d g)))))

(define print-rat (lambda (x)
  (define n (car x))
  (define d (cdr x))
  (display n)
  (display "/")
  (display d)
  (newline)))

(define test (lambda ()
  (define n_1 1000)
  (define n_2 (- 1000))
  (define d   (- 10000))
  (print-rat (make-rat n_1 d))
  (print-rat (make-rat n_2 d))
  (newline)
  (print-rat (new-rat n_1 d))
  (print-rat (new-rat n_2 d))))

