;;;; 2-1-1: Arithmetic Operations for Rational Numbers
;;;; 2022/03/26

;;; (make-rat <n> <d>): returns the rational number whose numerator is the
;;;                     integer <n> and whose denominator is the integer <d>.
;;; (numer <x>)       : returns the numerator   of the rational number <x>.
;;; (denom <x>)       : returns the denominator of the rational number <x>.

;; n_1   n_2   n_1 d_2 + n_2 d_1
;; --- + --- = -----------------
;; d_1   d_2        d_1 d_2
(define add-rat (lambda (x y)
  (define n_1 (numer x))
  (define n_2 (numer y))
  (define d_1 (denom x))
  (define d_2 (denom y))
  (make-rat (+ (* n_1 d_2) (* n_2 d_1))
            (* d_1 d_2))))

;; n_1   n_2   n_1 d_2 - n_2 d_1   n_1      n_2
;; --- - --- = ----------------- = --- + (- --- )
;; d_1   d_2        d_1 d_2        d_1      d_2
(define sub-rat (lambda (x y)
  (define n_1 (numer x))
  (define n_2 (numer y))
  (define d_1 (denom x))
  (define d_2 (denom y))
  (make-rat (- (* n_1 d_2) (* n_2 d_1))
            (* d_1 d_2))))

;; n_1   n_2   n_1 n_2
;; --- * --- = -------
;; d_1   d_2   d_1 d_2
(define mul-rat (lambda (x y)
  (define n_1 (numer x))
  (define n_2 (numer y))
  (define d_1 (denom x))
  (define d_2 (denom y))
  (make-rat (* n_1 n_2)
            (* d_1 d_2))))

;; n_1   n_2   n_1 d_2
;; --- / --- = -------
;; d_1   d_2   n_2 d_1
(define div-rat (lambda (x y)
  (define n_1 (numer x))
  (define n_2 (numer y))
  (define d_1 (denom x))
  (define d_2 (denom y))
  (make-rat (* n_1 d_2)
            (* n_2 d_1))))

;; n_1   n_2
;; --- = --- <=> n_1 d_2 = n_2 d_1
;; d_1   d_2
(define equal-rat? (lambda (x y)
  (define n_1 (numer x))
  (define n_2 (numer y))
  (define d_1 (denom x))
  (define d_2 (denom y))
  (= (* n_1 d_2)
     (* n_2 d_1))))

(define make-rat (lambda (n d)
  (define g (gcd n d))
  (cons (/ n g) (/ d g))))
(define numer (lambda (x)
  (car x)))
(define denom (lambda (x)
  (cdr x)))
(define print-rat-line (lambda (x)
  (define n (numer x))
  (define d (denom x))
  (display n)
  (display "/")
  (display d)
  (newline)))

(define test (lambda ()
  (define one-half  (make-rat 1 2))
  (print-rat-line one-half)
  (define one-third (make-rat 1 3))
  (print-rat-line one-third)
  (print-rat-line (add-rat one-half  one-third))
  (print-rat-line (mul-rat one-half  one-third))
  (print-rat-line (add-rat one-third one-third))))

