;;;; 1-03
;;;; 1-1-6
;;;; 2022/03/12

;;; Define a procedure that takes three numbers as arguments and returns the
;;; sum of the squares of the two larger numbers.

(define (biggest-of-three a b c)
  (cond ((and (>= a b) (>= a c)) a)
        ((and (>= b a) (>= b c)) b)
        ((and (>= c a) (>= c b)) c)))
(define (middle-of-three  a b c)
  (cond ((or (and (>= a b) (<= a c)) (and (<= a b) (>= a c))) a)
        ((or (and (>= b a) (<= b c)) (and (<= b a) (>= b c))) b)
        ((or (and (>= c a) (<= c b)) (and (<= c a) (>= c b))) c)))
(define (solute a b c)
  (+ (square (biggest-of-three a b c))
     (square (middle-of-three  a b c))))

