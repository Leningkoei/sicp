;;;; 1-09
;;;; 1-2-1
;;;; Each of the following two procedures defines a method for adding two
;;;; positive integers in terms of the procedures `inc`, which increments its
;;;; argument by 1, and `dec`, which decrements its argument by 1.

(define (+-kai-kou a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))
(define (+-kai-ni  a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))

;;; (+-kai-kou +4 5)
;;; (if (= +4 0) 5 (inc (+ (dec +4) 5)))
;;; (inc (+ (dec +4) 5))
;;; (inc (+ +3 5))
;;; (inc +8)
;;; +9

;;; (+-kai-ni  +4 5)
;;; (if (= +4 0) 5 (+ (dec +4) (inc 5)))
;;; (+ (dec +4) (inc 5))
;;; (+ +3 (inc 5))
;;; (+ +3 6)
;;; +9

