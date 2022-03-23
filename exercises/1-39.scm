;;;; 1-39
;;;; 1-3-3
;;;; 2022/03/23

;;; A continued fraction representation of the tangent function was published in
;;; 1770 by the German mathematician J.H. Lambert:
;;;                 x
;;; tan(x) = ---------------
;;;                x ** 2
;;;          1 - -----------
;;;                  x ** 2
;;;              3 - -------
;;;                  5 - ...
;;; where `x` is in radians. Define a procedure `(tan-cf x k)` that computes an
;;; approximation to the tangent function based on Lambert's formula. `k`
;;; specifies the number of terms to compute, as in exercise 1.37.

(define cont-frac (lambda (get-n get-d k)
  (define iterator (lambda (i n/d)
    (if (> i 0)
      (let ((n (get-n i))
            (d (get-d i)))
        (let ((next-i (-1+ i))
              (next-n/d (/ n (+ d n/d))))
          (iterator next-i next-n/d)))
      n/d)))
  (* 1.0 (iterator k 0))))

(define my-tan (lambda (x)
  (define get-n (lambda (i)
    (if (= i 1)
      x
      (- (square x)))))
  (define get-d (lambda (i)
    (- (* i 2) 1)))
  (cont-frac get-n get-d 100)))

;;; Why (my-tan 88) is not equal (tan 88)? (>= 88)
;;; What happened in 27~28Pi?

