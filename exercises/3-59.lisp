;;;; 3-59
;;;; 3-5-2
;;;; 2022/10/02

;;; In section 2.5.3 we saw how to implement a polynomial arithmetic system
;;; representing polynomials as lists of terms. In a similar way, we can work
;;; with `power series`, such as
;;;
;;;                x^2      x^3         x^4
;;; e^x = 1 + x + ----- + ------- + ----------- + ... ,
;;;                 2      3 * 2     4 * 3 * 2
;;;
;;;               x^2        x^4
;;; cos(x) = 1 - ----- + ----------- - ... ,
;;;                2      4 * 3 * 2
;;;
;;;                x^3           x^5
;;; sin(x) = x - ------- + --------------- - ... ,
;;;               3 * 2     5 * 4 * 3 * 2
;;;
;;; represented as infinite streams. We will represent the series `a_0 + a_1 x +
;;; a_2 x^2 + a_3 x^3 + ...` as the stream whose elements are the coefficients
;;; `a_0, a_1, a_2, a_3, ...`.
;;;
;;; a. The integral of the series `a_0 + a_1 x + a_2 x^2 + a_3 x^3 + ...` is the
;;; series `c + a_0 x + 1/2 a_1 x^2 + 1/3 a_2 x^3 + 1/4 a_3 x^4 + ...` where `c`
;;; is any constant. Define a procedure `integrate-series` that takes as input a
;;; stream `a_0, a_1, a_2, ...` representing a power series and returns the
;;; stream `a_0`, `1/2 a_1`, `1/3 a_2`, ... of coefficients of the non-constant
;;; terms of the integral of the series. (Since the result has no constant term,
;;; it doesn't represent a power series; when we use `integrate-series`, we will
;;; `cons` on the appropriate constant.)
;;;
;;; b. The function `x |-> e^x` is its own derivative. This implies that `e^x`
;;; and the integral of `e^x` and the integral of `e^x` are the same series,
;;; except for the constant term, which is `e^0 = 1`. Accordingly, we can
;;; generate the series for `e^x` as
;;;
;;; (define exp-series
;;;   (cons-stream 1 (integrate-series exp-series)))
;;;
;;; Show how to generate the series for sine and cosine, starting from the facts
;;; that derivative of sine is cosine and the derivative of cosine is the
;;; negative of sine:
;;;
;;; (define cosine-series
;;;   (cons-stream 1 <??>))
;;; (define sine-series
;;;   (cons-stream 0 <??>))

(defmacro cons-stream (car cdr)
  `(cons ,car #'(lambda () ,cdr)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (apply (cdr stream) nil))
(defun stream-null? (stream)
  (null stream))
(defparameter the-empty-stream nil)

(defun stream-map (f &rest streams)
  (if (reduce
       #'(lambda (accumulator current-stream)
           (if accumulator
               t
               (stream-null? current-stream)))
       (cons nil streams))
      the-empty-stream
      (cons-stream
       (apply f (map 'list #'stream-car streams))
       (apply #'stream-map f (map 'list #'stream-cdr streams)))))
(defun stream-scale (stream factor)
  (stream-map #'(lambda (current) (* current factor)) stream))
(defun stream-enumerate-interval (begin end &key (step 1))
  (if (and (not (equal end 'infinite)) (= begin end))
      the-empty-stream
      (cons-stream
       begin
       (stream-enumerate-interval (+ begin step) end :step step))))
(defun stream-for-each (f stream &key (begin 0) (end 'infinite))
  (if (or (stream-null? stream)
          (equal end 0))
      'done
      (progn
        (when (and (= begin 0)
                   (or (equal end 'infinite) (> end 0)))
          (apply f (stream-car stream) nil))
        (stream-for-each
         f (stream-cdr stream)
         :begin (if (= begin 0) 0 (- begin 1))
         :end (if (equal end 'infinite)
                  'infinite
                  (- end 1))))))
(defun print-line (line)
  (format t "~A~%" line))
(defun display-stream (stream &key (begin 0) (end 'infinite))
  (stream-for-each
   #'(lambda (current) (print-line current))
   stream :begin begin :end end))

;;; a

(defun stream-enumerate-reverse-interval (begin end)
  (stream-map
   #'(lambda (current) (/ 1 current))
   (stream-enumerate-interval begin end)))
(defun integrate-series (series)
  (stream-map
   #'(lambda (coefficient integer)
       (* coefficient integer))
   (stream-enumerate-reverse-interval 1 'infinite) series))

;;; b

(defparameter exp-series
  (cons-stream 1 (integrate-series exp-series)))
(defparameter cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(defparameter sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; (defparameter cosine-series-coefficients
;;   (stream-map
;;    #'(lambda (greater less)
;;        (- (/ 1 (* greater less))))
;;    (stream-enumerate-interval 2 'infinite :step 2)
;;    (stream-enumerate-interval 1 'infinite :step 2)))
;; (defun integrate-cosine-series (cosine-series)
;;   (stream-map
;;    #'(lambda (coefficient integer)
;;        (* coefficient integer))
;;    cosine-series-coefficients cosine-series))
;; (defparameter cosine-series
;;   (cons-stream 1 (integrate-cosine-series cosine-series)))
;; 
;; (defparameter sine-series-coefficients
;;   (stream-map
;;    #'(lambda (greater less)
;;        (- (/ 1 (* greater less))))
;;   (stream-enumerate-interval 3 'infinite :step 2)
;;   (stream-enumerate-interval 2 'infinite :step 2)))
;; (defun integrate-sine-series (sine-series)
;;   (stream-map
;;    #'(lambda (coefficient integer)
;;        (* coefficient integer))
;;    sine-series-coefficients sine-series))
;; (defparameter sine-series
;;   (cons-stream 1 (integrate-sine-series sine-series)))

;;; math problem???

;; (display-stream sine-series :end 5)
;; (display-stream cosine-series :end 5)
;; (display-stream exp-series :end 10)

;; (defparameter cosine-series-coefficients
;;   (stream-enumerate-interval 0 'infinite :step 2))
;; (defparameter sine-series-coefficients
;;   (stream-enumerate-interval 1 'infinite :step 2))
;; 
;; (defun integrate-cosine-series (cosine-series)
;;   (stream-map
;;    #'(lambda (coefficient integer)
;;        (- (* coefficient integer)))
;;    cosine-series-coefficients cosine-series))
;; (defun integrate-sine-series (sine-series)
;;   (stream-map
;;    #'(lambda (coefficient integer)
;;        (* coefficient integer))
;;    sine-series-coefficients sine-series))
;; 
;; (defparameter cosine-series
;;   (cons-stream 1 (integrate-sine-series sine-series)))
;; (defparameter sine-series
;;   (cons-stream 0 (integrate-cosine-series cosine-series)))
