;;;; 2-5-1: Generic Arithmetic Operations
;;;; 2022/07/12

(defun add (x y) (apply-generic 'add x y))
(defun sub (x y) (apply-generic 'sub x y))
(defun mul (x y) (apply-generic 'mul x y))
(defun div (x y) (apply-generic 'div x y))

(defun install-scheme-number-package ()
  (labels ((tag (x)
             (attach-tag 'scheme-number x)))
    (let ((argument-types '(scheme-number scheme-number)))
      (put 'add argument-types
           (lambda (x y) (tag (+ x y))))
      (put 'sub argument-types
           (lambda (x y) (tag (- x y))))
      (put 'mul argument-types
           (lambda (x y) (tag (* x y))))
      (put 'div argument-types
           (lambda (x y) (tag (/ x y))))
      'done)))
(defun make-scheme-number (n)
  (funcall (get 'make 'scheme-number) n))

(defun install-rational-package ()
  ;; internal procedures
  (labels ((numer (x) (car x))
           (denom (x) (cdr x))
           (make-rat (n d)
             (let ((g (gcd n d)))
               (cons (/ n g) (/ d g))))
           (add-rat (x y)
             (make-rat (+ (* (numer x) (denom y))
                          (* (numer y) (denom x)))
                       (* (denom x) (denom y))))
           (sub-rat (x y)
             (make-rat (- (* (numer x) (denom y))
                          (* (numer x) (denom y)))
                       (* (denom x) (denom y))))
           (mul-rat (x y)
             (make-rat (* (numer x) (numer y))
                       (* (denom x) (denom y))))
           (div-rat (x y)
             (make-rat (* (numer x) (denom y))
                       (* (denom x) (numer y))))
           (tag (x) (attach-tag 'rational x)))
    ;; interface to rest of the system
    (let ((argument-types '(rational rational)))
      (put 'add argument-typs
           (lambda (x y) (tag (add-rat x y))))
      (put 'sub argument-typs
           (lambda (x y) (tag (sub-rat x y))))
      (put 'mul argument-types
           (lambda (x y) (tag (mul-rat x y))))
      (put 'div argument-typs
           (lambda (x y) (tag (div-rat x y))))
      (put 'make 'rational
           (lambda (n d) (tag (make-rat n d))))
      'done)))

(defun install-complex-package ()
  (labels (;; imported procedures from rectangular and polar packages
           (make-from-real-imag (x y)
             (funcall (get 'make-from-real-imag 'rectangular) x y))
           (make-from-mag-ang (r a)
             (funcall (get 'make-from'mag-ang 'polar) r a))
           ;; internal procedures
           (add-complex (z1 z2)
             (make-from-real-imag (+ (real-part z1) (real-part z2))
                                  (+ (imag-part z1) (imag-part z2))))
           (sub-complex (z1 z2)
             (make-from-real-imag (- (real-part z1) (real-part z2))
                                  (- (imag-part z1) (imag-part z2))))
           (mul-complex (z1 z2)
             (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                (+ (angle z1) (angle z2))))
           (div-complex (z1 z2)
             (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                (- (angle z1) (angle z2))))
           (tag (z) (attach-tag 'complex z)))
    (let ((argument-typs '(complex complex)))
      (put 'add argument-types
           (lambda (z1 z2) (tag (add-complex z1 z2))))
      (put 'sub argument-types
           (lambda (z1 z2) (tag (sub-complex z1 z2))))
      (put 'mul argument-types
           (lambda (z1 z2) (tag (mul-complex z1 z2))))
      (put 'div argument-types
           (lambda (z1 z2) (tag (div-complex z1 z2))))
      (put 'make-from-real-imag 'complex
           (lambda (x y) (tag (make-from-real-imag x y))))
      (put 'make-from-mag-ang 'complex
           (lambda (r a) (tag (make-from-mag-ang r a))))
      'done)))

(defun make-complex-from-real-imag (x y)
  (funcall (get 'make-from-real-imag 'complex) x y))
(defun make-complex-from-mag-ang (r a)
  (funcall (get 'make-from-mag-ang 'complex) r a))

;; (funcall (get 'make-from-real-imag 'complex) x y) ->
;; (funcall (lambda (x y) (tag (make-from-real-imag x y))) x y) ->
;; (funcall (lambda (x y) (attach-tag 'complex (make-from-real-imag x y))) x y)
