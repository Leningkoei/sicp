;;;; 1-46
;;;; 1-3-4
;;;; 2022/03/26

;;; Several of the numerical methods described in this chapter are instances of
;;; an extremely general computational strategy known as `iterative
;;; improvement`. Iterative improvement says that, to compute something, we
;;; start with an initial guess for the answer, test if the guess is good
;;; good enough, and otherwise improve the guess and continue the process using
;;; the improved guess as the new guess. Write a procedure `iterative-improve`
;;; that takes two procedures as arguments: a method for telling whether a guess
;;; is good enough and a method for improving a guess. `Iterative-improve`
;;; should return as its value a procedure that takes a guess argument and keeps
;;; improving the guess until it is good enough. Rewrite the `sqrt` procedure of
;;; section 1.1.7 and the `fixed-point` procedure of section 1.3.3 in terms of
;;; `iterative-improve`.

(define display-line (lambda (content)
  (display content)
  (newline)))

(define tolerance 0.00001)
(define distance (lambda (a b)
  (abs (- a b))))
(define average (lambda (a b)
  (/ (+ a b) 2)))

(define my-sqrt (lambda (x)
  ; (define good-enough? (lambda (guess)             ; --
  ;   (< (distance (square guess) x) tolerance)))    ; --
  (define good-enough? (lambda (guess next)          ; ++
    (< (distance guess next) tolerance)))            ; ++
  (define improve (lambda (guess)
    (average guess (/ x guess))))
  (define try (lambda (guess)
    ; (if (good-enough? guess)                       ; --
    ;   guess                                        ; --
    ;   (try (improve guess)))))                     ; --
    (define next (improve guess))                    ; ++
    (if (good-enough? guess next)                    ; ++
      next                                           ; ++
      (try next))))                                  ; ++
  (* 1.0 (try 1))))

(define fixed-point (lambda (f first-guess)
  (define good-enough? (lambda (guess next)
    (< (distance guess next) tolerance)))
  (define improve f)
  (define try (lambda (guess)
    (define next (improve guess))
    (if (good-enough? guess next)
      next
      (try next))))
  (* 1.0 (try first-guess))))

(define iterative-improve (lambda (good-enough? improve)
  (define try (lambda (guess)
    (define next (improve guess))
    (if (good-enough? guess next)
      next
      (try next))))
  try))

(define my-sqrt-kai (lambda (x)
  (define good-enough? (lambda (guess next)
    (< (distance guess next) tolerance)))
  (define improve (lambda (guess)
    (average guess (/ x guess))))
  (define try (iterative-improve good-enough? improve))
  (* 1.0 (try 1))))
(define fixed-point-kai (lambda (f first-guess)
  (define good-enough? (lambda (guess next)
    (< (distance guess next) tolerance)))
  (define improve f)
  (define try (iterative-improve good-enough? improve))
  (* 1.0 (try first-guess))))

