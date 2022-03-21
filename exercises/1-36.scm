;;;; 1-36
;;;; 1-3-3
;;;; 2022/03/21

;;; Modify `fixed-point` so that it prints the sequence of approximations it
;;; generates, using the `newline` and `display` primitives shown in exercise
;;; 1.22. Then find a solution to `x ** x = 1000` by finding a fixed point of
;;; `x -> log(1000) / log(x). (Use Scheme's primitive `log` procedure, which
;;; computes natural logarithms.) Compare the number of steps this takes with
;;; and without average damping. (Note that you cannot start `fixed-point` with
;;; a guess of `1`, as this would cause division by `log(1) = 0`.)

(define display-line (lambda (content)
  (display content)
  (newline)))
(define fixed-point (lambda (f first-guess)
  (define close-enough? (lambda (x y)
    (< (abs (- x y)) 0.00001)))
  (define try (lambda (x)
    (let ((next (f x)))
      (display-line x)
      (if (close-enough? x next)
        next
        (try next)))))
  (try first-guess)))

(define solution (lambda ()
  (define f (lambda (x)
    (/ (log 1000) (log x))))
  (let ((result (fixed-point f 5)))
    (expt result result))))
(define average (lambda (x y)
  (/ (+ x y) 2)))
(define solution-kai (lambda ()
  (define f (lambda (x)
    (average x (/ (log 1000) (log x)))))
  (let ((result (fixed-point f 5)))
    (expt result result))))

(define test (lambda ()
  (display-line (solution))
  (display "done")
  (newline)
  (display-line (solution-kai))
  (display "done")))

