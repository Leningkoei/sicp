;;;; 2-12
;;;; 2-1-4
;;;; 2022/03/29

;;; Define a constructor `make-center-percent` that takes a center and a
;;; percentage tolerance and products the desired interval. You must also define
;;; a selector `percent` that produces the percentage tolerance for a given
;;; interval. The `center` selector is the same as the one shown above.

; (load "../extended-exercises/2-1-4.scm")
(load "2-08.scm")

;; percent = width / center
;; center * percent = width
(define make-center-percent (lambda (center percent)
  (define width (* center percent 0.01))
  (make-center-width center width)))
(define percent (lambda (interval)
  (define this.center (center interval))
  (define this.width (width interval))
  (* 100 (/ this.width this.center))))

(define test (lambda ()
  (define a-center 2)
  (define a-interval (make-center-percent 2 50))
  (display a-interval)
  (newline)
  (display (percent a-interval))
  (newline)))

