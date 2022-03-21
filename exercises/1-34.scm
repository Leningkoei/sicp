;;;; 1-34
;;;; 1-3-2
;;;; 2022/03/21

;;; Suppose we define the procedure

(define f (lambda (g)
  (g 2)))

;;; Then we have
; (f square)                        4
; (f (lambda (z) (* z (+ z 1))))    6
;;; What happens if we (perversely) ask the interpreter to evaluate the
;;; combination (f f)? Explain.

; (f f)
; (f 2)
; (2 2)
;  ^
; Error: `2` is not applicable.

