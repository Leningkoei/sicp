;;;; 2-47
;;;; 2-2-4
;;;; 2022/04/11

;;; Here are two possible constructors for frames:

(define make-frame (lambda (origin edge1 edge2)
  (list origin edge1 edge2)))
(define make-frame (lambda (origin edge1 edge2)
  (cons origin (cons edge1 edge2))))

;;; For each constructor supply the appropriate selectors to procedure an
;;; implementation for frames.

(define origin (lambda (frame)
  (car frame)))
(define edge1 (lambda (frame)
  (cadr frame)))
(define edge2 (lambda (frame)
  (caddr frame)))

(define origin (lambda (frame)
  (car frame)))
(define edge1 (lambda (frame)
  (cadr frame))))
(define edge2 (lambda (frame)
  (cddr frame)))

