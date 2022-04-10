;;;; 2-2-4: A Picture Language
;;;; 2022/04/10

;;; The picture language

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define flipped-pairs (lambda (painter)
  (define painter2 (beside painter (flip-vert painter)))
  (below painter2 painter2)))
; (define wave4 (flipped-pairs wave))

;; (below bottom-one up-one)
(define up-split (lambda (painter n)
  (if (= n 0) painter
    ((lambda ()
      (define smaller (up-split painter (- n 1)))
      (below painter (beside smaller smaller)))))))
;; (beside left-one right-one)
(define right-split (lambda (painter n)
  (if (= n 0) painter
    ((lambda ()
      (define smaller (right-split painter (- n 1)))
      (beside painter (below smaller smaller)))))))

(define corner-split (lambda (painter n)
  (if (= n 0) painter
    ((lambda ()
      (define up     (up-split     painter (- n 1)))
      (define right  (right-split  painter (- n 1)))
      (define corner (corner-split painter (- n 1)))
      (define top-left     (beside up up))
      (define bottom-right (below right right))
      (beside (below painter top-left)
              (below bottom-right corner)))))))

(define square-limit (lambda (painter n)
  (define quarter (corner-split painter n))
  (define half (beside (flip-horiz quarter) quarter))
  (below (flip-vert half) half)))

;;; Higher-order operations

(define square-of-four (lambda (top-right right-bottom bottom-left left-top)
  (lambda (painter)
    (define top    (beside (left-top painter)    (top-right painter)))
    (define bottom (beside (bottom-left painter) (right-bottom painter)))
    (below bottom top))))

(define flipped-pairs (lambda (painter)
  (define combine4 (square-of-four identity flip-vert identity flip-vert))
  (combine4 painter)))
(define square-limit (lambda (painter n)
  (define combine4 (square-of-four flip-horiz identity rotate180 flip-vert))
  (combine4 (corner-split painter n))))

