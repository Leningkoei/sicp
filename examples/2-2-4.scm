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

;;; Frames

;; void
;; frame: Frame
;; Function<void
;;          v: Vector
;;          Vector>
(define frame-coord-map (lambda (frame)
  ;; void
  ;; v: Vector
  ;; Vector
  (lambda (v)
              ; origin
    (add-vect (origin-frame frame)
              (add-vert (scale-vect (xcor-vect v) (edge1-frame frame))
                        (scale-vect (ycor-vect v) (edge2-frame frame)))))))

;; Frame
;; origin: Vector,
;; edge1:  Vector(Number . 0),
;; edge2:  Vector(0      . Number)
;; Frame
(define make-frame (lambda (origin edge1 edge2)
  (list origin edge1 edge2)))
(define origin-frame (lambda (frame)
  (car frame)))
(define edge1-frame (lambda (frame)
  (cadr frame)))
(define edge2-frame (lambda (frame)
  (caddr frame)))

;; Vector
;; xcor-vect: Number,
;; ycor-vect: Number
;; Vector
(define make-vect (lambda (xcor-vect ycor-vect)
  (cons xcor-vect ycor-vect)))

(define xcor-vect (lambda (vect)
  (car vect)))
(define ycor-vect (lambda (vect)
  (cdr vect)))

;; Vector
;; vect1: Vector,
;; vect2: Vector
;; Vector
(define add-vect (lambda (vect1 vect2)
  (define vect1.x (xcor-vect vect1))
  (define vect1.y (ycor-vect vect1))
  (define vect2.x (xcor-vect vect2))
  (define vect2.y (ycor-vect vect2))
  (make-vect (+ vect1.x vect2.x) (+ vect1.y vect2.y))))
(define sub-vect (lambda (vect1 vect2)
  (define vect1.x (xcor-vect vect1))
  (define vect1.y (ycor-vect vect1))
  (define vect2.x (xcor-vect vect2))
  (define vect2.y (ycor-vect vect2))
  (make-vect (- vect1.x vect2.x) (- vect1.y vect2.y))))
(define scale-vect (lambda (s vect)
  (define vect.x (xcor-vect vect))
  (define vect.y (ycor-vect vect))
  (make-vect (* s vect.x) (* s vect.y))))

;;; Painters

;; Segment
;; segment-list: Sequence<Segment>
;; Painter: Function<void
;;                   frame: Frame
;;                   void>
(define segments->painter (lambda (segment-list)
  ;; void
  ;; frame: Frame
  ;; void
  (lambda (frame)
              ;; void
              ;; segment: Segment
              ;; void
    (for-each (lambda (segment)
                  ;; void
                  ;; v: Vector
                  ;; Vector
      (draw-line ((frame-coord-map frame) (start-segment segment))
                  ;; void
                  ;; v: Vector
                  ;; Vector
                 ((frame-coord-map frame) (end-segment   segment)))))
              segment-list)))

;; Segment
;; start-segment: Vector,
;; end-segment: Vector
;; Segment
(define make-segment
  (lambda (start-segment end-segment)
    (cons start-segment end-segment)))
;; Segment
;; this: Segment
;; Vector
(define start-segment (lambda (this)
  (car this)))
;; Segment
;; this: Segment
;; Vector
(define end-segment (lambda (this)
  (cdr this)))

