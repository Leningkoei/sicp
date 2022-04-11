;;;; 2-49
;;;; 2-2-4
;;;; 2022/04/11

;;; Use `segments->painter` to define the following primitive painters:
;;; a. The painter that draws the outline of the designed frame.
;;; b. The painter that draws an "X" by connecting opposite corners of the
;;; frame.
;;; c. The painter that draws a diamond shape by connecting the midpoints of the
;;; sides of the frame.
;;; d. The `wave` painter.

(define (Frame::outline)
  (segment->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
                          (make-segment (make-vect 1 0) (make-vect 1 1))
                          (make-segment (make-vect 1 1) (make-vect 0 1))
                          (make-segment (make-vect 0 1) (make-vect 0 0)))))
(define (Frame::X)
  (segment->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                          (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define (Frame::diamond)
  (segment->painter (list (make-segment (make-vect 0   0)
                                        (make-vect 1   0))
                          (make-segment (make-vect 1   0)
                                        (make-vect 1   2/3))
                          (make-segment (make-vect 1   2/3)
                                        (make-vect 2/3 1))
                          (make-segment (make-vect 2/3 1)
                                        (make-vect 0   1))
                          (make-segment (make-vect 0   1)
                                        (make-vect 0   0)))))

;; void
;; vectors: Sequence<Vector>
;; Sequence<Segment>
(define (combine-vectors . vectors)
  (define next-vectors (append (cdr vectors) (list (car vectors))))
  ;; void
  ;; current-vector: Vector,
  ;; next-vector:    Vector
  ;; Segment
  (define map.procedure (lambda (current-vector next-vector)
    (make-segment current-vector next-vector)))
  (map map.procedure vectors next-vectors))
(define (wave)
  (segment->painter (combine-vectors (make-vect 1/8 0)         ; c
                                     (make-vect 3/8 0)         ; d
                                     (make-vect 4/8 2/8)       ; e
                                     (make-vect 5/8 0)         ; f
                                     (make-vect 7/8 0)         ; g
                                     (make-vect 5/8 4/8)       ; h
                                     (make-vect 1   2/8)       ; i
                                     (make-vect 1   4/8)       ; j
                                     (make-vect 6/8 5/8)       ; k
                                     (make-vect 5/8 5/8)       ; l
                                     (make-vect 6/8 7/8)       ; m
                                     (make-vect 5/8 1)         ; n
                                     (make-vect 4/8 1)         ; o
                                     (make-vect 3/8 7/8)       ; p
                                     (make-vect 4/8 5/8)       ; r
                                     (make-vect 2/8 5/8)       ; s
                                     (make-vect 0   6/8)       ; t
                                     (make-vect 0   3/8)       ; u
                                     (make-vect 1/8 2/8)       ; v
                                     (make-vect 3/8 4/8)       ; w
                                     (make-vect 4/8 3/8))))    ; x

