;;;; 2-48
;;;; 2-2-4
;;;; 2022/04/11

;;; A directed line segment in the plane can be represented as a pair of vectors
;;; -- the vector running from the origin to the start-point of the segment, and
;;; the vector running from the origin to the end-point of the segment. Use your
;;; vector representation from exercise 2.46 to define a representation for
;;; segments with a constructor `make-segment` and selectors `start-segment` and
;;; `end-segment`.

(define Segment::make-segment->Segment
  (lambda (start-segment:Vector end-segment:Vector)
    (cons start-segment:Vector end-segment:Vector)))
(define Segment::start-segment->Vector (lambda (this:Segment)
  (car this:Segment)))
(define Segment::end-segment->Vector (lambda (this:Segment)
  (cdr this:Segment)))

