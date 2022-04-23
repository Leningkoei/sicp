;;;; 2-51
;;;; 2-2-4
;;;; 2022/04/23

;;; Define the `below` operation for painter. `below` takes two painters as
;;; arguments. The resulting painter, given a frame, draws with the first
;;; painter in the bottom of the frame and with the second painter in the top.
;;; Define `below` in two different ways -- first by writing a procedure that
;;; is analogous to the `beside` procedure given above, and again in terms of
;;; `beside` and suitable rotation operations (from exercise 2.50).

(defun below  (painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom (transform-painter painter1
                                           (make-vector 0 0)
                                           (make-vector 1 0)
                                           split-point))
          (paint-top    (transform-painter painter2
                                           split-point
                                           (make-vector 1 0)
                                           (make-vector 0 0.5))))
      (lambda (frame) (funcall paint-bottom frame) (funcall paint-top frame)))))
(defun below-kai (painter1 painter2)
  (let ((helper (lambda (painter (flip-vert (rotate270 painter))))))
    (let ((painter1-kai (funcall helper painter1))
          (painter2-kai (funcall helper painter2)))
      (beside painter1 painter2))))
