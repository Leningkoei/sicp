;;;; 2-50
;;;; 2-2-4
;;;; 2022/04/23

;;; Define the transformation `flip-horiz`, which flips painters horizontally,
;;; and transformations that rotate painters counterclockwise by 180 degrees
;;; and 270 degrees.

(defun flip-horiz (painter)
  (transform-painter painter
                     (make-vector 1 0)
                     (make-vector 0 0)
                     (make-vector 1 1)))
(defun rotate180 (painter)
  (transform-painter painter
                     (make-vector 0 1)
                     (make-vector 1 1)
                     (make-vector 0 0)))
(defun rotate270 (painter)
  (transform-painter painter
                     (make-vector 0 1)
                     (make-vector 0 0)
                     (make-vector 1 1)))
