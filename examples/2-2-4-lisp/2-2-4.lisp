;;;; 2-2-4
;;;; 2022/04/21

(defun make-vector (IxI IyI)
  "Double -> Double -> Vector"
  (cons IxI IyI))
(defun vector-IxI (vector)
  (car vector))
(defun vector-IyI (vector)
  (cdr vector))
(defmacro vector-expand (lst &body body)
  "lst: ((vector vector-IxI-name vector-IyI-name)*)"
  (let ((lst-kai (map 'list
                      (lambda (element)
                        ;; element: (vector vector-IxI-name vector-IyI-name)
                        (let ((vector (car element))
                              (vector-IxI-name (cadr element))
                              (vector-IyI-name (caddr element)))
                          `((,vector-IxI-name (vector-IxI ,vector))
                            (,vector-IyI-name (vector-IyI ,vector)))))
                      lst)))
    ;; lst-kai: (`((,vector-IxI-name (vector-IxI ,vector))
    ;;             (,vector-IyI-name (vector-IyI ,vector)))*)
    (let ((lst-kai-ni (reduce 'append lst-kai)))
      ;; lst-kai-ni: `((,vector-IxI-name (vector-IxI ,vector))*
      ;;               (,vector-IyI-name (vector-IyI ,vector))*)
      `(let (,@lst-kai-ni) ,@body))))
(defun vector-add (vector1 vector2)
  "Vector -> Vector -> Vector"
  (vector-expand ((vector1 vector1-IxI vector1-IyI)
                  (vector2 vector2-IxI vector2-IyI))
                 (make-vector (+ vector1-IxI vector2-IxI)
                              (+ vector1-IyI vector2-IyI))))
(defun vector-sub (vector1 vector2)
  "Vector -> Vector -> Vector"
  (vector-expand ((vector1 vector1-IxI vector1-IyI)
                  (vector2 vector2-IxI vector2-IyI))
                 (make-vector (- vector1-IxI vector2-IxI)
                              (- vector1-IyI vector2-IyI))))
(defun vector-scale (number vector)
  "Number -> Vector -> Vector"
  (vector-expand ((vector vector-IxI vector-IyI))
                 (make-vector (* number vector-IxI)
                              (* number vector-IyI))))

(defun make-segment (start end)
  "Vector -> Vector -> Segment"
  (cons start end))
(defun segment-start (segment)
  (car segment))
(defun segment-end (segment)
  (cdr segment))

(defun make-frame (origin edge1 edge2)
  "Vector -> Vector -> Vector -> Frame"
  (cons origin (cons edge1 edge2)))
(defun frame-origin (frame)
  (car frame))
(defun frame-edge1 (frame)
  (cadr frame))
(defun frame-edge2 (frame)
  (cddr frame))
(defun frame-coord-map (frame)
  (lambda (v)
    (vector-add (frame-origin frame)
                (vector-add (vector-scale (vector-IxI v)
                                          (frame-edge1 frame))
                            (vector-scale (vector-IyI v)
                                          (frame-edge2 frame))))))
(defun make-painter (segments)
  "List<Segment> -> Painter, Painter: Frame -> nil, draw something inside the frame."
  (lambda (frame)
    (map nil
         (lambda (segment)
           (draw-line (funcall (frame-coord-map frame)
                               (segment-start segment))
                      (funcall (frame-coord-map frame)
                               (segment-end segment))))
         segments)))
(defun transform-painter (painter origin corner1 corner2)
  "Painter -> Vector -> Vector -> Vector -> Function"
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (funcall m origin))
            (new-edge1 (vector-sub (funcall m corner1) new-origin))
            (new-edge2 (vector-sub (funcall m corner2) new-origin)))
        (painter (make-frame new-origin new-edge1 new-edge2))))))

(defun beside (painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left  (transform-painter painter1
                                          (make-vector 0 0)
                                          split-point
                                          (make-vector 0 1)))
          (paint-right (transform-painter painter2
                                          split-point
                                          (make-vector 1 0)
                                          (make-vector 0.5 1))))
      (lambda (frame) (funcall paint-left frame) (funcall paint-right frame)))))
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

(defun wave2 ()
  (beside wave (flip-vert wave)))
(defun flipped-pairs (painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(defun wave4 ()
  (flipped-pairs wave))

(defun up-split     (painter n)
  (if (= n 0) painter
    (let ((smaller (up-split    painter (- n 1))))
      (below painter (beside smaller smaller)))))
(defun right-split  (painter n)
  (if (= n 0) painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))
(defun corner-split (painter n)
  (if (= n 0) painter
    (let ((up     (up-split     painter (- n 1)))
          (right  (right-split  painter (- n 1)))
          (corner (corner-split painter (- n 1))))
      (let ((top-left     (beside up up))
            (bottom-right (below right right)))
        (beside (below painter top-left)
                (below bottom-right corner))))))
