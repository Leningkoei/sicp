;;;; 3-05
;;;; 3-1-2
;;;; 2022/05/19

;;; `Monte Carlo intergration` is a method of estimating definite integrals by
;;; means of Monte Carlo simulation. Consider computing the area of a region of
;;; space described by a predicate `P(x, y)` that is true for points `(x, y)`
;;; in the region and false for points not in the region. For example, the
;;; region contained within a circle of radius 3 centered at (5, 7) is
;;; described by the predicate that tests whether `(x - 5)^2 + (y - 7)^2 <=
;;; 3^2`. To estimate the area of the region described by such a predicate,
;;; begin by choosing a rectangle that the rectangle that lies in the
;;; region. We can estimate the integral by picking, at random, points `(x, y)`
;;; that lie in the rectangle, and testing `P(x, y)` for each point to
;;; determine whether the point lies in the region. If we try this with many
;;; points, then the fraction of points that fall in the region should give an
;;; estimate of the proportion of the rectangle that lies in the region. Hence,
;;; multiplying this fraction by the area of the entire rectangle should
;;; produce an estimate of the integral.
;;; Implement Monte Carlo integration as a procedure `estimate-integral that
;;; takes as arguments a predicate `P`, upper and lower bounds `x1`, `x2`, `y1`
;;; and `y2` for the rectangle, and the number of trials to perform in order to
;;; produce the estimate. Your procedure should use the same `monte-carlo`
;;; procedure that was used above to estimate `Pi`. Use your
;;; `estimate-integral` to produce an estimate of `Pi` by measuring the area of
;;; a unit circle.
;;; You will find it useful to have a procedure that returns a number chosen at
;;; random from a given range. The following `random-in-range` procedure
;;; implements this in terms of the `random` procedure used in section 1.2.6,
;;; which returns a nonnegative number less than its input.
;;; (define (random-in-range low high)
;;;   (let ((range (- high low)))
;;;     (+ low (random range))))

(defun monte-carlo (trials experment)
  (labels ((iter (trials-remaining trials-passed)
             (cond ((= trials-remaining 0) (/ trials-passed trials))
                   ((funcall experment) (iter (- trials-remaining 1)
                                              (+ trials-passed 1)))
                   (t (iter (- trials-remaining 1) trials-passed)))))
    (iter trials 0)))
(defun random-in-range (low high)
  (let ((range (- high low)))
    (+ low (random range))))
(defun make-circle (center-x center-y radius)
  (list 'circle center-x center-y radius))
(defun circle-center-x (circle)
  (nth 1 circle))
(defun circle-center-y (circle)
  (nth 2 circle))
(defun circle-radius (circle)
  (nth 3 circle))
(defun in-circle-? (circle point-x point-y)
  (<= (+ (expt (- point-x (circle-center-x circle)) 2)
         (expt (- point-y (circle-center-y circle)) 2))
      (expt (circle-radius circle) 2)))
(defun estimate-integral (x1 y1 x2 y2 trials)
  (let ((center-x (/ (+ x1 x2) 2))
        (center-y (/ (+ y1 y2) 2))
        (radius   (/ (abs (- x2 x1)) 2))
        (rectangle-square (* (abs (- x2 x1)) (abs (- y2 y1)))))
    (let ((p (lambda ()
               (let ((random-point-x (random-in-range x1 x2))
                     (random-point-y (random-in-range y1 y2))
                     (circle (make-circle center-x center-y radius)))
                 (in-circle-? circle random-point-x random-point-y)))))
      (let ((p-result (monte-carlo trials p)))
        (let ((circle-square (* p-result rectangle-square)))
          (float (/ circle-square (expt radius 2))))))))

;;; Always floats at 3-.
