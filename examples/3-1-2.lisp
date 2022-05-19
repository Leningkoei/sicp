;;;; 3-1-2: The Benefits of Introducing Assignment
;;;; 2022/05/19

(let ((x random-init))
  (defun rand ()
    (setf x (rand-update x))
    x))

(defun cesaro-test ()
  (= (gcd (rand) (rand)) 1))
(defun monte-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passed)
             (cond ((= trials-remaining 0) (/ trials-passed trials))
                   ((funcall experiment) (iter (- trials-remaining 1)
                                               (+ trials-passed 1)))
                   (t (iter (- trials-remaining 1) trials-passed)))))
    (iter trials 0)))
(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
