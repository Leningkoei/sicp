;;;; 3-17
;;;; 3-3-1
;;;; 2022/05/24

;;; Devise a correct version of the `count-pairs` procedure of exercise 3.16
;;; that returns the number of distinct pairs in any structure. (Hint: Traverse
;;; the structure, maintaining an auxiliary data structure that is used to keep
;;; track of which pairs have already been counted.)

(defun pair? (x)
  (and x (listp x)))

;; (defun count-pairs (x)
;;   (let ((set nil))
;;     (labels
;;         ((iterator (current)
;;            (flet ((in-set? ()
;;                     (labels
;;                         ((iterator (rest-set)
;;                            (if (null rest-set)
;;                                nil
;;                                (or (eq current (car rest-set))
;;                                    (iterator (cdr rest-set))))))
;;                       (iterator set))))
;;              (if (or (not (pair? current)) (in-set?))
;;                  0
;;                  (progn (setf set (cons current set))
;;                         (+ (iterator (car current))
;;                            (iterator (cdr current))
;;                            1))))))
;;       (iterator x))))

(defun count-pairs (x)
  (labels
      ((collect-pairs (current set)
         (if (or (not (pair? current)) (member current set :test 'eq))
             set
             (let ((car-set (collect-pairs (car current) (cons current set))))
               (collect-pairs (cdr current) car-set)))))
    (length (collect-pairs x nil))))

(defun test ()
  (let ((x (list 'a)))
    (let ((z4 (cons (cons 'b x) x))
          (zn (rplacd x x)))
      (format t "~A" (count-pairs z4)) (fresh-line)
      (format t "~A" (count-pairs zn)) (fresh-line))))
