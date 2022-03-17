;;;; 1-2-6 Test for Promality
;;;; 2022/03/17

(defun square (x)
  (* x x))
(defun expmod (base exp-num m)
  (cond ((= exp-num 0) 1)
        ((evenp exp-num) (mod (square (expmod base (/ exp-num 2) m))
                                    m))
        (t (mod (* base (expmod base (- exp-num 1) m))
                      m))))
(defun try-it (a n)
  (= a (expmod a n n)))
(defun Fermat-test (n)
  (try-it (+ 1 (random (- n 1))) n))

(defun my-random ()
  (let ((random-num (+ 100000000 (random 100000000))))
    (if (Fermat-test random-num)
      ; (my-random)
      ; random-num)))
      random-num
      (my-random))))
(defun bit-coin ()
  (do ((random-num (my-random) (my-random))
       (my-count 0 (+ 1 my-count)))
      ; ((Fermat-test random-num) (list my-count random-num))
      ((not (Fermat-test random-num)) (list my-count random-num))
      (print (list my-count random-num))))

