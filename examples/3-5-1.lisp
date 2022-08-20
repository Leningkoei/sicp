;;;; 3-5-1: Streams Are Delayed Lists
;;;; 2022/08/22

(defun sum-primes (a b)
  (labels ((iterator (count accumulate)
             (cond ((> count b) accumulate)
                   ((prime? count) (iterator (+ count 1) (+ count accumulate)))
                   ('t (iterator (+ count 1) accumulate)))))
    (iterator a 0)))
