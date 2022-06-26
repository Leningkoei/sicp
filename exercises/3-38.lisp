;;;; 3-38
;;;; 3-3-4
;;;; 2022/06/26

;;; Suppose that Peter, Paul, and Mary share a joint bank account that initially
;;; contains $100. Concurrently, Peter deposits $10, Paul withdraws $20, and
;;; Mary withdraws half the money in the account, by executing the following
;;; commands:
;;; Peter: (set! balance (+ balance 10))
;;; Paul:  (set! balance (- balance 20))
;;; Mary:  (set! balance (- balance (/ balance 2)))

;;; a. List all the different possible values for `balance` after these three
;;; transactions have been completed, assuming that the banking system forces
;;; the three processes to run sequentially in some order.

;;; Pe -> Pa -> M: 100 -> 110 -> 90 -> 45
;;; Pe -> M -> Pa: 100 -> 110 -> 55 -> 35
;;; Pa -> Pe -> M: 100 ->  80 -> 90 -> 45
;;; Pa -> M -> Pe: 100 ->  80 -> 40 -> 50
;;; M -> Pe -> Pa: 100 ->  50 -> 60 -> 40
;;; M -> Pa -> Pe: 100 ->  50 -> 30 -> 40

;;; b. What are some other values that could be produced if the system allows
;;; the processes to be interleaved? Draw timing diagrams like the one in figure
;;; 3.29 to explain how these values can occur.

;;; It may can be solved by a tree:
;;; Level
;;; 0 --------> o
;;;            /|\
;;;           / | \
;;;          /  |  \
;;;         /   |   \
;;;        /    |    \
;;; 1 -> Pe1   Pa1   M 1
;;; ...
;;; 9 (Max)
;;; Be careful the order of the step.

(defun deep-search ()
  ;; height = 3, width = 3
  (let ((list `(,3 ,3 ,3)))
    (labels ((iterator (rest)
               (let ((pe (car rest))
                     (pa (cadr rest))
                     (m (caddr rest)))
                 (if (and (= pe 0) (= pa 0) (= m 0))
                     1
                     (+ (if (= pe 0) 0 (iterator `(,(- pe 1) ,pa ,m)))
                        (if (= pa 0) 0 (iterator `(,pe ,(- pa 1) ,m)))
                        (if (= m  0) 0 (iterator `(,pe ,pa ,(- m 1)))))))))
      (iterator list))))
;;; >>> 1680
