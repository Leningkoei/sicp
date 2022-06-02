;;;; 3-30
;;;; 3-3-4
;;;; 2022/06/01

;;; Figure 3.27 shows a `ripple-carry adder` formed by stringing together `n`
;;; full-adders. This is the simplest form of parallel adder for adding two
;;; `n`-bit binary numbers. The inputs A_1, A_2, A_3, ..., A_n and B_1, B_2,
;;; B_3, ..., B_n are the two binary numbers to be added (each A_k and B_k is a
;;; 0 or a 1). The circuit generates S_1, S_2, S_3, ..., S_n, the `n` bits of
;;; the sum, and C, the carry from the addition. Write a procedure
;;; `ripple-carry-adder` that generates this circuit. The procedure should takes
;;; as arguments three lists of `n` wires each -- the A_k, the B_k, and the S_k
;;; -- and also another wire C. The major drawback of the ripple-carry adder is
;;; the need to wait for the carry signals to propagate. What is the delay
;;; needed to obtain the complete output from an `n`-bit ripple-carry adder,
;;; expressed in terms of the delays for and-gates, or-gates, and inverters?

(defun ripple-carry-adder (a b s output)
  "List<wire> -> List<wire> -> List<wire> -> wire -> 'ok"
  (cond ((not (= (length a) (length b)))
         (error (format nil
                        "Bit count of `a` and `b` is not equal -- ripple-carry-adder ~A ~A"
                        a b)))
        ((not (= (length b) (length s)))
         (error (format nil
                        "Bit count of `b` and `s` is not equal -- ripple-carry-adder ~A ~A"
                        b s))))
  (labels ((iterator (rest-a rest-b rest-s c-out)
             (let ((rest-s (cdr rest-s))
                   (a (car rest-a))
                   (b (car rest-b))
                   (s (car rest-s))
                   (c-in (make-wire)))
               (if rest-s
                   (progn (full-adder a b c-in s c-out)
                          (iterator (cdr a) (cdr b) s c-in))
                   (progn (set-signal! c-in 0)
                          (full-adder a b c-in s c-out))))))
    (iterator a b s output)
    'ok))
