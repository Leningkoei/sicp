;;;; 2-4-3
;;;; 2022/05/18

(defun square (x)
  (expt x 2))

;;                                 Types
;;                  rectangular      |      Polar
;;           +-----------------------+----------------
;; real-part | real-part-rectangular | real-part-polar
;; imag-part | imag-part-rectangular | imag-part-polar
;; magnitude | magnitude-rectangular | magnitude-polar
;;   angle   |   angle-rectangular   |   angle-polar

;; * (put <op> <type> <item>)
;;   installs the <item> in the table, indexed by the <op> and the <type>.
;; * (get <op> <type>)
;;   looks up the <op>, <type> entry in the table and returns the item found
;;   there. If on item is found, `get` returns false.

(defun attach-tag (type-tag contents)
  (cons type-tag contents))

;; (defun install-rectangular-package ()
;;   (labels
;;       ;; internal procedures
;;       ((real-part (z)
;;          (car z))
;;        (imag-part (z)
;;          (cdr z))
;;        (magnitude (z)
;;          (sqrt (+ (square (real-part z) (square (imag-part z))))))
;;        (angle (z)
;;          (atan (imag-part z) (real-part z)))
;;        (make-from-real-imag (real-part imag-part)
;;          (cons real-part imag-part))
;;        (make-from-mag-ang (magnitude angle)
;;          (let ((real-part (* magnitude (cos angle)))
;;                (imag-part (* magnitude (sin angle))))
;;            (make-from-real-imag real-part imag-part)))
;;        ;; interface to the rest of system
;;        (tag (x)
;;          (attach-tag 'rectangular x))
