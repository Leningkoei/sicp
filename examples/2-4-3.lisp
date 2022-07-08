;;;; 2-4-3
;;;; 2022/05/18 -- 2022/07/08

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

(defun install-rectangular-package ()
  (labels
      ;; internal procedures
      ((real-part (z)
         (car z))
       (imag-part (z)
         (cdr z))
       (magnitude (z)
         (sqrt (+ (square (real-part z) (square (imag-part z))))))
       (angle (z)
         (atan (imag-part z) (real-part z)))
       (make-from-real-imag (real-part imag-part)
         (cons real-part imag-part))
       (make-from-mag-ang (magnitude angle)
         (let ((real-part (* magnitude (cos angle)))
               (imag-part (* magnitude (sin angle))))
           (make-from-real-imag real-part imag-part)))
       ;; interface to the rest of system
       (tag (x) (attach-tag 'rectangular x)))
    (put 'real-part '(rectangular) #'real-part)
    (put 'imag-part '(rectangular) #'imag-part)
    (put 'magnitude '(rectangular) #'magnitude)
    (put 'angle     '(rectangular) #'angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang   'rectangular
         (lambda (r a) (tag (make-from-mag-ang   r a))))
    'done))

(defun install-polar-package ()
  (labels
      ;; internal procedures
      ((real-part (z)
         (* (magnitude z) (cos (angle z))))
       (imag-part (z)
         (* (magnitude z) (sin (angle z))))
       (magnitude (z)
         (car z))
       (angle (z)
         (cdr z))
       (tag (x) (attach-tag 'polar x)))
    (put 'real-part '(polar) #'real-part)
    (put 'imag-part '(polar) #'imag-part)
    (put 'magnitude '(polar) #'magnitude)
    (put 'angle     '(polar) #'angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang   'polar
         (lambda (r a) (tag (make-from-mag-ang   r a))))
    'done))

(defun type-tag (datum)
  (if (pair? datum)
      (car datum)
      (error (format '() "Bad tagged datum -- TYPE-TAG ~A" datum))))
(defun contents (datum)
  (if (pair? datum)
      (cdr datum)
      (error (format '() "Bad tagged datum -- CONTENTS ~A" datum))))

(defun apply-generic (op &rest args)
  (let ((type-tags (map 'list #'type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map 'list #'contents args))
          (error (format '() "No method for these types -- APPLY-GENERIC ~A"
                         `(,op ,type-tags)))))))

(defun real-part (z) (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle     (z) (apply-generic 'angle     z))

;; type of z: (type-tag (data-a data-b))

(defun make-from-real-imag (x y)
  (funcall (get 'make-from-real-imag 'rectangular) x y))
(defun make-from-reg-ang   (r a)
  (funcall (get 'make-from-reg-ang 'polar) r a))
