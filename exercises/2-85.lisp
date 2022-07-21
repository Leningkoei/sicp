;;;; 2-85
;;;; 2-5-2
;;;; 2022/07/21

;;; This section mentioned a method for "simplifying" a data object by lowering
;;; it in the tower of types as far as possible. Design a procedure `drop` that
;;; accomplishes this for the tower described in exercise 2.83. The key is to
;;; decide, in some general way, whether an object can be lowered. For example,
;;; the complex number `1.5 + 0i` can be lowered as far as `real`, the complex
;;; number `1 + 0i` can be lowered as far as `integer`, and the complex number
;;; `2 + 3i` cannot be lowered at all. Here is a plan for determining whether an
;;; object can be lowered: Begin by defining a generic operation `project` that
;;; "pushes" an object down in the tower. For example, projecting a complex
;;; number would involve throwing away the imaginary part. Then a number can be
;;; dropped if, when we `project` it and `raise` the result back to the type we
;;; started with, we end up with something equal to what we started with. Show
;;; how to implement this idea in detail, by writing a `drop` procedure that
;;; drops an object as far as possible. You will need to design the various
;;; projection operations and install `project` as a generic operation in the
;;; system. You will also need to make use of a generic equality predicate, such
;;; as described in exercise 2.79. finally, use `drop` to rewrite
;;; `apply-generic` from exercise 2.84 so that it "simplifies" its answers.

(defun project (number)
  (apply-generic 'project number))

(put 'project '(complex)
     (lambda (z)
       (make-real (imag-part z))))
(put 'project '(real)
     (lambda (r)
       (make-rational (round r) 1)))
(put 'project '(rational)
     (lambda (q)
       (make-integer (round (/ (number q) (denom q))))))

(defun drop (number)
  (let ((proc (get 'project `(,(type-tag number)))))
    (if proc
        (drop (apply proc number))
        number)))

(defun apply-generic (op &rest args)
  (drop (let ((type-tags (map 'list #'type-tag args)))
          (let ((target-type (get-highest-level type-tags)))
            (let ((fixed-args (map 'list
                                   (lambda (current-arg)
                                     (raise-to current-arg target-type))
                                   args))
                  (fixed-type-tags (map 'list
                                        (lambda (current-type-tag)
                                          (declare (ignore current-type-tag))
                                          target-type)
                                        type-tags)))
              (let ((proc (get op fixed-type-tags)))
                (if proc
                    (apply proc (map 'list #'content fixed-args))
                    (error (format '() "No method for these types ~A"
                                   `(,op ,fixed-type-tags))))))))))
