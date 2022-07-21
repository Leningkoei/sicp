;;;; 2-82
;;;; 2-5-2
;;;; 2022/07/21

;;; Show how to generalize `apply-generic` to handle coercion in the general
;;; case of multiple arguments. One strategy is to attempt to coerce all the
;;; arguments to the type of the first argument, then to the type of the second
;;; argument, and so on. Give an example of a situation where this strategy (and
;;; likewise the two-argument version given above) is not sufficiently
;;; general. (Hint: Consider the case where there are some suitable mixed-type
;;; operations present in the table that will not be tried.)

;; Why not let our user to decide the time of using coercion?

(defun coerce-all (target-type args)
  (let ((coerced-args
          (map 'list
               (lambda (current-arg)
                 (let ((type-tag (type-tag current-arg)))
                   (let ((current-type->target-type
                           (get-coercion target-type type-tag)))
                     (if coercion-procedure
                         (current-type->target-type current-arg)
                         '())))))))
    (if (find '() coerced-args)
        '()
        coerced-args)))

(defun holy-shit (op args)
  (labels ((iterator (rest-args)
             (if rest-args
                 (let ((current-target-type (type-tag (car rest-args))))
                   (let ((coerced-args (coerce-all current-target-type args)))
                     (if coerced-args
                         (let ((proc (get op type-tags)))
                           (if proc
                               (apply proc (map 'list content args))
                               (iterator (cdr rest-args))))
                         (iterator (cdr rest-args)))))
                 'holy-shit-not-find)))
    (iterator op args)))

(defun apply-generic (op &rest args)
  (let ((type-tags (map 'list type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map 'list content args))
          (let ((holy-shit (holy-shit op args)))
            (if (equal holy-shit 'holy-shit-not-find)
                (error (format '() "" `(,op ,type-tags)))
                holy-shit))))))
