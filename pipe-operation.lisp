;;; recursive

;; compiling time
;; (defmacro <- (args &rest funcs)
;;   (if funcs
;;       `(apply ,(car funcs) (<- ,args ,@(cdr funcs)))
;;       args))
;; compiling time
;; (defmacro -> (args &rest funcs)
;;   `(<- ,args ,@(reverse funcs)))

;;; iterator

;; running time
;; (defun -> (args &rest funcs)
;;   (reduce #'(lambda (pre-result func)
;;               (apply func pre-result))
;;           `(,args . ,funcs)))
;; compiling time
(defmacro -> (args &rest funcs)
  (reduce #'(lambda (pre-result func)
              `(apply ,func ,pre-result))
          `(,args . ,funcs)))
