;;;; 2-75
;;;; 2-4-3
;;;; 2022/07/10

;;; Implement the constructor `make-from-mag-ang` in message-passing style. This
;;; procedure should be analogous to the `make-from-real-imag` procedure given
;;; above.

(defparameter make-from-mag-ang
  (labels ((make-from-mag-ang (r a)
             (labels ((dispatch (operation)
                        (cond ((equal operation 'real-part)
                               (* (cos a) r))
                              ((equal operation 'imag-part)
                               (* (sin a) r))
                              ((equal operation 'magnitude) r)
                              ((equal operation 'angle) a)
                              ('t (error (format '() "~A -- ~A ~A"
                                                 "Unknown operation"
                                                 "MAKE-FROM-MAG-ANG"
                                                 operation))))))
                      #'dispatch)))
    #'make-from-mag-ang))
