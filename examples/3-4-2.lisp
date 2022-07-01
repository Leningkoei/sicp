;;;; 3-4-2
;;;; 2022/07/01

;;; Implementing serializers

(defun make-serializer ()
  (let ((mutex (make-mutex)))
    (lambda (p)
      (labels ((seralized-p (&rest args)
                 (mutex 'acquire)
                 (let ((val (apply p args)))
                   (mutex 'release)
                   val)))
        #'seralized-p))))

(defun make-mutex ()
  (let ((cell '(())))
    (labels ((clear! (cell)
               (replaca cell '()))
             (test-and-set! (cell)
               "Tests the cell and returns the result of the test. In addition,
  if the test was false, `test-and-set!` sets the cell contents to true before
  returning false."
               (if (car cell)
                   't
                   (progn (replaca cell 't) '())))
             (the-mutex (m)
               (cond ((equal m 'acquire)
                      (if (test-and-set! cell)
                          ;; retry
                          (the-mutex 'acquire)))
                     ((equal m 'release)
                      (clear! cell)))))
      #'the-mutex)))
