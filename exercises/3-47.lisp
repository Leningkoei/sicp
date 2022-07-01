;;;; 3-47
;;;; 3-4-2
;;;; 2022/07/01

;;; A semaphore (of size `n`) is a generalization of a mutex. Like a mutex, a
;;; semaphore supports acquire and release operations, but it is more general in
;;; that up to `n` processes can acquire it concurrently. Additional processes
;;; that attempt to acquire the semaphore must wait for release operations. Give
;;; implementations of semaphores
;;; a. in terms of mutexes
;;; b. in terms of atomic `test-and-set!` operations.

(defun make-semaphore (n)
  (let ((mutex (make-mutex))
        (count 0))
    (labels ((acquire ()
               (if (< count n)
                   (progn (if (= count (- n 1)) (funcall mutex 'acquire))
                          (setf c (+ c 1)))
                   (acquire)))
             (release ()
               (if (< 0 count)
                   (progn (setf count (- count 1))
                          (if (= count (- n 1) (funcall mutex 'release))))
                   (error (format '() "There are not enough rest processes are
  using this cell."))))
             (dispatch (m)
               (cond ((equal m 'acquire)
                      (acquire))
                     ((equal m 'release)
                      (release))
                     ('t (error (format '()
                                        "Unknown request -- MAKE-SEMAPHORE ~A"
                                        m))))))
      #'dispatch)))

(defun make-semaphore (n)
  (let ((count 0))
    (labels ((decrease! ()
               (setf count (- count 1)))
             (increase! ()
               (setf count (+ count 1)))
             (test-and-decrease! ()
               (if (< 0 count)
                   (decrease!)
                   (error (format '() "There are not enough rest processes are
  using this cell."))))
             (test-and-set! ()
               (if (< count n)
                   (progn (increase) '())
                   't))
             (the-semaphore (m)
               (cond ((equal m 'acquire)
                      (if (test-and-set!)
                          (the-semaphore 'acquire)))
                     ((equal m 'release)
                      (test-and-decrease!)))))
      #'the-semaphore)))
