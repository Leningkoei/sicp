;;;; 3-22
;;;; 3-3-2
;;;; 2022/05/29

;;; Instead of representing a queue as a pair of pointers, we can build a queue
;;; as a procedure with local state. The local state will consist of pointers to
;;; the beginning and the end of an ordinary list. Thus, the `make-queue`
;;; procedure will have the form
;;; (define (make-queue)
;;;   (let ((front-ptr ...)
;;;         (rear-ptr ...))
;;;     <definitions of internal procedures>
;;;     (define (dispatch m) ...)
;;;     dispatch))
;;; Complete the definition of `make-queue` and provide implementations of the
;;; queue operations using this representation.

(defun make-queue ()
  (let ((front-ptr nil)
        (rear-ptr nil))
    (flet ((empty? ()
             (null front-ptr))
           (print* ()
             (format t "QUEUE: ")
             (map 'list (lambda (param) (format t "~A " param)) front-ptr)
             (fresh-line)))
      (let ((insert (lambda (item)
                      (let ((new-pair (cons item nil)))
                        (if (empty?)
                            (progn (setf front-ptr new-pair)
                                   (setf rear-ptr new-pair)
                                   t)
                            (progn (rplacd rear-ptr new-pair)
                                   (setf rear-ptr new-pair)
                                   t)))))
            (delete (lambda ()
                      (if (empty?)
                          (error "DELETE! called with an empty queue")
                          (progn (setf front-ptr (cdr front-ptr)))))))
        (let ((dispatch
                (lambda (method)
                  (cond ((equal method 'insert) insert)
                        ((equal method 'delete) (funcall delete))
                        ((equal method 'empty?) (empty?))
                        ((equal method 'print) (print*))))))
          dispatch)))))

(defun test ()
  (let ((queue (make-queue)))
    (funcall (funcall queue 'insert) 1)
    (funcall (funcall queue 'insert) 2)
    (funcall (funcall queue 'insert) 3)
    (funcall (funcall queue 'insert) 4)
    (funcall queue 'print)
    (funcall queue 'delete)
    (funcall queue 'delete)
    (funcall queue 'delete)
    (funcall queue 'delete)
    (funcall queue 'print)))
