;;;; 3-3-2: Representing Queues
;;;; 2022/05/26

;;; In terms of data abstraction, we can regard a queue as defined by the
;;; following set of operations:
;;; * a constructor:
;;;   (make-queue)
;;;   >> returns an empty queue (a queue containing no items).
;;; * two selectors:
;;;   (empty-queue? <queue>)
;;;   >> tests if the queue is empty.
;;;   (front-queue <queue>)
;;;   >> returns the object at the front of the queue, signaling an error if the
;;;      queue is empty; it does not modify the queue.
;;; * two mutators:
;;;   (insert-queue! <queue> <item>)
;;;   >> inserts the item at the rear of the queue and returns the modified
;;;      queue as its value.
;;;   (delete-queue! <queue>)
;;;   >> removes the item at the front of the queue and returns the modified
;;;      queue as its value, signaling an error if the queue is empty before the
;;;      deletion.

(defun front-ptr (queue)
  (car queue))
(defun rear-ptr (queue)
  (cdr queue))
(defun set-front-ptr! (queue item)
  (rplaca queue item))
(defun set-rear-ptr! (queue item)
  (rplacd queue item))
(defun make-queue ()
  (cons nil nil))
(defun empty-queue? (queue)
  (null (front-ptr queue)))
(defun front-queue (queue)
  (if (empty-queue? queue)
      (error (format nil "Front called with an empty queue ~A" queue))
      (car (front-ptr queue))))
(defun insert-queue! (queue item)
  (let ((new-pair (cons item nil)))
    (if (empty-queue? queue)
        (progn (set-front-ptr! queue new-pair)
               (set-rear-ptr! queue new-pair)
               queue)
        (progn (rplacd (rear-ptr queue) new-pair)
               (set-rear-ptr! queue new-pair)
               queue))))
(defun delete-queue! (queue)
  (if (empty-queue? queue)
      (error (format nil "Delete! called with an empty queue ~A" queue))
      (progn (set-front-ptr! queue (cdr (front-ptr queue)))
             queue)))
