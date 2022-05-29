;;;; 3-21
;;;; 3-3-2
;;;; 2022/05/26

;;; Ben Bitdiddle decides to test the queue implementation described above. He
;;; types in the procedures to the lisp interpreter and proceeds to try them
;;; out:
;;; (define q1 (make-queue))
;;; (insert-queue! q1 'a)
;;; >> ((a) a)
;;; (insert-queue! q1 'b)
;;; >> ((a b) b)
;;; (delete-queue! q1)
;;; >> ((b) b)
;;; (delete-queue! q1)
;;; >> (() b)
;;; "It's all wrong!" he complains. "The interpreter's response shows that the
;;; last item is inserted into the queue twice. And when I delete both items,
;;; the second `b` is still there, so the queue isn't empty, even though it's
;;; supposed to be." Eva Lu Ator suggests that Ben has misunderstood what is
;;; happening. "It's not that the items are going into the queue twice," she
;;; explains. "It's just that the standard Lisp printer doesn't know how to make
;;; sense of the queue representation. If you want to see the queue printed
;;; correctly, you'll have to define your own print procedure for queues."
;;; Explain what Eva Lu is talking about. In particular, show why Ben's examples
;;; produce the printed results that they do. Define a procedure `print-queue`
;;; that takes a queue as input and prints the sequence of items in the queue.

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
      (error (format nil "DELETE! called with an empty queue ~A" queue))
      (progn (set-front-ptr! queue (cdr (front-ptr queue))) queue)))

(defun print-queue (queue)
  "What is cycle?"
  (format t "QUEUE: ")
  (map 'list
       (lambda (param) (format t "~A " param))
       (front-ptr queue))
  (fresh-line))

(defun test ()
  (let ((queue (make-queue)))
    (insert-queue! queue 1)
    (insert-queue! queue 2)
    (insert-queue! queue 3)
    (insert-queue! queue 4)
    (print-queue queue)
    (delete-queue! queue)
    (delete-queue! queue)
    (delete-queue! queue)
    (delete-queue! queue)
    (print-queue queue)))
