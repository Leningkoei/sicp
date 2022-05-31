;;;; 3-27
;;;; 3-3-3
;;;; 2022/05/31

;;; `memoization` (also called `tabulation`) is a technique that enables a
;;; procedure to record, in a local table, values that have previously been
;;; computed. This technique can make a vast difference in the performance of a
;;; program. A memoized procedure maintains a table in which values of previous
;;; calls are stored using as keys the arguments that produced the values. When
;;; the memoized procedure is asked to compute a value, it first checks the
;;; table to see if the value is already there and, if so, just returns that
;;; value. Otherwise, it computes the new value in the ordinary way and stores
;;; this in the table. As an example of memoization, recall from section 1.2.2
;;; the exponential process for computing Fibonacci numbers:

;;; (define fib (n)
;;;   (cond ((= n 0) 0)
;;;         ((= n 1) 1)
;;;         (t (+ (fib (- n 1)) (fib (- n 2))))))

;;; The memoized version of the same procedure is

;;; (define memo-fib
;;;   (memoize (lambda (n)
;;;              (cond ((= n 0) 0)
;;;                    ((= n 1) 1)
;;;                    (t (+ (memo-fib (- n 1)) (memo-fib (- n 2))))))))

;;; where the memoizer  is defined as

;;; (define memoize (f)
;;;   (let ((table (make-table)))
;;;     (lambda (x)
;;;       (let ((previously-computed-result (lookup x table)))
;;;         (or previously-computed-result
;;;             (let ((result (f x)))
;;;               (insert! x result table)
;;;               result))))))

;;; Draw an environment diagram to analyze the computation of `(memo-fib
;;; 3)`. Explain why `memo-fib` computes the `n`th Fibonacci number in `a`
;;; number of steps proportional to `n`. Would the scheme still work if we had
;;; simply defined `memo-fib` to be `(memoize fib)`?

;;; The table will live with `memo-fib` forever?

(defun table-lookup (key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        record)))
(defun table-insert! (key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (rplacd record value)
        (rplacd table (cons (cons key value) (cdr table))))
    'ok))
(defun make-table ()
  (list '*table*))

(defun memoize (f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (table-lookup x table)))
        (or previously-computed-result
            (let ((result (funcall f x)))
              (table-insert! x result table)
              result))))))

(let ((memo-fib (memoize (lambda (n)
                           (cond ((= n 0) 0)
                                 ((= n 1) 1)
                                 (t (+ (funcall 'fib (- n 1))
                                       (funcall 'fib (- n 2)))))))))
  (defun fib (n) (funcall memo-fib n)))
