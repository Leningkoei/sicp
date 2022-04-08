;;;; 2-37
;;;; 2-2-3
;;;; 2022/04/08

(load "print.scm")
(define (reload)
  (load "2-37.scm"))

;;; Suppose we represent vectors `v = (v_i)` as sequences of numbers, and
;;; matrices `m = (m_ij)` as sequences of vectors (the rows of the matrix). For
;;; example, the matrix
;;; +-       -+
;;; | 1 2 3 4 |
;;; | 4 5 6 6 |
;;; | 6 7 8 9 |
;;; +-       -+
;;; is represented as the sequence `((1 2 3 4) (4 5 6 6) (6 7 8 9))`. Which this
;;; representation, we can use sequence operations to concisely express the
;;; basic matrix and vector options. This operations (which are described in any
;;; book on matrix algebra) are the following:
;;; (dot-product     v w): returns the sum Sigma_i v_i u_i;
;;; (matrix-*-vector m v): returns the vector t, where ti = Sigma_j m_ij v_j;
;;; (matrix-*matrix  m n): returns the matrix p, where p_ij = Sigma_k m_ik n_kj;
;;; (transpose         m): returns the matrix n, where n_ij = m_ji;
;;; We can define the dot product as
;;; (define (dot-product v w)
;;;   (accumulate + 0 (map * v m)))
;;; Fill in the missing expressions in the following procedures for computing
;;; the other matrix operations. (The procedure `accumulate-n` is defined in
;;; exercise 2.36.)
;;; (define (matrix-*-vector m v)
;;;   (map <??> m))
;;; (define (transpose mat)
;;;   (accumulate-n <??> <??> mat))
;;; (define (matrix-*-matrix m n)
;;;   (let ((cols (transpose n)))
;;;     (map <??> m)))

(load "accumulate.scm")

(define dot-product:Number (lambda (vector-a:Vector vector-b:Vector)
  (accumulate + 0 (map * vector-a:Vector vector-b:Vector))))

;;; My math is too much worse. And I never see the `dot-procedure:Number`. And I
;;; don't known `map` has rest parameter.
(define my-matrix-*-vector:Vector (lambda (matrix:Matrix vector:Vector)
  (define matrix.width  (length (car matrix:Matrix)))
  (define vector.length (length vector:Vector))
  (if (not (= matrix.width vector.length))
    (error "The matrix and the vector is not comfortable.")
    ((lambda ()
      (define double-pointer:Number
        (lambda (sequence-a:Sequence sequence-b:Sequence)
          (if (null? sequence-a:Sequence)
            0
            ((lambda ()
              ; (print (car sequence-a:Sequence) (car sequence-b:Sequence))
              (+ (* (car sequence-a:Sequence)
                    (car sequence-b:Sequence))
                 (double-pointer:Number (cdr sequence-a:Sequence)
                                        (cdr sequence-b:Sequence))))))))
      (define map.procedure:Number (lambda (row:Vector)
        (double-pointer:Number row:Vector vector:Vector)))
      (map map.procedure:Number matrix:Matrix))))))
(define matrix-*-vector:Vector (lambda (matrix:Matrix vector:Vector)
  (define map.procedure:Number (lambda (row:Vector)
    (dot-product:Number row:Vector vector:Vector)))
  (map map.procedure:Number matrix:Matrix)))

(define accumulate-n (lambda (op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate   op init (map (lambda (seq) (car seq)) seqs))
          (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs))))))
(define Matrix.transpose:Matrix (lambda (this:Matrix)
  (accumulate-n cons '() this:Matrix)))

(define matrix-*-matrix:Matrix (lambda (matrix-a:Matrix matrix-b:Matrix)
  (define reversed-matrix-b:Matrix (Matrix.transpose:Matrix matrix-b:Matrix))
  (define map.procedure:Vector (lambda (matrix-a.row:Vector)
    (matrix-*-vector:Vector reversed-matrix-b:Matrix matrix-a.row:Vector)))
  (map map.procedure:Vector matrix-a:Matrix)))

(define (test)
  (define vector-a:Vector (list 0 1 2 3))
  (define vector-b:Vector (list 1 2 3 4))
  (define matrix:Matrix   (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
  (print (dot-product:Number vector-a:Vector vector-b:Vector))
  (print (my-matrix-*-vector:Vector matrix:Matrix vector-a:Vector))
  (print (matrix-*-vector:Vector matrix:Matrix vector-a:Vector))
  (print (Matrix.transpose:Matrix matrix:Matrix))
  (print (matrix-*-matrix:Matrix matrix:Matrix matrix:Matrix)))

