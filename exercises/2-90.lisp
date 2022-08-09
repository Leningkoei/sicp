;;;; 2-90
;;;; 2-5-3
;;;; 2022/08/09

;;; Suppose we want to have a polynomial system that is efficient for both
;;; sparse and dense polynomials. One way to do this is to allow both kinds of
;;; term-list representations in our system. The situation is analogous to the
;;; complex-number example of section 2.4, where we allowed both rectangular and
;;; polar representations. To do this we must distinguish different types of
;;; term lists and make the operations on term lists generic. Redesign the
;;; polynomial system to implement this generalization. This is a major effort,
;;; not a local change.

(defun attach-tag (type-tag contents)
  (cons type-tag contents))
(defun type-tag (datum)
  (car datum))
(defun content (datum)
  (cdr datum))

(defun sparse? (term-list)
  (equal (type-tag term-list) 'sparse))
(defun dense? (term-list)
  (equal (type-tag term-list) 'dense))

(defun the-empty-term-list ()
  '())
(defun first-term (term-list)
  (cond ((sparse? term-list) (first-term@sparse (content term-list)))
        ((dense?  term-list) (first-term@dense  (content term-list)))
        ('t (error (format '() "Unknown type -- FIRST-TERM ~A"
                           (type-tag term-list))))))
(defun rest-term (term-list)
  (cdr (content term-list)))
(defun adjoin-term (term term-list)
  (cond ((sparse? term-list) (adjoin-term@sparse (content term-list)))
        ((dense?  term-list) (adjoin-term@dense  (content term-list)))
        ('t (error (format '() "Unknown-type -- ADJOIN-TERM"
                           (type-tag term-list))))))

(defun make-term (order coefficient)
  `(,order ,coefficient))
(defun order (term)
  (car term))
(defun coefficient (term)
  (cadr term))

(defun adjoin-term@sparse (term term-list)
  (if (= zero? (coeff term))
      term-list
      (cons term term-list)))
(defun adjoin-term@dense (term term-list)
  (cons term term-list))
(defun first-term@sparse (term-list)
  (car term-list))
(defun first-term@dense (term-list)
  (make-term (- (length term-list) 1) ; order
             (car (term-list)))) ; coefficient

;;; A local change できた.
