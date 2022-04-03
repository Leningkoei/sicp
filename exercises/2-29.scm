;;;; 2-29
;;;; 2-2-2
;;;; 2022/04/03

(define reload (lambda ()
  (load "2-29.scm")))
; (define print (lambda (. contents)
(define (print . contents)
  (define iterator.handle-fail (lambda (rest-contents)
    (display (car rest-contents))
    (display " ")
    (iterator (cdr rest-contents))))
  (define iterator (lambda (rest-contents)
    (if (null? rest-contents)
      (newline)
      (iterator.handle-fail rest-contents))))
  (iterator contents))

;;; A binary mobile consists of two branches, a left branch and a right branch.
;;; Each branch is a rod of a certain length, from which hangs either a weight
;;; or another binary mobile. We can represent a binary mobile using compound
;;; data by constructing it from two branches (for example, using `list`):

(define make-mobile (lambda (left right)
  (list left right)))

;;; A branch is constructed from a `length` (which must be a number) together
;;; with a `structure`, which may be either a number (representing a simple
;;; weight) or another mobile:

(define make-branch (lambda (length structure)
  (list length structure)))

;;; a. Write the corresponding selectors `left-branch` and `right-branch`, which
;;; return the branches of a mobile, and `branch-length` and `branch-structure`,
;;; which return the components of a branch.

(define left-branch (lambda (mobile)
  (car mobile)))
(define right-branch (lambda (mobile)
  (cadr mobile)))
(define branch-length (lambda (branch)
  (car branch)))
(define branch-structure (lambda (branch)
  (cadr branch)))

;;; b. Using your selectors, define a procedure `total-weight` that returns the
;;; total weight of a mobile.

(define total-weight.get-weight (lambda (structure)
  (if (pair? structure)
    (total-weight structure)
    structure)))
(define total-weight (lambda (mobile)
  (define this.left:Branch  (left-branch mobile))
  (define this.right:Branch (right-branch mobile))
  (define this.left.structure:Branch/Number
    (branch-structure this.left:Branch))
  (define this.right.structure:Branch/Number
    (branch-structure this.right:Branch))
  (+ (total-weight.get-weight this.left.structure:Branch/Number)
     (total-weight.get-weight this.right.structure:Branch/Number))))

;;; c. A mobile is said to be `balanced` if the torque applied by its top-left
;;; branch (that is, if the length of the left rod multiplied by the weight
;;; hanging from that rod is equal to the corresponding product for the right
;;; side) and if each of the mobiles hanging off its branches is balanced.
;;; Design a predicate that tests whether a binary mobile is balanced.

(define Branch.torque:Number (lambda (this:Branch)
  (define this.length:Number (branch-length this:Branch))
  (define this.structure:Branch/Number (branch-structure this:Branch))
  (define this.weight:Number
    (total-weight.get-weight this.structure:Branch/Number))
  (* this.length:Number this.weight:Number)))
(define Mobile.equal? (lambda (this:Mobile)
  (define this.left-branch:Branch (left-branch this:Mobile))
  (define this.right-branch:Branch (right-branch this:Mobile))
  (define this.left-branch.torque:Number
    (Branch.torque:Number this.left-branch:Branch))
  (define this.right-branch.torque:Number
    (Branch.torque:Number this.right-branch:Branch))
  (= this.left-branch.torque:Number this.right-branch.torque:Number)))

;;; d. Suppose we change the representation of mobiles so that the constructors
;;; are

; (define make-mobile (lambda (left right)
;   (cons left right)))
; (define make-branch (lambda (length structure)
;   (cons length structure)))

;;; How much do you need to change your programs to convert to the new
;;; representation?

(define make-mobile-kai (lambda (left right)
  (cons left right)))
(define make-branch-kai (lambda (length structure)
  (cons length structure)))
(define Mobile.left:Branch (lambda (this:Mobile)
  (car this:Mobile)))
(define Mobile.right:Branch (lambda (this:Mobile)
  (cdr this:Mobile)))
(define Branch.length:Number (lambda (this:Branch)
  (car this:Branch)))
(define Branch.structure:Mobile/Number (lambda (this:Branch)
  (cdr this:Branch)))

;;; Other change is rename these functions.

(define test (lambda ()
  (define b.left-branch (make-branch 1 1))
  (define b.right-branch (make-branch 2 2))
  (define b:Mobile (make-mobile b.left-branch b.right-branch))
  (define a.left-branch (make-branch 3 3))
  (define a.right-branch (make-branch 3 b:Mobile))
  (define a:Mobile (make-mobile a.left-branch a.right-branch))
  (print "branch-length:" (branch-length (left-branch a:Mobile)))
  (print "branch-structure:" (branch-structure (right-branch a:Mobile)))
  (print "total-weight:" (total-weight a:Mobile))
  (print "Mobile.equal?(b):" (Mobile.equal? b:Mobile))
  (print "Mobile.equal?(c):" (Mobile.equal? a:Mobile))))

