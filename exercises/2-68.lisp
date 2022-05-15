;;;; 2-68
;;;; 2-3-4
;;;; 2022/05/15

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))
(defun leaf? (object)
  (equal (car object) 'leaf))
(defun symbol-leaf (x)
  (cadr x))
(defun weight-leaf (x)
  (caddr x))
(defun left-branch (tree)
  (car tree))
(defun right-branch (tree)
  (cadr tree))
(defun symbols (tree)
  (if (leaf? tree) (list (symbol-leaf tree))
    (caddr tree)))
(defun weight (tree)
  (if (leaf? tree) (weight-leaf tree)
    (cadddr tree)))
(defun make-code-tree (left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;;; The answer
(defun encode-symbol (target tree)
  (labels
   ((iterator
     (result tree)
     (if (leaf? tree) (handle-leaf result tree) (handle-tree result tree)))
    (handle-tree
     (result tree)
     (let ((left-result (iterator (append result (list 0)) (left-branch tree))))
       (if (not (null left-result)) left-result
         (iterator (append result (list 1)) (right-branch tree)))))
    (handle-leaf
     (result leaf)
     (let ((symbol (symbol-leaf leaf)))
       (if (equal target symbol) result nil))))
   (let ((code (iterator nil tree)))
     (if (not (null code)) code
       (error "Undefined Symbol -- encode-symbol")))))

;;; The `encode` procedure takes as arguments a message and a tree and produces
;;; the list of bits that gives the encoded message.
(defun encode (message tree)
  (if (null message) nil
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))
;;; `encode-symbol` is a procedure, which you must write, that returns the list
;;; of bits that encodes a given symbol according to a given tree. You should
;;; design `encode-symbol` so that it signals an error if the symbol is not in
;;; the tree at all. Test your procedure by encoding the result you obtained in
;;; exercise 2.67 with the sample tree and seeing whether it is the same as the
;;; original sample message.

(defun test ()
  (let ((sample-tree
         (make-code-tree (make-leaf 'A 4)
                         (make-code-tree (make-leaf 'B 2)
                                         (make-code-tree (make-leaf 'C 1)
                                                         (make-leaf 'D 1)))))
        (sample-message (list 'A 'B 'C 'D)))
    (print (encode sample-message sample-tree)))
  nil)
