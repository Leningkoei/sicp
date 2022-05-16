;;;; 2-69
;;;; 2-3-4
;;;; 2022/05/16

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))
(defun leaf? (object)
  (equal 'leaf (car object)))
(defun symbol-leaf (x)
  (cadr x))
(defun weight-leaf (x)
  (caddr x))

(defun left-branch (tree)
  (car tree))
(defun right-branch (tree)
  (cadr tree))
(defun symbols (tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(defun weight (tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(defun make-code-tree (left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (t (cons (car set) (adjoin-set x (cdr set))))))
(defun make-leaf-set (pairs)
  (if (null pairs) nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;; answer
(defun successive-merge (pairs)
  (labels ((iterator (result rest-pairs)
             (if (null rest-pairs) result
                 (let ((new-result (make-code-tree (car rest-pairs) result))
                       (new-rest-pairs (cdr rest-pairs)))
                   (iterator new-result new-rest-pairs)))))
    (if (null pairs) nil
        (iterator (car pairs) (cdr pairs)))))

;;; The following procedure takes as its argument a list of symbol-frequency
;;; pairs (where no symbol appears in more than one pair) and generates a
;;; Huffman encoding tree according to the Huffman algorithm.
(defun generate-Huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))
;;; `make-leaf-set` is the procedure given above that transforms the list of
;;; pairs into an ordered set of leaves. `successive-merge` is the procedure
;;; you must write, using `make-code-tree` to successively merge the
;;; smallest-weight elements of the set until there is only one element left,
;;; which is the desired Huffman tree. (This procedure is slightly tricky, but
;;; not really complicated. If you find yourself designing a complex procedure,
;;; then you are almost certainly doing something wrong. You can take
;;; significant advantage of the fact that we are using an ordered set
;;; representation.)

(defun test ()
  (let ((pairs (list (list 'a 4) (list 'b 2) (list 'c 1) (list 'd 1)))
        (sample-tree
          (make-code-tree (make-leaf 'a 4)
                          (make-code-tree (make-leaf 'b 2)
                                          (make-code-tree (make-leaf 'c 1)
                                                          (make-leaf 'd 1))))))
    (let ((my-tree (generate-Huffman-tree pairs)))
      (print sample-tree)
      (print my-tree)
      (print (equal sample-tree my-tree))
      nil)))
