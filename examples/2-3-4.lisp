;;;; 2-3-4: Huffman encoding trees
;;;; 2022/05/01

;;; Representing Huffman trees

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))
(defun leaf? (object)
  (equal 'leaf (car object)))
(defun leaf-symbol (leaf)
  (cadr leaf))
(defun leaf-weight (leaf)
  (caddr leaf))

(defun encode-tree-left-branch (tree)
  (car tree))
(defun encode-tree-right-branch (tree)
  (cadr tree))
(defun encode-tree-symbols (tree)
  (if (leaf? tree) (leaf-symbol tree)
    (caddr tree)))
(defun encode-tree-weight (tree)
  (if (leaf? tree) (leaf-weight tree)
    (cadddr tree)))
(defun make-code-tree (left right)
  (list left right
        (append (encode-tree-symbols left) (encode-tree-symbols right))
        (+ (encode-tree-weight left) (encode-tree-weight right))))

;;; The decoding procedure

(defun choose-branch (bit branch)
  (cond ((= 0 bit) (encode-tree-left-branch branch))
        ((= 1 bit) (encode-tree-right-branch branch))
        (t (error "ERROR"))))
(defun decode (bits tree)
  (labels
   ((decode-1 (bits current-branch)
              (if (null bits) nil
                (let ((next-branch (choose-branch (car bits) current-branch)))
                  (if (leaf? next-branch)
                      (cons (leaf-symbol next-branch)
                            (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch))))))
   (decode-1 bits tree)))

;;; Sets of weighted elements

(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((< (encode-tree-weight x) (encode-tree-weight (car set))) (cons x set))
        (t (cons (car set) (adjoin-set x (cdr set))))))
