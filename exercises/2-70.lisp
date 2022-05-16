;;;; 2-70
;;;; 2-3-4
;;;; 2022/05/16

;;; The following eight-symbol alphabet with associated relative frequencies
;;; was designed to efficiently encode the lyrics of 1950s rock songs. (Note
;;; that the "symbols" of an "alphabet" need not be individual letters.)
;;; A    2 | NA  16
;;; BOOM 1 | SHA 3
;;; GET  2 | YIP 9
;;; JOB  2 | WAH 1
;;; Use `generate-Huffman-tree` (exercise 2.69) to generate a corresponding
;;; Huffman tree, and use `encode` (exercise 2.68) to encode the following
;;; message:
;;; get a job
;;; sha na na na na na na na na
;;; get a job
;;; sha na na na na na na na na
;;; wah yip yip yip yip yip yip yip yip yip 
;;; sha boom
;;; How many bits are required for the encoding? What is the smallest number of
;;; bits that would be needed to encode this song if we used a fixed-length
;;; code for the eight-symbol alphabet?

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
  (if (null pairs)
      nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))
(defun successive-merge (pairs)
  (labels
      ((iterator (result rest-pairs)
         (if (null rest-pairs)
             result
             (let ((new-result (make-code-tree (car rest-pairs) result))
                   (new-rest-pairs (cdr rest-pairs)))
               (iterator new-result new-rest-pairs)))))
    (if (null pairs)
        nil
        (iterator (car pairs) (cdr pairs)))))
(defun generate-Huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))
(defun test-generate-Huffman-tree ()
  (let ((pairs (list (list 'a 4) (list 'b 2) (list 'c 1) (list 'd 1)))
        (sample-tree
          (make-code-tree (make-leaf 'a 4)
                          (make-code-tree (make-leaf 'b 2)
                                          (make-code-tree (make-leaf 'd 1)
                                                          (make-leaf 'c 1))))))
    (let ((my-tree (generate-Huffman-tree pairs)))
      (equal sample-tree my-tree))))

(defun encode-symbol (target tree)
  (labels
      ((iterator (result tree)
         (if (leaf? tree) (handle-leaf result tree) (handle-tree result tree)))
       (handle-tree (result tree)
         (let ((left-result
                 (iterator (append result (list 0)) (left-branch tree))))
           (if (null left-result)
               (iterator (append result (list 1)) (right-branch tree))
               left-result)))
       (handle-leaf (result leaf)
         (let ((symbol (symbol-leaf leaf)))
           (if (equal target symbol) result nil))))
    (let ((code (iterator nil tree)))
      (if (null code)
          (error "Undefined Symbol -- encode-symbol")
          code))))
(defun encode (message tree)
  (cond ((null tree) (error "Tree is a nil -- encode"))
        ((null message) nil)
        (t (append (encode-symbol (car message) tree)
                   (encode (cdr message) tree)))))
(defun test-encode ()
  (let ((sample-tree
          (make-code-tree (make-leaf 'a 4)
                          (make-code-tree (make-leaf 'b 2)
                                          (make-code-tree (make-leaf 'd 1)
                                                          (make-leaf 'c 1)))))
        (sample-message (list 'a 'b 'c 'd)))
    (encode sample-message sample-tree)))

(defun decode (bits tree)
  (labels
      ((choose-branch (bit branch)
         (cond ((= bit 0) (left-branch branch))
               ((= bit 1) (right-branch branch))
               (t (error "bad bit --choose-branch"))))
       (decode-1 (bits current-branch)
         (if (null bits)
             nil
             (let ((next-branch (choose-branch (car bits) current-branch)))
               (if (leaf? next-branch)
                   (cons (symbol-leaf next-branch)
                         (decode-1 (cdr bits) tree))
                   (decode-1 (cdr bits) next-branch))))))
    (decode-1 bits tree)))

(defconstant the-pairs
  (list (list 'a 2) (list 'na 16) (list 'boom 1) (list 'sha 3)
        (list 'get 2) (list 'yip 9) (list 'job 2) (list 'wah 1)))
(defconstant the-message
  (list 'get 'a 'job 'sha 'na 'na 'na 'na 'na 'na 'na 'na
        'get 'a 'job 'sha 'na 'na 'na 'na 'na 'na 'na 'na
        'wah 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip
        'sha 'boom))
(defconstant the-tree
  (generate-Huffman-tree the-pairs))
(defconstant the-code
  (encode the-message the-tree))
(defconstant the-origin-message
  (decode the-code the-tree))
