;;;; 2-67
;;;; 2-3-4
;;;; 2022/??/??

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))
(defun leaf? (object)
  (equal 'leaf (car object)))
(defun leaf-symbol (this)
  (nth 1 this))
(defun leaf-weight (this)
  (nth 2 this))

(defun code-tree-? (object)
  (equal 'code-tree (car object)))
(defun code-tree-left (this)
  (nth 1 this))
(defun code-tree-right (this)
  (nth 2 this))
(defun code-tree-symbol (this)
  (nth 3 this))
(defun code-tree-weight (this)
  (nth 4 this))

(defun leaf-&-code-tree=>symbol (this)
  "If I want to expand this method, I have to change the method name anywhere it used."
  (cond ((leaf? this) (leaf-symbol this))
        ((code-tree-? this) (code-tree-symbol this))
        (t (error "Unknown type -- leaf-&-code-tree=>symbol"))))
(defun leaf-&-code-tree=>weight (this)
  "As same as ...=>symbol"
  (cond ((leaf? this) (leaf-weight this))
        ((code-tree-? this) (code-tree-symbol this))
        (t (error "Unknown type -- leaf-&-code-tree=>weight"))))

(defun make-code-tree (left right)
  "leaf | code-tree -> leaf | code-tree -> code-tree"
  (list 'code-tree left right
        (append (leaf-&-code-tree=>symbol left)
                (leaf-&-code-tree=>symbol right))
        (+ (leaf-&-code-tree=>weight left)
           (leaf-&-code-tree=>weight right))))

;;; Define an encoding tree and a sample message:
(defun sample-tree ()
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
(defun sample-message ()
  (list 0 1 1 0 0 1 0 1 0 1 1 1 0))
;;; Use the `decode` procedure to decode the message, and give the result.
