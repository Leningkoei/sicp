;;;; 2-64
;;;; 2-3-3
;;;; 2022/04/27

(defun quotient (a b)
  (let ((c (mod a b)))
    (let ((d (- a c)))
      (/ d b))))

;;; The following procedure `list->tree` converts an ordered list to a balanced
;;; binary tree. The helper procedure `partial-tree` takes as arguments an
;;; integer `n` and list of at least `n` elements and constructs a balanced
;;; tree containing the first `n` elements of the list. The result returned by
;;; `partial-tree` is a pair (formed with `cons`) whose `car` is the
;;; constructed tree and whose `cdr` is the list of elements not included in
;;; the tree.
(defun list->tree (elements)
  (car (partial-tree elements (length elements))))
(defun partial-tree (elts n)
  (if (= 0 n) (cons nil elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts) right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))
;;; a.Write a short paragraph explaining as clearly as you can how
;;; `partial-tree` works. Draw the tree produced by `list->tree` for the list
;;; `(1 3 5 7 9 11)`.
;;; (list->tree (1 3 5 7 9 11))
;;; (car (partial-tree (1 3 5 7 9 11) 6))
;;; + left-size: 2
;;; + left-result: (partial-tree (1 3 5 7 9 11) 2)
;;; + + left-size: 0
;;; + + left-result: (partial-tree (1 3 5 7 9 11) 0)
;;; + + + (nil (1 3 5 7 9 11))
;;; + + left-result: (nil (1 3 5 7 9 11))
;;; + + left-tree: nil
;;; + + non-left-elts: (1 3 5 7 9 11)
;;; + + right-size: 1
;;; + + this-entry: 1
;;; + + right-result: (partial-tree (3 5 7 9 11) 1)
;;; + + + left-size: 0
;;; + + + left-result: (partial-tree (3 5 7 9 11) 0)
;;; + + + + (nil (3 5 7 9 11))
;;; + + + left-result: (nil (3 5 7 9 11))
;;; + + + left-tree: nil
;;; + + + non-left-elts: (3 5 7 9 11)
;;; + + + right-size: 0
;;; + + + this-entry: 3
;;; + + + right-result: (partial-tree (5 7 9 11) 0)
;;; + + + + (nil (5 7 9 11))
;;; + + + right-result: (nil (5 7 9 11))
;;; + + + right-tree: nil
;;; + + + remaining-elts: (5 7 9 11)
;;; + + + ((3 nil nil) (5 7 9 11))
;;; + + right-result: ((3 nil nil) (5 7 9 11))
;;; + + right-tree: (3 nil nil)
;;; + + remaining-elts: (5 7 9 11)
;;; + + ((1 nil (3 nil nil)) (5 7 9 11))
;;; + left-result: ((1 nil (3 nil nil)) (5 7 9 11))
;;; + left-tree: (1 nil (3 nil nil))
;;; + non-left-elts: (5 7 9 11)
;;; + right-size: 3
;;; + this-entry: 5
;;; + right-result: (partial-tree (7 9 11) 3)
;;; + + left-size: 1
;;; + + left-result: (partial-tree (7 9 11) 1)
;;; + + + left-size: 0
;;; + + + left-result: (partial-tree (7 9 11) 0)
;;; + + + + (nil (7 9 11))
;;; + + + left-result: (nil (7 9 11))
;;; + + + left-tree: nil
;;; + + + non-left-elts: (7 9 11)
;;; + + + right-size: 0
;;; + + + this-entry: 7
;;; + + + right-result: (partial-tree (9 11) 0)
;;; + + + + (nil (9 11))
;;; + + + right-result: (nil (9 11))
;;; + + + right-tree: nil
;;; + + + remaining-elts: (9 11)
;;; + + + ((7 nil nil) (9 11))
;;; + + left-result: ((7 nil nil) (9 11))
;;; + + left-tree: (7 nil nil)
;;; + + non-left-tree: (9 11)
;;; + + right-size: 1
;;; + + this-entry: 9
;;; + + right-result: (partial-tree (11) 1)
;;; + + + left-size: 0
;;; + + + left-result: (partial-tree (11) 0)
;;; + + + + (nil (11))
;;; + + + left-result: (nil (11))
;;; + + + left-tree: nil
;;; + + + non-left-elts: (11)
;;; + + + right-size: 0
;;; + + + this-entry: 11
;;; + + + right-result: (partial-tree nil 0)
;;; + + + + (nil nil)
;;; + + + right-result: (nil nil)
;;; + + + right-tree: nil
;;; + + + remaining-elts nil
;;; + + + ((11 nil nil) nil)
;;; + + right-result: ((11 nil nil) nil)
;;; + + right-tree: (11 nil nil)
;;; + + remaining-elts: nil
;;; + + ((9 (7 nil nil) (11 nil nil)) nil)
;;; + right-result: ((9 (7 nil nil) (11 nil nil)) nil)
;;; + right-tree: (9 (7 nil nil) (11 nil nil))
;;; + remaining-elts: nil
;;; + ((5 (1 nil (3 nil nil)) (9 (7 nil nil) (11 nil nil))) nil)
;;; (car ((5 (1 nil (3 nil nil)) (9 (7 nil nil) (11 nil nil))) nil))

;;; b. What is the order of growth in the number of steps required by
;;; `list-tree` to convert a list of `n` elements?
