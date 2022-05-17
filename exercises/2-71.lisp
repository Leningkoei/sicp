;;;; 2-71
;;;; 2-3-4
;;;; 2022/05/17

;;; Suppose we have a Huffman tree for an alphabet of `n` symbols, and that the
;;; relative frequencies of the symbols are `1, 2, 4, ..., 2^(n - 1)`. Sketch
;;; the tree for `n = 5`; for `n = 10`. In such a tree (for general `n`) how
;;; many bits are required to encode the most frequent symbol? the least
;;; frequent symbol?

;;; answer

;; most  frequent symbol: 1 bit
;; least frequent symbol: n - 1 bits
