;;;; 3-46
;;;; 3-4-2
;;;; 2022/07/01

;;; Suppose that we implement `test-and-set!` using an ordinary procedure as
;;; shown in the text, without attempting to make the operation atomic. Draw a
;;; timing diagram like the one in figure 3.29 to demonstrate how the mutex
;;; implementation can fail by allowing two processes to acquire the mutex at
;;; the same time.

;; two processes use one cell concurrently
;; 
;;     a  ce ll  b
;;          |
;;     +----+----+
;;     |         |
;;  acquire   acquire
;;     |         |
;; get false get false
;;     |         |
;; set true   set true
;;     |         |
;;  do sth     do sth
