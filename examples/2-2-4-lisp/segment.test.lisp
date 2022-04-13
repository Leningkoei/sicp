(load "vector.lisp")
(load "segment.lisp")
(in-package Segment)

((lambda (vector-a vector-b)
  ((lambda (a)
    (print (get-start a))
    (print (get-end a))
    nil)
   (new vector-a vector-b)))
 (Vector:new 1 2) (Vector:new 3 4))

(fresh-line)
(sb-ext:quit)

