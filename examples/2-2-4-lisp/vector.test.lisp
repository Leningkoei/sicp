(load "vector.lisp")
(in-package Vector)

((lambda (a b)
  (print (get-x a))
  (print (get-y a))
  (print (add a b))
  (print (sub a b))
  (print (scale 2 a))
  nil)
 (new 1 2) (new 3 4))

(fresh-line)
(sb-ext:quit)

