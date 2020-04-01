(import 'func :all)


(dump (reduce * '(1 2 3 4)))


(defun plus-1 (num)
  (+ num 1))

(defun plus-2 (num)
  (+ num 2))

(dump ((compose plus-1 plus-2 plus-1 ) 10))
