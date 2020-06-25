(import 'func :all)
(import 'assert-utils :all)


(defvar var 42)

(defun plus (num-1 num-2)
  (+ num-1 num-2))

(defun plus-1 (num)
  (+ num 1))

(defun plus-2 (num)
  (+ num 2))

(assert-== 24 (reduce * '(1 2 3 4)))
(assert-== 14 ((compose plus-1 plus-2 plus-1 ) 10))
(assert-not (ignore dsf adsf dsaf))
(assert-equal "asd" (identity "asd"))
(assert-eq 'var (identity var))
(assert-== 4 ((partial plus 1) 3))
(assert-== 46 ((partial plus 43) 3))
(assert-== 46 ((partial plus _ 43) 3))
