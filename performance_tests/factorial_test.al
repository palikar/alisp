
(defun factorial(n)
  (if (== n 1) 1
    (* n (factorial (- n 1)))))


(defvar N 35)
(defvar COUNT 1000)
(dolist (i (range 1 COUNT))
  (factorial N))
