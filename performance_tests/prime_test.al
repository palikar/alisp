(defun prim (number)
  (dolist (i (range 2 number))
    (when (== (mod number i) 0) (return 'nil)))
  (return 't))


(defun primes (n)  
  (dolist (num (range 1 n))
    (when (prim num)
      (println num))))


(defvar N 1000)
(primes N)

