(defun prim (n)
  (dolist (i (range 1 n))
    (when (== (mod n i) 0) (return 'nil)))
  (return 't))



(defun primes (n)  
  (dolist (num (range 1 n))
    (when (prim num)
      (println num)
      )))


(defvar N 1000)
(primes N)
