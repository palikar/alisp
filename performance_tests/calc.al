(let ((total 0)
      (index 0))

  (while (< index 10 000)
    (setq total ( + (*  (/ 3 2) (+ (/ 2 16.0) 12) 100) (* 10 19) (- 89 (* 103 54)) (<< 100  6)))
    (setq index (+ index 1)))
  
  (print total))
