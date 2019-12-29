(defun variables-test (count)
  (let ((index 0))
    (while (< index count )
      (let (a)
        (setq a index))
      (setq index (+ index 1)))))

(defvar n 100000)
(variables-test n)
