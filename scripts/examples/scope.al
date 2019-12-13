;; printing 10 10 42 43 42 10 10

(defun do-it(a)
  (println a)
  (let ((a 42))
    (println a)
    (let ((a (+ a 1)))
      (println a))
    (println a))
  (println a))

(defvar a 10)
(println a)
(do-it a)
(println a)
