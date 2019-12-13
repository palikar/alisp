(defvar cnt 0)
(while (< cnt 10)
  (println "cnt: " cnt)
  (setq cnt (+ 1 cnt)))

(dolist (element '(1 2 3 4 5 "s"))
  (println "element: " element))
