(defvar new-var (funcall (lambda (a) (println a) a)) "")

(defvar new-lam (lambda (a) (println a) a))
(funcall new-lam "a")
(funcall 'new-lam "a")


