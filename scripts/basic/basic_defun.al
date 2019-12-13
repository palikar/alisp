(defun fun-1 ()
  (println "fun-1"))

(defun fun-2 (a b)
  (println "fun-1" a b))

(defun fun-3 (a b &optional c)
  (println "fun-1" a b)
  (println c))

(defun fun-4 (a b &optional c &rest d)
  (println "fun-1" a b)
  (println c)
  (println d))

(fun-1)
(fun-2 "a" "b")
(fun-3 "a" "b")
(fun-3 "a" "b" "c")
(fun-4 "a" "b" "c" "d")
(fun-4 "a" "b" "c" "d" "e")


(defun ret-fun()
  "ret-fun")
(fun-2 (ret-fun) (ret-fun))

(defvar a "a")
(defvar b "b")
(fun-2 a b)
