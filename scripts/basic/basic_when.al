
(defvar new-var 10)
(when (== new-var 10)
  (println "The var is 10"))

(setq new-var 12)
(when (== new-var 10)
  (println "The var is 10"))
