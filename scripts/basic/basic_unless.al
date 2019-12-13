
(defvar new-var 10)
(unless (== new-var 10)
  (println "The var is not 10"))

(setq new-var 12)
(unless (== new-var 10)
  (println "The var is not 10"))
