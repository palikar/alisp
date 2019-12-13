(cond
 ((eq 10 11) (println "ten is eleven")) 
 ((eq 10 10) (println "ten is ten"))
 ('t (println "final")))

(defvar new-var (cond
                 ((eq 10 11) "eleven") 
                 ((eq 10 10) "ten")
                 ('t "final")))

(println new-var)
