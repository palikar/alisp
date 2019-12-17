(cond
 ((== 10 11) (println "ten is eleven")) 
 ((== 10 10) (println "ten is ten"))
 ('t (println "final")))

(defvar new-var (cond
                 ((== 10 11) "eleven") 
                 ((== 10 10) "ten")
                 ('t "final")))

(println new-var)
