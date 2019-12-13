(when (and 't 't) (println "true"))
(unless (and 't 'nil) (println "false"))
(unless (and 'nil 't) (println "false"))
(unless (and 'nil 'nil) (println "false"))

(when (or 't 't) (println "true"))
(unless (or 't 'nil) (println "true"))
(unless (or 'nil 't) (println "true"))
(unless (or 'nil 'nil) (println "false"))

(when (not (and 'nil 'nil))) "true")
