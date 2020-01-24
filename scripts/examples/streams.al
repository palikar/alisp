(println "Starting the stream!")

(let ((s "string"))
  (println s)
  )

;; (let ((s (stream :from-string "")))
;;   (println "Reading from the stream")
;;   (stream-write-lines s '("line1" "line2" "line3"))
;;   (stream-write s "line4")
;;   (println (stream-content s))
;;   (stream-close s)
;;   )


;; (let ((s (stream :from-string ":")))
;;   (with-cout s
;;              (print "line1|")
;;              (print "line2"))

;;   (println "Content-> "(stream-content s))

;;   (stream-close s)
;;   )

(println "end")

