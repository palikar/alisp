(import 'process :all)

(popen '("ls") '())

;; (let ((p (popen '("cat") '())))
;;   (send p "asdsad")
;;   (println "Process is done"))
