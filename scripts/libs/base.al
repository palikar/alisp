;; (import 'base64 :all)
;; (dump (base32-decode-string (base32-encode-string "I love bubence")))
;; (dump (base32-encode-string "this is string"))

;; (defgen gen (start end)
;;   (yield 1)
;;   (yield 2)
;;   (yield 3)
;;   )


;; (dogen (i (gen 1 10))
;;        (println i))


;; (defun taken ()
;;   (let ((l '()))
;;     (dolist (i (range 1 10))
;;       (dogen (j gen)
;;              (push j li)
;;              ))))


;; (if nil (println "this is basic")
;;   (println "this is not basic")
;;   (println "this is not basic")
;;   (println "this is not basic"))

(defvar a 42)
(println (+ 23 34 34 34 a (+ 12 30)))

;; (when t
;;   (println "this is not basic")
;;   "sadsa"
;;   (println "this is not basic"))
