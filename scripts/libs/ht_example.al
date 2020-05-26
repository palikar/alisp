(import 'ht :all)

(defvar h-1 (ht-create))
(ht-set h-1 "key-1" 41)
(ht-set h-1 "key-2" 42)
(ht-set h-1 "key-3" 43)
(ht-set h-1 "key-4" 44)

(defvar h-2 (ht-create))
(ht-set h-2 "key-1" 213)
(ht-set h-2 "key-5" 46)
(ht-set h-2 "key-6" 47)
(ht-set h-2 "key-7" 48)
(ht-set h-2 "key-8" 49)

(dump (ht-copy h-1))
(dump (ht-merge h-1 h-2))

(dump (ht-select h-1 (lambda (key value) (== 0 (mod value 2)))))
(dump (ht-reject h-1 (lambda (key value) (== 0 (mod value 2)))))

(dump (ht-select-keys h-1 '("key-1" "key-3")))

(dump (ht-get h-1 "key-1"))
(dump (ht-get h-1 "key-4"))

(dump (ht-keys h-1))
(dump (ht-values h-1))
(dump (ht-items h-1))
(dump (ht-size h-1))

(ht-set h-1 "key-1" 123)
(dump (ht-get h-1 "key-1"))

(ht-update h-1 h-2)
(dump (ht-get h-1 "key-1"))

(ht-remove h-1 "key-1")
(dump (ht-get h-1 "key-1"))

(ht-clear h-2)
(println "h-1: ")
(ht--dump h-1)
(println "h-2: ")
(ht--dump h-2)

(dump (ht-empty? h-1))
(dump (ht-empty? h-2))

(dump (ht? h-1))
(dump (ht? h-2))

(ht--dump (ht-map h-1 (lambda (value) (* value 2))))

(ht-each h-1 (lambda (key value)
               (println key " -:- " value)))

(dump (ht-equal? h-1 h-2))
