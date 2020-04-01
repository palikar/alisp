(import 'locale :all)

(set-preffered-locale)
(dump (locale-name))
(set-locale (locale "de_DE.utf8"))
(dump (locale-name))
(reset-locale)
(dump (locale-name))

(dump (put-money 123.3))
(dump (put-num 123.3))
(dump (put-time 1584911772 "%c"))

(println "---------------")

(dump (isgraph ?2))
(dump (isprint ?a))
(dump (isalnum ?3))
(dump (isxdigit ?a))
(dump (ispunct ?.))
(dump (isdigit ?3))
(dump (isalpha ?a))
(dump (islower ?a))
(dump (isupper ?A))
(dump (iscntrl 213))
(dump (isblank ? ))
(dump (isspace ? ))

(println "---------------")

(dump  (to-string (money-decimal-point)))
(dump  (to-string (money-thousand-sep)))
(dump  (to-string (money-symobl)))
(dump  (to-string (money-negative-sign)))
(dump  (to-string (money-positive-sign)))
(dump  (to-string (num-decimal-point)))

(println "---------------")

(dump  (to-string (num-thousand-sep)))
(dump  (to-string (num-false-name)))
(dump  (to-string (num-true-name)))


(defun m ()
  nil
  )

(let ((ls nil))
  nil)
