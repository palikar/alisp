

(defvar a "a")
(defvar l '("a" "b") )

(dump `(,@l b ,a))
(dump `(,@a b c))
(dump `((,a) (b ,a (b c d ,a)) c))
(dump `,a)



