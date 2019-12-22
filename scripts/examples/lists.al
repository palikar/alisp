
(dump (head '("a" "b" "c")))

(dump (last '("a" "b" "c")))

(dump (car '("a" "b" "c")))

(dump (cons '("a" "b" "c")))

(dump (tail '("a" "b" "c")))

(dump (init '("a" "b" "c")))

(mapc dump (init '("a" "b" "c")))


(dump (push '("a" "b" "c") "d" ))
(dump (push '("a" "b" "c") 52 ))


(dump (delete '("a" "b" "c") "a" ))
(dump (delete '("a" "b" "c" 123 ) 123))
(dump (delete '(a b c ) 'b))

(dump (remove '("a" "b" "c") "a" ))
(dump (remove '("a" "b" "c" 123 ) 123))
(dump (remove '(a b c ) 'b))


