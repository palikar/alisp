(import 'random :all)

(csrand 123)
(assert (pint (crand)))
(assert (pint (crand)))

(csrand)
(assert (pint (crand)))
(assert (pint (crand)))

(seed 123)

(assert (pint (rand-int 0 10)))
(assert (pstring (choice '("a" "b" "c" "d" "f" "g"))))
(assert (plist (sample '("a" "b" "c" "d" "f" "g") 10)))

(seed-rand)

(assert (preal (uniform 20.0 30.0)))
(assert (preal (expovariate 20.0)))
(assert (preal (gammavariate 20.0 30.0)))
(assert (preal (gauss 10.0 5.0)))
(assert (preal (lognormvariate 10.0 5.0)))
(assert (preal (weibullvariate 10.0 4.0)))
(assert (preal (student-t 10.0)))
(assert (preal (fisher-f 10.0 3.0)))
(assert (preal (geometric 10.0)))
