(import 'random :all)

(assert (pint (crand)))
(assert (pint (crand)))

(assert (pint (rand-int 0 10)))
(assert (pstring (choice '("a" "b" "c" "d" "f" "g"))))
(assert (plist (sample '("a" "b" "c" "d" "f" "g") 10)))

(assert (preal (uniform 20.0 30.0)))
(assert (preal (exponential 20.0)))
(assert (preal (gamma 20.0 30.0)))
(assert (preal (gauss 10.0 5.0)))
(assert (preal (lognorm 10.0 5.0)))
(assert (preal (weibull 10.0 4.0)))
(assert (preal (student-t 10.0)))
(assert (preal (fisher-f 10.0 3.0)))
(assert (preal (geometric 10.0)))
