(import 'assert-utils :all)
(import 'asserts :all)
(import 'math :all)


(assert-== 4 (pow 2 2))

(assert-real (exp 2))
(assert-real (exp2 2))
(assert-real (expm1 2))
(assert-real (log 12))
(assert-real (log10 12))
(assert-real (log1p 12))

(assert-real (sin 12))
(assert-real (cos 12))
(assert-real (tan 12))

(assert-real (asin 12))
(assert-real (acos 12))
(assert-real (atan 12))

(assert-real (sinh 12))
(assert-real (cosh 12))
(assert-real (tanh 12))

(assert-real (asinh 12))
(assert-real (acosh 12))

(assert-real (ceil 12.32))
(assert-real (floor 12.32))

(assert-real (erf 12.32))
(assert-real (erfc 12.32))

(assert-real (tgamma 12.32))
(assert-real (lgamma 2.32))

(assert-real (hypot 2.32 8.2))
(assert-real (fdim 2.32 23))

(assert-real (sqrt 2))
(assert-real (cbrt 2))

(assert-int (gcd 43 32))
(assert-int (lcm 43 32))
