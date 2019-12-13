(println (+ 10 10))
(println (+ 10 (* 10 10)))
(println (+ 10 (/ 10 10)))
(println (+ 10 (- 10 9)))

(println (+ 10 10.0))
(println (+ 10 (* 10.0 10)))
(println (+ 10 (/ 10 10.0)))
(println (+ 10.0 (- 10 9)))

(println (/ 10 3))
(println (/ 10 3.0))
(println (/ 10.0 3))

(println (when (<= 10 10) "<= 10 10"))
(println (when (< 10 10) "< 10 10"))
(println (when (>= 10 10) ">= 10 10"))
(println (when (> 10 10) "> 10 10"))

(println (when (<= 10 11) "<= 10 11"))
(println (when (< 10 11) "< 10 11"))
(println (when (>= 10 11) ">= 10 11"))
(println (when (> 10 11) "> 10 11"))

(println (when (<= 11 10) "<= 11 10"))
(println (when (< 11 10) "< 11 10"))
(println (when (>= 11 10) ">= 11 10"))
(println (when (> 11 10) "> 11 10"))

(println (when (== 11 10) "== 11 10"))
(println (when (== 10 11) "== 10 11"))
(println (when (== 10 10) "== 10 10"))

(println (when (!= 11 10) "!= 11 10"))
(println (when (!= 10 11) "!= 10 11"))
(println (when (!= 10 10) "!= 10 10"))
