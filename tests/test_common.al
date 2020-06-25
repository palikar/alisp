(import 'platform :all)

(assert (dumpbuildscript))

(assert (println os))
(assert (println alisp-version))
(assert (println compiler-name))
(assert (println compiler-version))
(assert (println arch))
(assert (println enabled-stack-trace))
(assert (println debug-build))
(assert (println enabled-documentation))
(assert (println enabled-line-trace))
(assert (println disabled-checks))
(assert (println max-evaluation-depth))
(assert (println build-info))
(assert (println --al-license--))

(assert (dumpcredits))

(when t (println "hello"))
(unless nil (println "hello"))

(if t (println "hello then") (println "hello else"))
(if nil (println "hello then") (println "hello else"))

(let (nil-var)
  "sasad"
  t
  "asdasd"
  nil)

(print (+ 213 32 (+ 23 32)))
(print (- 213 32 (- 43 32)))
(print (* 213 32 (* 43 32)))
(print (/ 213 32 (/ 43 32)))

(print (+ 213.0 32.0 (+ 43.0 32.0)))
(print (- 213.0 32.0 (- 43.0 32.0)))
(print (* 213.0 32.0 (* 43.0 32.0)))
(print (/ 213.0 32.0 (/ 43.0 32.0)))
