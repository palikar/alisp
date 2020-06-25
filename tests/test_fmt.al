(import 'fmt :all)
(import 'assert-utils :all)
(import 'asserts :all)

(assert-equal "this 8" (fmt "this {0}" 8 "b" 1.3))
(assert-equal "this 8" (fmt "this {}" 8))
(assert-equal "this b" (fmt "this {1}" 8 "b"))

(assert-string (fmt "{:o}" 65))
(assert-string (fmt "{:b}" 65))
(assert-string (fmt "{:x}" 65))

(assert-string (fmt "{:-<34}" 65))
(assert-string (fmt "{:->34}" 65))
(assert-string (fmt "{:-^34}" 65))

(assert-equal "this 8" (printf "this {0}" 8 "b" 1.3))
(assert-equal "this 8" (printf "this {}" 8))
(assert-equal "this b" (printf "this {1}" 8 "b"))

(assert-string (printf "{:o}" 65))
(assert-string (printf "{:b}" 65))

(assert-equal "this 8" (printfln "this {0}" 8 "b" 1.3))
(assert-equal "this 8" (printfln "this {}" 8))
(assert-equal "this b" (printfln "this {1}" 8 "b"))

(assert-string (printfln "{:o}" 65))
(assert-string (printfln "{:b}" 65))

(assert-equal "this 8" (eprintf "this {0}" 8 "b" 1.3))
(assert-equal "this 8" (eprintf "this {}" 8))
(assert-equal "this b" (eprintf "this {1}" 8 "b"))

(assert-string (eprintf "{:o}" 65))
(assert-string (eprintf "{:b}" 65))

(assert-equal "this 8" (eprintfln "this {0}" 8 "b" 1.3))
(assert-equal "this 8" (eprintfln "this {}" 8))
(assert-equal "this b" (eprintfln "this {1}" 8 "b"))

(assert-string (eprintfln "{:o}" 65))
(assert-string (eprintfln "{:b}" 65))
