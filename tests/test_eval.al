(import 'assert-utils :all)
(import 'asserts :all)


(assert-real (eval-string "2.3"))
(assert-int (eval-string "423"))
(assert-== 64 (eval-string "(+ 32 32)"))
