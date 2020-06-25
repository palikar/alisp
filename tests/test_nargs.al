(import 'nargs :all)
(import 'assert-utils :all)


(assert (nargs-has '(:flag) :flag))
(assert (nargs-has '(:flag "kjalds" :flag-2) :flag))
(assert-not (nargs-has '(:flag) :flag-non))

(assert (nargs-next '(:flag t) :flag))
(assert-not (nargs-next '(:flag t) :flag-non))

(assert-equal "hello" (nargs-next '(:flag "hello") :flag))
(assert-equal "hello" (nargs-next '(:flag "hello" :flag-2 "hello-2") :flag))
(assert-equal "hello-2" (nargs-next '(:flag "hello" :flag-2 "hello-2") :flag-2))
(assert-not-equal "hello_not" (nargs-next '(:flag "hello") :flag))

(assert (nargs-truthy '(:flag t) :flag))
(assert (nargs-truthy '(:flag "asd") :flag))
(assert (nargs-truthy '(:flag 213) :flag))

(assert-not (nargs-falsey '(:flag t) :flag))
(assert-not (nargs-falsey '(:flag "asd") :flag))
(assert-not (nargs-falsey '(:flag 213) :flag))

(assert (nargs-falsey '(:flag nil) :flag))
(assert (nargs-falsey '(:flag ()) :flag))
(assert (nargs-falsey '(:flag "") :flag))

(assert-not (nargs-truthy '(:flag nil) :flag))
(assert-not (nargs-truthy '(:flag ()) :flag))
(assert-not (nargs-truthy '(:flag "") :flag))


