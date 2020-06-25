(import 'system :all)
(import 'assert-utils :all)
(import 'asserts :all)



(assert-list env-vars)
(assert-string (get-env "HOME"))
(assert (check-env "HOME"))

(assert-list (list-env))

(assert-not (chwd "adsföladsfjl"))
(assert (chwd "/"))



