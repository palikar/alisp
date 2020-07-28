(import 'assert-utils :all)
(import 'asserts :all)
(import 'async :all)
(import 'time :all)

(timeout (lambda () (assert t)) 100)

(defvar fut
  (async-start (lambda ()
                 (print "action...")
                 (t-sleep 100))))

(assert-not (async-ready fut))
(assert-not (async-state fut))

(async-await fut)

(assert (async-state fut))
(assert (async-ready fut))
