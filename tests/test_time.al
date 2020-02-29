(import 'time :all)

(t-sleep 20)

(assert (plist (t-gmtime (t-time))))
(assert (plist (t-localtime (t-time))))
(assert (pstring (t-ctime)))


(assert (preal (t-ns 1)))
(assert (preal (t-ms 1)))
(assert (preal (t-s 1)))
(assert (preal (t-hr 1)))

(assert (preal (t-clock-time system-clock)))
(assert (preal (t-clock-time steady-clock)))
(assert (preal (t-clock-time high-res-clock)))

