(import 'time :all)


(dump (t-gmtime (t-time)))

(dump t-localtime (t-time))

(dump (t-hr 1))

(t-sleep 1000)

(dump (t-ctime))
