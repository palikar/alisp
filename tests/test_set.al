(import 'setc :all)


(defvar s (set-create))

(set-add s 1)
(set-add s 2)
(set-add s 3)
(set-add s 3)
(set-add s 4)
(set-add s 4)
(set-add s "a")
(set-add s "a")
(set-add s 's)
(set-add s 's)

(dump s)

