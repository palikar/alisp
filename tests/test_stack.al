(import 'stack :all)

(defvar st (stack-create))

(assert (stack-p st))

(defvar not-st 42)
(assert (not (stack-p not-st)))

(assert (== 0 (stack-length st)))
(assert (stack-empty st))

(stack-push st 1)
(stack-push st 2)
(stack-push st 3)
(stack-push st 4)
(stack-push st 5)

(assert (== 5 (stack-length st)))
(assert (not (stack-empty st)))
(assert (== 1 (stack-first st)))
(assert (== 5 (stack-last st)))
(assert (== 2 (stack-nth st 1)))
(assert (== 3 (stack-nth st 2)))

(assert (== 5 (stack-pop st)))
(assert (== 4 (stack-pop st)))
(assert (== 3 (stack-pop st)))
(assert (== 2 (stack-pop st)))

(assert (== 1 (stack-length st)))

(stack-clear st)

(assert (== 0 (stack-length st)))
(assert (stack-empty st))
