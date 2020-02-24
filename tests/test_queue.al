(import 'queue :all)

(defvar qu (queue-create))

(assert (queue-p qu))

(defvar not-qu 42)
(assert (not (queue-p not-qu)))

(assert (== 0 (queue-length qu)))
(assert (queue-empty qu))

(queue-enqueue qu 1)
(queue-enqueue qu 2)
(queue-enqueue qu 3)
(queue-enqueue qu 4)
(queue-enqueue qu 5)

(assert (== 5 (queue-length qu)))
(assert (not (queue-empty qu)))
(assert (== 1 (queue-first qu)))
(assert (== 5 (queue-last qu)))
(assert (== 2 (queue-nth qu 1)))
(assert (== 3 (queue-nth qu 2)))

(assert (== 1 (queue-dequeue qu)))
(assert (== 2 (queue-dequeue qu)))
(assert (== 3 (queue-dequeue qu)))
(assert (== 4 (queue-dequeue qu)))

(assert (== 1 (queue-length qu)))

(queue-clear qu)

(assert (== 0 (queue-length qu)))
(assert (queue-empty qu))
