(defun queue-create ()
  "(queue-create)

Creates an empty queue"
  (let ((q (list )))
    (prop-set q "--queue--" t)
    q))

(defun queue-p (queue)
  "(queue-p QUEUE)

Check if `QUEUE` is a queue. "
  (prop-exists queue "--queue--"))

(defun queue-enqueue (queue element)
  "(queue-enqueue QUEUE ELEMENT)

Insert element last into queue."
  (when (queue-p queue)
    (shove queue element)))

(defun queue-dequeue (queue)
  "(queue-eqqueue QUEUE)

Remove the first element from queue and return it."
  (when (queue-p queue)
    (let ((el (last queue)))
      (delete queue el)
      el)))

(defun queue-empty (queue)
  "(queue-empty QUEUE

Check if queue is empty and return `t` if it is. Return `nil`
otherwise.  )"
  (if (queue-p queue)
      (== (length queue) 0)))

(defun queue-first (queue)
  "(queue-first QUEUE)

Return the first element of the queue.
"
  (when (queue-p queue)
    (last queue)))

(defun queue-nth (queue n)
  "(queue-nth QUEUE INDEX)

Return the element of the queue at position `INDEX`.
"
  (when (queue-p queue)
    (nth queue (- (length queue) n 1))))

(defun queue-last (queue)
  "(queue-last QUEUE)

Return the last element of the queue.
"
  (when (queue-p queue)
    (head queue)))

(defun queue-copy (queue)
  "(queue-copy QUEUE)

Return a copy of the queue
"
  (when (queue-p queue)
    (let ((new-q (list)))
      (dolist (el queue)
        (push new-q el))
      new-q)))

(defun queue-length (queue)
  "queue-length QUEUE

Return the number of elements in the queue.
"
  (when (queue-p queue)
    (length queue)))

(defun queue-clear (queue)
  "(queue-clear QUEUE)

Remove all elements in the queue.
"
  (when (queue-p queue)
    (clear queue)))
