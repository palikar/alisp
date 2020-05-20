### Queue

#### Description


#### Functions

**queue-clear** : *(queue-clear QUEUE)*

Remove all elements in the queue.


**queue-copy** : *(queue-copy QUEUE)*

Return a copy of the queue


**queue-last** : *(queue-last QUEUE)*

Return the last element of the queue.


**queue-nth** : *(queue-nth QUEUE INDEX)*

Return the element of the queue at position `INDEX`.


**queue-length** : *queue-length QUEUE*

Return the number of elements in the queue.


**queue-empty** : *(queue-empty QUEUE*

Check if queue is empty and return `t` if it is. Return `nil`
otherwise.  )

**queue-first** : *(queue-first QUEUE)*

Return the first element of the queue.


**queue-dequeue** : *(queue-eqqueue QUEUE)*

Remove the first element from queue and return it.

**queue-enqueue** : *(queue-enqueue QUEUE ELEMENT)*

Insert element last into queue.

**queue-p** : *(queue-p QUEUE)*

Check if `QUEUE` is a queue. 

**queue-create** : *(queue-create)*

Creates an empty queue

#### Constants
