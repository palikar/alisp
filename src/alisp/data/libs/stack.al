(defun stack-create ()
  "(stack-create)

Creates an empty stack"
  (let ((s (list )))
    (prop-set s "--stack--" t)
    s))

(defun stack-p (stack)
  "(stack-p STACK)

Check if `STACK` is a stack. "
  (prop-exists stack "--stack--"))

(defun stack-push (stack element)
  "(stack-push STACK ELEMENT)

Insert element last into stack."
  (when (stack-p stack)
    (push stack element)))

(defun stack-pop (stack)
  "(stack-pop STACK)

Remove the first element from stack and return it."
  (when (stack-p stack)
    (let ((el (last stack)))
      (delete stack el)
      el)))

(defun stack-empty (stack)
  "(stack-empty STACK

Check if stack is empty and return `t` if it is. Return `nil`
otherwise.  )"
  (if (stack-p stack)
      (== (length stack) 0)))

(defun stack-first (stack)
  "(stack-first STACK)

Return the first element of the stack.
"
  (when (stack-p stack)
    (head stack)))

(defun stack-nth (stack n)
  "(stack-nth STACK INDEX)

Return the element of the stack at position `INDEX`.
"
  (when (stack-p stack)
    (nth stack n)))

(defun stack-last (stack)
  "(stack-last STACK)

Return the last element of the stack.
"
  (when (stack-p stack)
    (last stack)))

(defun stack-copy (stack)
  "(stack-copy STACK)

Return a copy of the stack
"
  (when (stack-p stack)
    (let ((new-s (list)))
      (dolist (el stack)
        (push new-s el))
      new-s)))

(defun stack-length (stack)
  "stack-length STACK

Return the number of elements in the stack.
"
  (when (stack-p stack)
    (length stack)))

(defun stack-clear (stack)
  "(stack-clear STACK)

Remove all elements in the stack.
"
  (when (stack-p stack)
    (clear stack)))
