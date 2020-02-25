(defun set-create ()
  "(set-create)

Creates an empty set"
  (let ((q (list )))
    (prop-set q "--set--" t)
    q))

(defun set-p (setc)
  "(set-p SETC)

Check if `SETC` is a set. "
  (prop-exists setc "--set--"))

(defun set-empty (setc)
  "(set-empty SETC

Check if set is empty and return `t` if it is. Return `nil`
otherwise.  )"
  (if (set-p setc)
      (== (length setc) 0)))

(defun set-first (setc)
  "(set-first SETC)

Return the first element of the set.
"
  (when (set-p setc)
    (first setc)))

(defun set-nth (set n)
  "(set-nth SETC INDEX)

Return the element of the set at position `INDEX`.
"
  (when (set-p setc)
    (nth setc n)))

(defun set-last (setc)
  "(set-last SETC)

Return the last element of the set.
"
  (when (set-p setc)
    (last setc)))

(defun set-copy (setc)
  "(set-copy SETC)

Return a copy of the set
"
  (when (set-p setc)
    (let ((new-q (list)))
      (dolist (el setc)
        (push new-q el))
      new-q)))

(defun set-length (setc)
  "(set-length SETC)

Return the number of elements in the set.
"
  (when (set-p setc)
    (length setc)))

(defun set-clear (setc)
  "(set-clear SETC)

Remove all elements in the set.
"
  (when (set-p setc)
    (clear set)))

(defun set-add (setc el)
  "(set-clear SETC)

Remove all elements in the set.
"
  (when (set-p setc)
    (unless (contains setc el)
      (push setc el))))

(defun set-remove (setc el)
  "(set-clear SETC)

Remove all elements in the set.
"
  (when (set-p setc)
    (clear setc)))
