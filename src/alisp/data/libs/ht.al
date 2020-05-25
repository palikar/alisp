(defvar --doc-- "This is the ht library")

(defun ht--assert (sym)
  (assert (prop-exists sym "--hash-table--" )))

(defun ht-create ()
  (let ((h (list)))
    (prop-set h "--hash-table--" t)
    h))

(defun ht-merge (&rest tables))

(defun ht-copy (table))

(defun ht-select (predicate))

(defun ht-reject (predicate))


(defun ht-select-keys (table keys))


(defun ht-get (table key &optional default))

(defun ht-keys (table))

(defun ht-values (table))

(defun ht-items (table))

(defun ht-find (predicate table))

(defun ht-size (table)
  (ht--assert table)
  (length table))



(defun ht-set (table key value)
  (ht--assert table)
  (unless (prop-exists table key)
    (shove table key))
  (prop-set table key value))

(defun ht-update (table-1 table-2))

(defun ht-remove (table key))

(defun ht-clear (table))




(defun ht-map (predicate))

(defun ht-each (predicate))




(defun ht? (table-or-object))

(defun ht-contains? (table key))

(defun ht-equal? (table1 table2))

(defun ht-empty? (table))



(defvar h (ht-create))
(ht-set h "sym" 42)
(dump (prop-list h))
(dump h)
