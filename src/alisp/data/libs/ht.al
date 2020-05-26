(defvar --doc-- "This is the ht library")


(defun ht--assert (sym)
  "(ht--assert SYM)

"
  (assert (prop-exists sym "--hash-table--" )))

(defun ht--dump (table)
  "(ht--dump TABLE)

"
  (ht--assert table)
  (dolist (key table)
    (println key " : " (ht-get table key))))

(defun ht-create ()
  "(ht-create)

"
  (let ((h (list)))
    (prop-set h "--hash-table--" t)
    h))

(defun ht-merge (&rest tables)
  "(ht-merge &REST TABLES)

"
  (let ((new-table (ht-create)))
    (dolist (table tables)
      (dolist (key table)
        (ht-set new-table key (ht-get table key))))
    new-table))

(defun ht-copy (table)
  "(ht-copy TABLE)

"
  (ht--assert table)
  (ht-merge table))

(defun ht-select (table predicate)
  "(ht-select TABLE PREDICATE)

"
  (ht--assert table)
  (let ((new-table (ht-create)))
    (dolist (key table)
      (let ((value (ht-get table key)))
        (when (predicate key value)
          (ht-set new-table key value))))
    new-table))

(defun ht-reject (table predicate)
  "(ht-reject TABLE PREDICATE)

"
  (ht--assert table)
  (let ((new-table (ht-create)))
    (dolist (key table)
      (let ((value (ht-get table key)))
        (unless (predicate key value)
          (ht-set new-table key value))))
    new-table))


(defun ht-select-keys (table keys)
  "(ht-select-keys TABLE KEYS)

"
  (ht--assert table)
  (let ((new-table (ht-create)))
    (dolist (key keys)
      (let ((value (ht-get table key)))
        (when value (ht-set new-table key value))))
    new-table
    ))


(defun ht-get (table key &optional default)
  "(ht-get TABLE KEY &OPTIONAL DEFAULT)

"
  (ht--assert table)
  (unless (prop-exists table key)
    (return default))
  (prop-get table key))

(defun ht-keys (table)
  "(ht-keys TABLE)

"
  (let ((res (list)))
    (dolist (el table)
      (push res el))
    res))

(defun ht-values (table)
  "(ht-values TABLE)

"
  (ht--assert table)
  (let ((res (list)))
    (dolist (key table)
      (push res (ht-get table key)))
    res
    ))

(defun ht-items (table)
  "(ht-items TABLE)

"
  (ht--assert table)
  (let ((res (list)))
    (dolist (key table)
      (push res (list key (ht-get table key))))
    res
    ))

(defun ht-find (table predicate)
  "(ht-find TABLE PREDICATE)

"
  (dolist (key table)
    (let ((value (ht-get table key)))
      (when (predicate key value)
        (return value)))))

(defun ht-size (table)
  "(ht-size TABLE)

"
  (ht--assert table)
  (length table))


(defun ht-set (table key value)
  "(ht-set TABLE KEY VALUE)

"
  (ht--assert table)
  (unless (prop-exists table key)
    (shove table key))
  (prop-set table key value))

(defun ht-update (table-1 table-2)
  "(ht-update TABLE-1 TABLE-2)

"
  (ht--assert table-1)
  (ht--assert table-2)
  (dolist (key table-2)
    (when (prop-exists table-1 key)
      (ht-set table-1 key (ht-get table-2 key))))
  table-1)

(defun ht-remove (table key)
  "(ht-remove TABLE KEY)

"
  (ht--assert table)
  (when (prop-remove table key)
    (delete table key)))

(defun ht-clear (table)
  "(ht-clear TABLE)

"
  (ht--assert table)
  (dolist (key table)
    (prop-remove table key))
  (clear table)
  table)


(defun ht-map (table fun)
  "(ht-map TABLE FUN)

"
  (ht--assert table)
  (let ((new-table (ht-create)))
    (dolist (key table)
      (ht-set new-table key (fun (ht-get table key))))
    new-table))

(defun ht-each (table fun)
  "(ht-each TABLE FUN)

"
  (dolist (key table)
    (fun key (ht-get table key))))


(defun ht? (table-or-object)
  "(ht? TABLE-OR-OBJECT)

"
  (prop-exists table-or-object "--hash-table--"))

(defun ht-contains? (table key)
  "(ht-contains? TABLE KEY)

"
  (prop-exists table key))

(defun ht-equal? (table-1 table-2)
  "(ht-equal? TABLE-1 TABLE-2)

"
  (unless (ht? table-1) (return nil))
  (unless (ht? table-2) (return nil))
  (when (!= (length table-1) (length table-2)) (return nil))
  (dolist (key table-1)
    (unless (ht-contains? table-1 key) (return nil))
    (unless (equal (ht-get table-1 key) (ht-get table-2 key))
      (return nil)))
  (dolist (key table-2)
    (unless (ht-contains? table-2 key) (return nil))
    (unless (equal (ht-get table-1 key) (ht-get table-2 key))
      (return nil))))

(defun ht-empty? (table)
  "(ht-empty? TABLE)

"
  (== (length table) 0))
