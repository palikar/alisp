(defvar --doc-- "Anaphoric expressions for, providing implicit temporary variables.")

(defvar eps 0.00001)

(defun assert-== (a b)
  (assert (== a b)))

(defun assert-!= (a b)
  (assert (!= a b)))

(defun assert-equal (a b)
  (assert (equal a b)))

(defun assert-not-equal (a b)
  (assert-not (equal a b)))

(defun assert-eq (a b)
  (assert (eq a b)))

(defun assert-not-eq (a b)
  (assert-not (eq a b)))

(defun assert-list-contain (lis el)
  (assert (contains lis el)))

(defun assert-list-not-contain (lis el)
  (assert (not (contains lis el))))

(defun assert-nil (a)
  (assert (equal a nil)))

(defun assert-non-nil (a)
  (assert (not (equal a nil))))

(defun assert-almost-equal (a b &optional eps)
  (let ((ep (if eps eps --eps--)))
    (assert (< (- a b) ep))))

(defun assert-not-almost-equal (a b &optional eps)
  (let ((ep (if eps eps --eps--)))
    (assert (>= (- a b) ep))))

(defun assert-less-equal (a b)
  (assert (<= a b)))

(defun assert-less (a b)
  (assert (< a b)))

(defun assert-greater (a b)
  (assert (> a b)))

(defun assert-greater-equal (a b)
  (assert (>= a b)))

(defun assert-matches (str part)
  (string-contains str part))

(defun assert-has-property (el prop)
  (assert prop-exist el prop))

(defmacro assert-throws (sig &rest body)
  `(condition-case nil
       ,@body
     (assert nil)
     (sig (assert t))))
