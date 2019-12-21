(defun scope-create-second (num-scopes)
  (let ((local-var 42) size index)
    (dumpcallstack)))

(defun scope-create (num-scopes)
  (let ((local-var 42) size index)
    (let ((local-var (+ local-var 1)) )
      (scope-create-second 12)
      )))


(defvar global-var 10)
(defvar filename --FILE--)
(scope-create global-var)
