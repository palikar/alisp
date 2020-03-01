(condition-case nil
    (progn
      (defun fun (a b (c))))
  ('defun-signal (assert 't)))

(condition-case nil
    (progn
      (defun fun (a b &rest b &optional) ))
  ('defun-signal (assert 't)))

(condition-case nil
    (progn
      (defun fun (a b b &optional b &optional c &rest c) ))
  ('defun-signal (assert 't)))

(condition-case nil
    (progn
      (defun fun (a b b &optional b &rest c &rest c) ))
  ('defun-signal (assert 't)))

(condition-case nil
    (progn
      (defun fun (a b "c") ))
  ('defun-signal (assert 't)))

