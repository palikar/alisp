;; (assert-not (defun fun-1 (a b (c)) ) )
;; (assert-not (defun fun-2 (a b &rest b &optional) ) )
;; (assert-not (defun fun-3 (a b b &optional b &optional c &rest c) ) )
;; (assert-not (defun fun-4 (a b b &optional b &rest c &rest c) ) )
;; (assert-not (defun fun-5 (a b "c") ) )

;; (assert 'nil)
