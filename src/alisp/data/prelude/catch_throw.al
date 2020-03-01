(defmacro catch (sym &rest body)
  `(condition-case nil
       (progn ,@body)
     (,sym )))


(defmacro throw (sy)
  `(signal ,sy '()))
