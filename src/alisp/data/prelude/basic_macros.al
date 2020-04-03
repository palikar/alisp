(defun inc (var)
  (+ var 1))

(defun dec (var)
  (- var 1))

(defmacro 1+ (var)
  `(setq ,var (+ ,var 1)))

(defmacro 1- (var)
  `(setq ,var (- ,var 1)))


(defmacro std-redirect (file-str &rest body)
  `(let* ((file (file-open ,file-str))
          (str (stream :from-file file)))
     (with-cout str ,@body)
     (stream-close str)
     (file-close file)))

(defun even? (arg)
  (== 0 (mod arg 2)))

(defun odd? (arg)
  (!= 0 (mod arg 2)))

(defun square (arg)
  (* arg arg))
