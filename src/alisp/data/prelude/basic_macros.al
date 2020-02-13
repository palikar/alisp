(defmacro inc (var)
  `(setq ,var (+ ,var 1)))

(defmacro dec (var)
  `(setq ,var (- ,var 1)))

(defmacro 1+ (var)
  `(setq ,var (+ ,var 1)))

(defmacro 1- (var)
  `(setq ,var (- ,var 1)))

(defmacro 2+ (var)
  `(setq ,var (+ ,var 2)))

(defmacro 2- (var)
  `(setq ,var (- ,var 2)))

(defmacro std-redirect (file-str &rest body)
  `(let* ((file (file-open ,file-str))
          (str (stream :from-file file)))
     (with-cout str ,@body)
     (stream-close str)
     (file-close file)))


;; (defmacro std-redirect (file &rest body)
;;   `(let* ((file (file-open file :out))
;;           (str (stream :from-file file)))
;;      (with-cout str ,@body)
;;      (stream-close str)
;;      (file-close file)))
