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
