(defmacro inc (var)
  `(setq ,var (+ ,var 1)))

(defmacro 1+ (var)
  `(progn
     (inc ,var)
     (inc ,var)
     ))

(defmacro custom-when (condition &rest body) 
  `(if ,condition
       (progn ,@body)
     nil))


(defvar a 42)
(println "Beginning: " a )
(inc a)
(1+ a)
(println "End: " a )

(custom-when t
             (println "1:true!")
             (println "2:true!")
             (println "3:true!"))
