(defvar a "a")
(defvar l '("a" "b") )

(dump `(,@l b ,a))
(dump `(,@a b c))
(dump `((,a) (b ,a (b c d ,a)) c))
(dump `,a)


(defun scope-create-second (num-scopes)
  (let ((local-var 42) size index)
    (dumpcallstack)
    (dumpstack)
    ))

(defun scope-create (num-scopes)
  (let ((local-var 42) size index)
    (let ((local-var (+ local-var 1)) )
      (scope-create-second 12)
      )))


(defvar global-var 10)
(defvar filename --FILE--)
(scope-create global-var)



(dump (head '("a" "b" "c")))

(dump (last '("a" "b" "c")))

(dump (car '("a" "b" "c")))

(dump (cons '("a" "b" "c")))

(dump (tail '("a" "b" "c")))

(dump (init '("a" "b" "c")))

(mapc dump (init '("a" "b" "c")))


(dump (push '("a" "b" "c") "d" ))
(dump (push '("a" "b" "c") 52 ))


(dump (delete '("a" "b" "c") "a" ))
(dump (delete '("a" "b" "c" 123 ) 123))
(dump (delete '(a b c ) 'b))

(dump (remove '("a" "b" "c") "a" ))
(dump (remove '("a" "b" "c" 123 ) 123))
(dump (remove '(a b c ) 'b))


(defun list-example()
  (let (a b (c 10))
    (when 't
      (dump (nth '(0 1 2 3 4 5 6 7) 7)))))

(list-example)

(defun do-it(a)
  (println a)
  (let ((a 42))
    (println a)
    (let ((a (+ a 1)))
      (println a))
    (println a))
  (println a))

(setq a 10)
(println a)
(do-it a)
(println a)



(defvar new-var 10)
(if (== new-var 10)
    (println "The var is 10")
  (println "The var is not 10"))

(setq new-var 12)
(if (== new-var 10)
    (println "The var is 10"))


(setq new-var (funcall (lambda (a) (println a) a) ""))

(defvar new-lam (lambda (a) (println a) a))
(funcall new-lam "a")
(funcall 'new-lam "a")


(mapc print '("a" "b" "c" ))
(mapc print '("H" "e" "l" "l" "o" ))

(defun fun(a)
  (let ((b (+ 1 a)))
    (println b)))
(mapc println '(1 2 3 4 5 6))


(defvar new-var-new "new")
(println new-var-new)

(setq new-var-new "old")
(println new-var-new)


(setq new-var 10)

(when (== new-var 10)
  (println "The var is 10"))

(setq new-var 12)
(when (== new-var 10)
  (println "The var is 10"))

(cond
 ((== 10 11) (println "ten is eleven")) 
 ((== 10 10) (println "ten is ten"))
 ('t (println "final")))

(setq new-var (cond
               ((== 10 11) "eleven") 
               ((== 10 10) "ten")
               ('t "final")))

(println new-var)


;; Pellentesque dapibus suscipit ligula.  Donec posuere augue in quam.  Etiam vel tortor sodales tellus ultricies commodo.  Suspendisse potenti.  Aenean in sem ac leo mollis blandit.  Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi.  Phasellus lacus.  Etiam laoreet quam sed arcu.  Phasellus at dui in ligula mollis ultricies.  Integer placerat tristique nisl.  Praesent augue.  Fusce commodo.  Vestibulum convallis, lorem a tempus semper, dui dui euismod elit, vitae placerat urna tortor vitae lacus.  Nullam libero mauris, consequat quis, varius et, dictum id, arcu.  Mauris mollis tincidunt felis.  Aliquam feugiat tellus ut neque.  Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et dictum nunc justo sit amet elit.


(defvar new-nil-1 'nil)
;; (defvar new-nil-2  nil)

(defvar new-t-1 't)
;; (defvar new-t-2  t)


;; Pellentesque dapibus suscipit ligula.  Donec posuere augue in quam.  Etiam vel tortor sodales tellus ultricies commodo.  Suspendisse potenti.  Aenean in sem ac leo mollis blandit.  Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi.  Phasellus lacus.  Etiam laoreet quam sed arcu.  Phasellus at dui in ligula mollis ultricies.  Integer placerat tristique nisl.  Praesent augue.  Fusce commodo.  Vestibulum convallis, lorem a tempus semper, dui dui euismod elit, vitae placerat urna tortor vitae lacus.  Nullam libero mauris, consequat quis, varius et, dictum id, arcu.  Mauris mollis tincidunt felis.  Aliquam feugiat tellus ut neque.  Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et dictum nunc justo sit amet elit.



(println (+ 10 10))
(println (+ 10 (* 10 10)))
(println (+ 10 (/ 10 10)))
(println (+ 10 (- 10 9)))

(println (+ 10 10.0))
(println (+ 10 (* 10.0 10)))
(println (+ 10 (/ 10 10.0)))
(println (+ 10.0 (- 10 9)))

(println (/ 10 3))
(println (/ 10 3.0))
(println (/ 10.0 3))

(println (when (<= 10 10) "<= 10 10"))
(println (when (< 10 10) "< 10 10"))
(println (when (>= 10 10) ">= 10 10"))
(println (when (> 10 10) "> 10 10"))

(println (when (<= 10 11) "<= 10 11"))
(println (when (< 10 11) "< 10 11"))
(println (when (>= 10 11) ">= 10 11"))
(println (when (> 10 11) "> 10 11"))

(println (when (<= 11 10) "<= 11 10"))
(println (when (< 11 10) "< 11 10"))
(println (when (>= 11 10) ">= 11 10"))
(println (when (> 11 10) "> 11 10"))

(println (when (== 11 10) "== 11 10"))
(println (when (== 10 11) "== 10 11"))
(println (when (== 10 10) "== 10 10"))

(println (when (!= 11 10) "!= 11 10"))
(println (when (!= 10 11) "!= 10 11"))
(println (when (!= 10 10) "!= 10 10"))

(if (psym 'a) "this is sym")

(if (pstring "str") "this is str")

(if (pint 42) "this is int")

(if (preal 3.14) "this is real")

(if (pfunction (lambda (a) (print a))) "this is function")

(if (plist '(a b c)) "this is list")



(println "Hello world")
(println "Hello" " world")
(print "Hel" "lo\n")

(defvar string-var "Hello world")
(println string-var)
(println "This is variable" string-var)

(defvar sym "the value of sym")
(dump 'sym)
(dump sym)
(dump "sym")
(dump '("sym" sym))


(setq new-var 10)
(if (== new-var 10)
    (progn
      (println "The var is 10")
      (println "The var is 10")
      (println "The var is 10")))

'(a b c)
(quote (a b c))

(setq new-var "new")
(println new-var)

(set 'new-var "old")
(println new-var)



(println "This is @ : \u0040")
(println "This is A : \u0041")
(println "This is ! : \041")
(println "This is A : \101")

(println "This is a : \x61")
(println "This is b : \x62")
(println "This is c : \x63")



(setq new-var 10)
(unless (== new-var 10)
  (println "The var is not 10"))

(setq new-var 12)
(unless (== new-var 10)
  (println "The var is not 10"))


(defvar cnt 0)
(while (< cnt 10)
  (println "cnt: " cnt)
  (setq cnt (+ 1 cnt)))

(dolist (element '(1 2 3 4 5 "s"))
  (println "element: " element))



(defmacro custom-when (condition &rest body) 
  `(if ,condition
       (progn ,@body)
     nil))


(defvar new-a 42)
(println "Beginning: " new-a )
(inc new-a)
(1+ new-a)
(println "End: " new-a )

(custom-when t
             (println "1:true!")
             (println "2:true!")
             (println "3:true!"))

(println (parse-int "123"))
(println (parse-int "42"))

(println (parse-float "42.21"))
(println (parse-float "10.45"))

(println (parse-float "42.21"))

(println (to-string "string"))
(println (to-string 10.45))
(println (to-string 10))
(println (to-string 'sym))

(println (to-string println))
(println (to-string ?a))

(println (to-string (to-char 65)))
(println (to-string (to-char 97)))


(println "Starting the stream!")

(let ((s (stream :from-string "")))
  (println "Reading from the stream")
  (stream-write-lines s '("line1" "line2" "line3"))
  (stream-write s "line4")
  (println (stream-content s))
  (stream-close s)
  )


(let ((s (stream :from-string ":")))
  (with-cout s
             (print "line1|")
             (print "line2"))

  (println "Content-> "(stream-content s))

  (stream-close s)
  )
(println "end")


(mapc println (range 1 10))

(dolist (element (range 1 100))
  (println "element: " element))

('println "hello world")
