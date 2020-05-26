(import 'anaphora :all)

(anaphoric-if (+ 10 42)
    (println it))

(anaphoric-prog1 (+ 10 42)
  (println it))

(anaphoric-prog2 (+ 10 43) (+ 10 43)
  (println it))

(anaphoric-when (+ 10 42)
  (println it))

(anaphoric-cond
 ((+ 10 13)  (println "val: " it))
 ((+ 10 12)  (println "val: " it))
 ((+ 10 10)  (println "val: " it)))

(anaphoric-let (+ 10 10)
  (println it))

(dump (anaphoric-and 't it nil))

(dump (anaphoric-+ 1 10 (* it 3)))

(dump (anaphoric-- 1 10 (* it 3)))

(dump (anaphoric-* 1 10 (* it 3)))

(dump (anaphoric-/ 1.0 10 (* it 3)))
