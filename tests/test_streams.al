(import 'assert-utils :all)
(import 'asserts :all)

(let ((s (stream :from-string "")))
  (stream-write-lines s '("a" "b" "c"))
  (stream-write s "d")
  (assert-equal "a\nb\nc\nd" (stream-content s))
  (stream-close s))

(let ((s (stream :from-string "")))
  (with-cout s
             (print "a")
             (print "b"))
  (assert-equal "ab"(stream-content s))
  (stream-close s))

(let ((s (stream :from-string "")))
  (with-cout s
             (println "a")
             (println "b"))
  (assert-equal "a\nb\n"(stream-content s))
  (stream-close s))

(let ((s (stream :from-string "abc")))
  (with-cin s
            (assert-equal ?a (read-char))
            (assert-equal ?b (read-char))
            (assert-equal ?c (read-char)))
  (stream-close s))

(let ((s (stream :from-string "abc")))
  (with-cin s
            (assert-equal "ab" (read-chars 2)))
  (stream-close s))

(let ((s (stream :from-string "abc\ncba")))
  (with-cin s
            (assert-equal "abc" (read-line))
            (assert-equal "cba" (read-line)))
  (stream-close s))


