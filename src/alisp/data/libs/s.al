;; (import 's :all)
(import 're)


(defun s--times (s times)
  (let ((str ""))
    (dotimes (i times)
      (setq str (string-join str s)))
    str))

(defun s-trim (s)
  (string-strip s))

(defun s-trim-left (s)
  (let ((str s)
        (i (string-find s " ")))
    (while  (== 0 i)
      (setq str (string-replace str " " ""))
      (setq i (string-find str " ")))
    str))

(defun s-trim-right (s)
  (let ((str s)
        (las (- (string-length s) 1)))
    (while  (== (nth str las) 32)
      (setq las (- las 1)))
    (string-substring str 0 (+ 1 las)))
  
  )

;; (dump (s-trim " space  "))
;; (dump (s-trim-left " space  "))
;; (dump (s-trim-right " space  "))


(defun s-chomp (s)
  (cond
   ((string-endswith s "\r\n") (string-substring s 0 (- (string-length s) 2)))
   ((string-endswith s "\n\r") (string-substring s 0 (- (string-length s) 2)))
   ((string-endswith s "\n") (string-substring s 0 (- (string-length s) 1)))
   ((string-endswith s "\r") (string-substring s 0 (- (string-length s) 1)))
   (t s)))

;; (dump (s-chomp "new\n"))
;; (dump (s-chomp "new\r"))
;; (dump (s-chomp "new\r\n"))

(defun s-collapse-whitespace (s)
  (re.re-replace "[ \t\n\r]+" s " "))

;; (dump (s-collapse-whitespace "only   one space   please"))

(defun s-word-wrap (len s)
  (let ((words (string-split s " "))
        (res "")
        (i 0))
    (dolist (w words)
      (setq i (+ i (string-length w)))
      (setq res (string-join res w " "))
      (when (>= i len)
        (setq res (string-join res "\n"))
        (setq i 0)))
    res))

;; (dump (s-word-wrap 10 "This is too long"))
;; (dump (s-word-wrap 10 "This is way way too long"))
;; (dump (s-word-wrap 10 "It-wraps-words-but-does-not-break-them"))

(defun s-center (len s)
  (let ((l (string-length s)))
    (if (< l len)
        (string-join (s--times " " (/ (- len l) 2)) s (s--times " " (/ (- len l) 2)))
      s)))

;; (dump (s-center 5 "ab"))

(defun s-pad-left (len padding s)
  (string-join (s--times padding len) s))
(defun s-pad-right (len padding s)
  (string-join s (s--times padding len)))

;; (dump (s-pad-left 3 "0" "3"))
;; (dump (s-pad-left 3 "0" "23"))
;; (dump (s-pad-left 3 "0" "1234"))
;; (dump (s-pad-right 3 "." "3") )
;; (dump (s-pad-right 3 "." "23"))
;; (dump (s-pad-right 3 "." "1234"))

(defun s-truncate (len s)
  (if (> (string-length s) len)
      (string-join (string-substring s 0 (- len 3)) "...")
    s))


;; (dump (s-truncate 6 "This is too long"))
;; (dump (s-truncate 16 "This is also too long this is also too long "))
;; (dump (s-truncate 16 "But this is not!"))

(defun s-left (len s)
  (string-substring s 0 len))

;; (dump (s-left 3 "lib/file.js"))
;; (dump (s-left 3 "li"))

(defun s-right (len s)
  (if (>= (string-length s) len )
      (string-substring s (- (string-length s) len) (string-length s))
    s))

;; (dump (s-right 3 "lib/file.js"))
;; (dump (s-right 3 "li"))

(defun s-chop-suffix (suffix s)
  (if (string-endswith s suffix)
      (string-substring s 0 (- (string-length s) (string-length suffix)))
    s))

;; (dump (s-chop-suffix "-test.js" "penguin-test.js"))
;; (dump (s-chop-suffix "\n" "no newlines\n"))
;; (dump (s-chop-suffix "\n" "some newlines\n\n"))


(defun s-chop-suffixes (suffixes s)
  (let ((res s))
    (dolist (suf suffixes)
      (setq res (s-chop-suffix suf res)))
    res))

;; (dump (s-chop-suffixes '("_test.js" "-test.js" "Test.js") "penguin-test.js"))
;; (dump (s-chop-suffixes '("\r" "\n") "penguin\r\n"))
;; (dump (s-chop-suffixes '("\n" "\r") "penguin\r\n"))

(defun s-chop-prefix (prefix s)
  (if (string-startswith s prefix)
      (string-substring s (string-length prefix) (string-length s) )
    s))

;; (dump (s-chop-prefix "/tmp" "/tmp/file.js"))
;; (dump (s-chop-prefix "/tmp" "/tmp/tmp/file.js"))

(defun s-chop-prefixes (prefixes s)
  (let ((res s))
    (dolist (pref suffixes)
      (setq res (s-chop-pref pref res)))
    res))

;; (dump (s-chop-prefixes '("/tmp" "/my") "/tmp/my/file.js"))
;; (dump (s-chop-prefixes '("/my" "/tmp") "/tmp/my/file.js"))

(defun s-shared-start (s1 s2)
  (let ((search-length (min (string-length s1) (string-length s2)))
        (i 0))
    (while (and (< i search-length)
                (== (nth s1 i) (nth s2 i)))
      (setq i (+ i 1)))
    (string-substring s1 0 i)))

;; (dump (s-shared-start "bar" "baz"))
;; (dump (s-shared-start "foobar" "foo"))
;; (dump (s-shared-start "bar" "foo"))


(defun s-shared-end (s1 s2)
  (let* ((l1 (string-length s1))
         (l2 (string-length s2))
         (search-length (min l1 l2))
         (i 0))
    (while (and (< i search-length)
                (== (nth s1 (- l1 i 1)) (nth s2 (- l2 i 1))))
      (setq i (+ i 1)))
    (if (== 0 i) "" (string-substring s1 (- i)))))

;; (dump (s-shared-end "bar" "var"))
;; (dump (s-shared-end "foo" "foo"))
;; (dump (s-shared-end "bar" "foo"))

(defun s-repeat (num s)
  (s--times s num))

(defun s-concat (&rest strings)
  (let ((res ""))
    (dolist (r strings)
      (setq res (string-join res r)))))

(defun s-prepend (prefix s)
  (string-join prefix s))

(defun s-append (suffix s)
  (string-join s suffix))


(defun s-lines (s)
  (string-splitlines s))

(defun s-match (regexp s &optional start)
  (if start
      (re.re-search regexp (string-substring s start (string-length s)))
    (re.re-search regexp s)))

;; (dump (s-match "^def" "abcdefg"))
;; (dump (s-match "^abc" "abcdefg"))
;; (dump (s-match "/.*/([a-z]+)\\.([a-z]+)" "/some/weird/file.html"))

(defun s-match-strings-all (regex string)
  (if start
      (re.re-search-all regexp (string-substring s start (string-length s)))
    (re.re-search-all regexp s)))

(defun s-slice-at (regexp s)
  (let ((str s)
        (matches (re.re-search-all regexp s))
        (ind 0)
        (res (list ))
        prev)
    (if matches
        (progn
          (dolist (m matches)
            (let* ((m-0 (nth m 0))
                   (i (string-find (if prev (string-substring str (string-length prev) (string-length str)) str) m-0)))
              (push res (string-substring str 0 (+ i (if prev (string-length prev) 0))))
              (setq str (string-substring str (+ i (if prev (string-length prev) 0)) (string-length str)))
              (setq ind i)
              (setq prev m-0)
              ))
          (push res str)
          res)
      `(,s))))

;; (dump (s-slice-at "-" "abc"))
;; (dump (s-slice-at "-" "abc-def"))
;; (dump (s-slice-at "[.#]" "abc.def.ghi#id"))

(defun s-split (separator s &optional omit-nulls)
  (if omit-nulls
      (progn
        (filter (lambda (x) (!= (string-length x) 0)) (string-split s separator)))
    (string-split s separator)))

;; (dump (s-split "|" "a|bc|12|3"))
;; (dump (s-split ":" "a,c,d"))
;; (dump (s-split "," "a,c,d"))
;; (dump (s-split "\n" "z\nefg\n"))
;; (dump (s-split "\n" "z\nefg\n" t))

(defun list-concat (l1 l2)
  (dolist (el l2)
    (push l1 el))
  l1)

(defun s--join-with-sep (strs s)
  (let ((res ""))
    (dolist (str strs)
      (setq res (string-join res str s)))
    (string-substring res 0 (- (string-length res) (string-length s)))
    ))

(defun s-split-up-to (separator s n &optional omit-nulls)
  (let ((splits (s-split separator s omit-nulls))
        (res (list)))
    (dolist (j (range 0 n))
      (push res (nth splits 0))
      (delete splits (nth splits 0)))
    (push res (s--join-with-sep splits separator))
    res
    ))

;; (dump (s-split-up-to "-" "Author-Track-number-one" 1)) ;; => '("Author" "Track-number-one")
;; (dump (s-split-up-to "-" "Author-Track-number-one" 2)) ;; => '("Author" "Track" "number-one"))
;; (dump (s-split-up-to "|" "foo||bar|baz|qux" 3  t))                  ;; => '("foo" "bar" "baz|qux")


(defun s-join (separator strings))

(defun s-equals? (s1 s2))
(defun s-less? (s1 s2))
(defun s-matches? (regexp s &optional start))
(defun s-blank? (s))
(defun s-present? (s))
(defun s-ends-with? (suffix s &optional ignore-case))
(defun s-starts-with? (prefix s &optional ignore-case))
(defun s-contains? (needle s &optional ignore-case))
(defun s-lowercase? (s))
(defun s-uppercase? (s))
(defun s-mixedcase? (s))
(defun s-capitalized? (s))
(defun s-numeric? (s))

(defun s-replace (old new s))
(defun s-replace-all (replacements s))
(defun s-downcase (s))
(defun s-upcase (s))
(defun s-capitalize (s))
(defun s-titleize (s))
(defun s-with (s form &rest more))
(defun s-index-of (needle s &optional ignore-case))
(defun s-reverse (s))
(defun s-presence (s))
(defun s-format (template replacer &optional extra))
(defun s-lex-format (format-str))
(defun s-count-matches (regexp s &optional start end))
(defun s-wrap (s prefix &optional suffix))

(defun s-split-words (s))
(defun s-lower-camel-case (s))
(defun s-upper-camel-case (s))
(defun s-snake-case (s))
(defun s-dashed-words (s))
(defun s-capitalized-words (s))
(defun s-titleized-words (s))
(defun s-word-initials (s))



