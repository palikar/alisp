(import 're)

(defun s--mapcar-head (fn-head fn-rest l)
  "Like MAPCAR, but applies a different function to the first element."
  (if l
      (list (funcall fn-head (head l)) (mapcar fn-rest (tail list)))))

(defun s--times (s times)
  (let ((str ""))
    (dotimes (i times)
      (setq str (string-join str s)))
    str))

(defun s--join-with-sep (strs s)
  (let ((res ""))
    (dolist (str strs)
      (setq res (string-join res str s)))
    (string-substring res 0 (- (string-length res) (string-length s)))
    ))

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

(defun s-chomp (s)
  (cond
   ((string-endswith s "\r\n") (string-substring s 0 (- (string-length s) 2)))
   ((string-endswith s "\n\r") (string-substring s 0 (- (string-length s) 2)))
   ((string-endswith s "\n") (string-substring s 0 (- (string-length s) 1)))
   ((string-endswith s "\r") (string-substring s 0 (- (string-length s) 1)))
   (t s)))

(defun s-collapse-whitespace (s)
  (re.re-replace "[ \t\n\r]+" s " "))

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

(defun s-center (len s)
  (let ((l (string-length s)))
    (if (< l len)
        (string-join (s--times " " (/ (- len l) 2)) s (s--times " " (/ (- len l) 2)))
      s)))

(defun s-pad-left (len padding s)
  (string-join (s--times padding len) s))

(defun s-pad-right (len padding s)
  (string-join s (s--times padding len)))

(defun s-truncate (len s)
  (if (> (string-length s) len)
      (string-join (string-substring s 0 (- len 3)) "...")
    s))

(defun s-left (len s)
  (string-substring s 0 len))

(defun s-right (len s)
  (if (>= (string-length s) len )
      (string-substring s (- (string-length s) len) (string-length s))
    s))

(defun s-chop-suffix (suffix s)
  (if (string-endswith s suffix)
      (string-substring s 0 (- (string-length s) (string-length suffix)))
    s))

(defun s-chop-suffixes (suffixes s)
  (let ((res s))
    (dolist (suf suffixes)
      (setq res (s-chop-suffix suf res)))
    res))

(defun s-chop-prefix (prefix s)
  (if (string-startswith s prefix)
      (string-substring s (string-length prefix) (string-length s) )
    s))

(defun s-chop-prefixes (prefixes s)
  (let ((res s))
    (dolist (pref prefixes)
      (setq res (s-chop-prefix pref res)))
    res))

(defun s-shared-start (s1 s2)
  (let ((search-length (min (string-length s1) (string-length s2)))
        (i 0))
    (while (and (< i search-length)
                (== (nth s1 i) (nth s2 i)))
      (setq i (+ i 1)))
    (string-substring s1 0 i)))

(defun s-shared-end (s1 s2)
  (let* ((l1 (string-length s1))
         (l2 (string-length s2))
         (search-length (min l1 l2))
         (i 0))
    (while (and (< i search-length)
                (== (nth s1 (- l1 i 1)) (nth s2 (- l2 i 1))))
      (setq i (+ i 1)))
    (if (== 0 i) "" (string-substring s1 (- i 1) (string-length s1)))))

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

(defun s-split (separator s &optional omit-nulls)
  (if omit-nulls
      (progn
        (filter (lambda (x) (!= (string-length x) 0)) (string-split s separator)))
    (string-split s separator)))

(defun list-concat (l1 l2)
  (dolist (el l2)
    (push l1 el))
  l1)

(defun s-split-up-to (separator s n &optional omit-nulls)
  (let ((splits (s-split separator s omit-nulls))
        (res (list)))
    (dolist (j (range 0 n))
      (push res (nth splits 0))
      (delete splits (nth splits 0)))
    (push res (s--join-with-sep splits separator))
    res
    ))

(defun s-join (separator strings)
  (s--join-with-sep strings separator))

(defun s-equals? (s1 s2)
  (string-equals s1 s2))

(defun s-less? (s1 s2)
  (string-less s1 s2))

(defun s-matches? (regexp s &optional start)
  (if start
      (if (re.re-match regexp (string-substring s start (string-length s))) 't 'nil)
    (if (re.re-match regexp s) 't 'nil)))

(defun s-blank? (s)
  (if (or (eq s nil) (== (string-length s) 0)) 't 'nil))

(defun s-present? (s)
  (not (s-blank? s)))

(defun s-ends-with? (suffix s &optional ignore-case)
  (if ignore-case
      (string-endswith (string-lower s) (string-lower suffix))
    (string-endswith s suffix)))

(defun s-starts-with? (prefix s &optional ignore-case)
  (if ignore-case
      (string-startswith (string-lower s) (string-lower prefix))
    (string-endswith s prefix)))

(defun s-contains? (needle s &optional ignore-case)
  (if ignore-case
      (string-contains (string-lower s) (string-lower needle))
    (string-contains s needle)))

(defun s-lowercase? (s)
  (if (re.re-match "[[:lower:]]*" s) 't 'nil))

(defun s-uppercase? (s)
  (if (re.re-match "[[:upper:]]*" s) 't 'nil))

(defun s-mixedcase? (s)
  (and (re.re-search "[[:upper:]]+" s)
       (re.re-search "[[:lower:]]+" s)))

(defun s-capitalized? (s)
  (if (re.re-match "^[[:upper:]][^[:upper:]]*$" s) 't 'nil))

(defun s-numeric? (s)
  (if (re.re-match "^[0-9]+$" s) 't 'nil))

(defun s-replace (old new s)
  (string-replace s old new ))

(defun s-replace-all (replacements s)
  (string-replace-all old new s))

(defun s-downcase (s)
  (string-lower s))

(defun s-upcase (s)
  (string-uppe s))

(defun s-capitalize (s)
  (string-join (string-upper (string-substring s 0 1)) (string-lower (string-substring s 1 (string-length s)))))

(defun s-titleize (s)
  (string-capitalize s))

(defmacro s-with (s form &rest more)
  (if (not more)
      (if (plist form)
          `(,(car form) ,@(tail form) ,s)
        (list form s))
    `(s-with (s-with ,s ,form) ,@more))
  )

(defun s-index-of (needle s &optional ignore-case)
  (if ignore-case
      (find (string-lower s) (string-lower needle))
    (find s needle)))

(defun s-reverse (s)
  (string-reverse s))

(defun s-presence (s)
  (not s-blank? s))

(defun s-count-matches (regexp s &optional start end)
  (let* ((beg (if start start 0))
         (end (if end end (string-length s)))
         (str (string-substring s beg end)))
    (length (re.re-search-all regexp str))))

(defun s-wrap (s prefix &optional suffix)
  (if suffix
      (string-join prefix s suffix)
    (string-join prefix s prefix)))

(defun s-split-words (s)
  (string-split s " "))

(defun s-lower-camel-case (s)
  
  )

(defun s-upper-camel-case (s)
  (let ((words (s-split-words s)))
    (s-join "" (mapcar string-capizalize words))))

(defun s-snake-case (s)
  (let ((words (s-split-words s)))
    (s-join "_" (mapcar string-lower words))))

(defun s-dashed-words (s)
  (let ((words (s-split-words s)))
    (s-join "-" (mapcar string-lower words))))

(defun s-capitalized-words (s)
  (let ((words (s-split-words s)))
    (s-join " " (mapcar s-capitalize words))))

(defun s-titleized-words (s)
  (let ((words (s-split-words s)))
    (s-join " " (mapcar s-titleize words))))

(defun s-word-initials (s)
  (s-join "" (mapcar (lambda (ss) (string-substring ss 0 1)) (s-split-words s))))


