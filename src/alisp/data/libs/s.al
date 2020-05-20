(import 're)

(defvar --doc-- "S helps you work with strings. This module is entirely based on the [s.el](https://github.com/magnars/s.el) library for emacs lisp. The functions in the module are implemented pureley in alisp.")

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
  "(s-trim S)

Remove whitespace at the beginning and end of s."
  (string-strip s))

(defun s-trim-left (s)
  "(s-trim-left S)

Remove whitespace at the beginning of s."
  (let ((str s)
        (i (string-find s " ")))
    (while  (== 0 i)
      (setq str (string-replace str " " ""))
      (setq i (string-find str " ")))
    str))

(defun s-trim-right (s)
  "(s-trim-right S)

Remove whitespace at the end of s."
  (let ((str s)
        (las (- (string-length s) 1)))
    (while  (== (nth str las) 32)
      (setq las (- las 1)))
    (string-substring str 0 (+ 1 las)))
  
  )

(defun s-chomp (s)
  "(s-chomp S)

Remove whitespace at the end of s."
  (cond
   ((string-endswith s "\r\n") (string-substring s 0 (- (string-length s) 2)))
   ((string-endswith s "\n\r") (string-substring s 0 (- (string-length s) 2)))
   ((string-endswith s "\n") (string-substring s 0 (- (string-length s) 1)))
   ((string-endswith s "\r") (string-substring s 0 (- (string-length s) 1)))
   (t s)))

(defun s-collapse-whitespace (s)
  "(s-collapse-whitespace S)

Convert all adjacent whitespace characters to a single space."
  (re.re-replace "[ \t\n\r]+" s " "))

(defun s-word-wrap (len s)
  "(s-word-wrap LEN S)

If s is longer than len, wrap the words with newlines."
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
  "(s-center LEN S)

If s is shorter than len, pad it with spaces so it is centered."
  (let ((l (string-length s)))
    (if (< l len)
        (string-join (s--times " " (/ (- len l) 2)) s (s--times " " (/ (- len l) 2)))
      s)))

(defun s-pad-left (len padding s)
  "(s-pad-left LEN PADDING S)

If s is shorter than len, pad it with padding on the left."
  (string-join (s--times padding len) s))

(defun s-pad-right (len padding s)
  "(s-pad-right LEN PADDING S)

If s is shorter than len, pad it with padding on the right."
  (string-join s (s--times padding len)))

(defun s-truncate (len s)
  "(s-truncate LEN S)

If s is longer than len, cut it down to len - 3 and add ... at the end."
  (if (> (string-length s) len)
      (string-join (string-substring s 0 (- len 3)) "...")
    s))

(defun s-left (len s)
  "(s-left LEN S)

Returns up to the len first chars of s."
  (string-substring s 0 len))

(defun s-right (len s)
  "(s-right LEN S)

Returns up to the len last chars of s."
  (if (>= (string-length s) len )
      (string-substring s (- (string-length s) len) (string-length s))
    s))

(defun s-chop-suffix (suffix s)
  "(s-chop-suffix SUFFIX S)

Remove suffix if it is at end of s."
  (if (string-endswith s suffix)
      (string-substring s 0 (- (string-length s) (string-length suffix)))
    s))

(defun s-chop-suffixes (suffixes s)
  "(s-chop-suffixes SUFFIXES S)

Remove suffixes one by one in order, if they are at the end of s."
  (let ((res s))
    (dolist (suf suffixes)
      (setq res (s-chop-suffix suf res)))
    res))

(defun s-chop-prefix (prefix s)
  "(s-chop-prefix PREFIX S)

Remove prefix if it is at the start of s."
  (if (string-startswith s prefix)
      (string-substring s (string-length prefix) (string-length s) )
    s))

(defun s-chop-prefixes (prefixes s)
  "(s-chop-prefixes PREFIXES S)

Remove prefixes one by one in order, if they are at the start of s."
  (let ((res s))
    (dolist (pref prefixes)
      (setq res (s-chop-prefix pref res)))
    res))

(defun s-shared-start (s1 s2)
  "(s-shared-start S1 S2)

Returns the longest prefix s1 and s2 have in common."
  (let ((search-length (min (string-length s1) (string-length s2)))
        (i 0))
    (while (and (< i search-length)
                (== (nth s1 i) (nth s2 i)))
      (setq i (+ i 1)))
    (string-substring s1 0 i)))

(defun s-shared-end (s1 s2)
  "(s-shared-end S1 S2)

Returns the longest suffix s1 and s2 have in common."
  (let* ((l1 (string-length s1))
         (l2 (string-length s2))
         (search-length (min l1 l2))
         (i 0))
    (while (and (< i search-length)
                (== (nth s1 (- l1 i 1)) (nth s2 (- l2 i 1))))
      (setq i (+ i 1)))
    (if (== 0 i) "" (string-substring s1 (- i 1) (string-length s1)))))

(defun s-repeat (num s)
  "(s-repeat NUM S)

Make a string of s repeated num times."
  (s--times s num))

(defun s-concat (&rest strings)
  "(s-concat &REST STRINGS)

Join all the string arguments into one string."
  (let ((res ""))
    (dolist (r strings)
      (setq res (string-join res r)))))

(defun s-prepend (prefix s)
  "(s-prepend PREFIX S)

Concatenate prefix and s."
  (string-join prefix s))

(defun s-append (suffix s)
  "(s-append SUFFIX S)

Concatenate s and suffix."
  (string-join s suffix))

(defun s-lines (s)
  "(s-lines S)

Splits s into a list of strings on newline characters."
  (string-splitlines s))

(defun s-match (regexp s &optional start)
  "(s-match REGEXP S &OPTIONAL START)

When the given expression matches the string, this function returns a list of the whole matching string and a string for each matched subexpressions. If it did not match the returned value is an empty list (nil).

When start is non-nil the search will start at that index."
  (if start
      (re.re-search regexp (string-substring s start (string-length s)))
    (re.re-search regexp s)))

(defun s-match-strings-all (regex string)
  "(s-match-strings-all REGEX STRING)

Return a list of matches for regex in string.

Each element itself is a list of matches, as per match-string. Multiple matches at the same position will be ignored after the first."
  (if start
      (re.re-search-all regexp (string-substring s start (string-length s)))
    (re.re-search-all regexp s)))

(defun s-slice-at (regexp s)
  "(s-slice-at REGEXP S)

Slices s up at every index matching regexp."
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
  "(s-split SEPARATOR S &OPTIONAL OMIT-NULLS)

Split s into substrings bounded by matches for regexp separator. If omit-nulls is non-nil, zero-length substrings are omitted."
  (if omit-nulls
      (progn
        (filter (lambda (x) (!= (string-length x) 0)) (string-split s separator)))
    (string-split s separator)))

(defun list-concat (l1 l2)
  ((list-concat L1 L2)

                dolist (el l2)
    (push l1 el))
  l1)

(defun s-split-up-to (separator s n &optional omit-nulls)
  "(s-split-up-to SEPARATOR S N &OPTIONAL OMIT-NULLS)

Split s up to n times into substrings bounded by matches for regexp separator.

If omit-nulls is non-nil, zero-length substrings are omitted."
  (let ((splits (s-split separator s omit-nulls))
        (res (list)))
    (dolist (j (range 0 n))
      (push res (nth splits 0))
      (delete splits (nth splits 0)))
    (push res (s--join-with-sep splits separator))
    res
    ))

(defun s-join (separator strings)
  "(s-join SEPARATOR STRINGS)

Join all the strings in strings with separator in between."
  (s--join-with-sep strings separator))

(defun s-equals? (s1 s2)
  "(s-equals? S1 S2)

Is s1 equal to s2?"
  (string-equals s1 s2))

(defun s-less? (s1 s2)
  "(s-less? S1 S2)

Is s1 less than s2?"
  (string-less s1 s2))

(defun s-matches? (regexp s &optional start)
  "(s-matches? REGEXP S &OPTIONAL START)

Does regexp match s? If start is non-nil the search starts at that index."
  (if start
      (if (re.re-match regexp (string-substring s start (string-length s))) 't 'nil)
    (if (re.re-match regexp s) 't 'nil)))

(defun s-blank? (s)
  "(s-blank? S)

Is s nil or the empty string?"
  (if (or (eq s nil) (== (string-length s) 0)) 't 'nil))

(defun s-present? (s)
  "(s-present? S)

Is s anything but nil or the empty string?"
  (not (s-blank? s)))

(defun s-ends-with? (suffix s &optional ignore-case)
  "(s-ends-with? SUFFIX S &OPTIONAL IGNORE-CASE)

Does s end with suffix?

If ignore-case is non-nil, the comparison is done without paying attention to case differences."
  (if ignore-case
      (string-endswith (string-lower s) (string-lower suffix))
    (string-endswith s suffix)))

(defun s-starts-with? (prefix s &optional ignore-case)
  "(s-starts-with? PREFIX S &OPTIONAL IGNORE-CASE)

Does s start with prefix?

If ignore-case is non-nil, the comparison is done without paying attention to case differences."
  (if ignore-case
      (string-startswith (string-lower s) (string-lower prefix))
    (string-endswith s prefix)))

(defun s-contains? (needle s &optional ignore-case)
  "(s-contains? NEEDLE S &OPTIONAL IGNORE-CASE)

Does s contain needle?

If ignore-case is non-nil, the comparison is done without paying attention to case differences."
  (if ignore-case
      (string-contains (string-lower s) (string-lower needle))
    (string-contains s needle)))

(defun s-lowercase? (s)
  "(s-lowercase? S)

Are all the letters in s in lower case?"
  (if (re.re-match "[[:lower:]]*" s) 't 'nil))

(defun s-uppercase? (s)
  "(s-uppercase? S)

Are all the letters in s in upper case?"
  (if (re.re-match "[[:upper:]]*" s) 't 'nil))

(defun s-mixedcase? (s)
  "(s-mixedcase? S)

Are there both lower case and upper case letters in s?"
  (and (re.re-search "[[:upper:]]+" s)
       (re.re-search "[[:lower:]]+" s)))

(defun s-capitalized? (s)
  "(s-capitalized? S)

In s, is the first letter upper case, and all other letters lower case?"
  (if (re.re-match "^[[:upper:]][^[:upper:]]*$" s) 't 'nil))

(defun s-numeric? (s)
  "(s-numeric? S)

Is s a number?"
  (if (re.re-match "^[0-9]+$" s) 't 'nil))

(defun s-replace (old new s)
  "(s-replace OLD NEW S)

Replaces old with new in s."
  (string-replace s old new ))

(defun s-replace-all (replacements s)
  "(s-replace-all REPLACEMENTS S)

replacements is a list of cons-cells. Each car is replaced with cdr in s."
  (string-replace-all old new s))

(defun s-downcase (s)
  "(s-downcase S)

Convert s to lower case."
  (string-lower s))

(defun s-upcase (s)
  "(s-upcase S)

Convert s to upper case."
  (string-uppe s))

(defun s-capitalize (s)
  "(s-capitalize S)

Convert the first word's first character to upper case and the rest to lower case in s."
  (string-join (string-upper (string-substring s 0 1)) (string-lower (string-substring s 1 (string-length s)))))

(defun s-titleize (s)
  "(s-titleize S)

Convert each word's first character to upper case and the rest to lower case in s."
  (string-capitalize s))

(defmacro s-with (s form &rest more)
  "Threads s through the forms. Inserts s as the last item in the first form, making a list of it if it is not a list already. If there are more forms, inserts the first form as the last item in second form, etc."
  (if (not more)
      (if (plist form)
          `(,(car form) ,@(tail form) ,s)
        (list form s))
    `(s-with (s-with ,s ,form) ,@more))
  )

(defun s-index-of (needle s &optional ignore-case)
  "(s-index-of NEEDLE S &OPTIONAL IGNORE-CASE)

Returns first index of needle in s, or nil."
  (if ignore-case
      (find (string-lower s) (string-lower needle))
    (find s needle)))

(defun s-reverse (s)
  "(s-reverse S)

Return the reverse of s."
  (string-reverse s))

(defun s-presence (s)
  "(s-presence S)

Return s if it's s-present?, otherwise return nil."
  (not s-blank? s))

(defun s-count-matches (regexp s &optional start end)
  "(s-count-matches REGEXP S &OPTIONAL START END)

Count occurrences of regexp in `s'.

start, inclusive, and end, exclusive, delimit the part of s to match."
  (let* ((beg (if start start 0))
         (end (if end end (string-length s)))
         (str (string-substring s beg end)))
    (length (re.re-search-all regexp str))))

(defun s-wrap (s prefix &optional suffix)
  "(s-wrap S PREFIX &OPTIONAL SUFFIX)

Wrap string s with prefix and optionally suffix.

Return string s with prefix prepended. If suffix is present, it is appended, otherwise prefix is used as both prefix and suffix."
  (if suffix
      (string-join prefix s suffix)
    (string-join prefix s prefix)))

(defun s-split-words (s)
  "(s-split-words S)

Split s into list of words."
  (string-split s " "))

(defun s-upper-camel-case (s)
  "(s-upper-camel-case S)

Convert s to UpperCamelCase."
  (let ((words (s-split-words s)))
    (s-join "" (mapcar string-capizalize words))))

(defun s-snake-case (s)
  "(s-snake-case S)

Convert s to snake_case."
  (let ((words (s-split-words s)))
    (s-join "_" (mapcar string-lower words))))

(defun s-dashed-words (s)
  "(s-dashed-words S)

Convert s to dashed-words."
  (let ((words (s-split-words s)))
    (s-join "-" (mapcar string-lower words))))

(defun s-capitalized-words (s)
  "(s-capitalized-words S)

Convert s to Capitalized words."
  (let ((words (s-split-words s)))
    (s-join " " (mapcar s-capitalize words))))

(defun s-titleized-words (s)
  "(s-titleized-words S)

Convert s to Titleized Words."
  (let ((words (s-split-words s)))
    (s-join " " (mapcar s-titleize words))))

(defun s-word-initials (s)
  "(s-word-initials S)

Convert s to its initials."
  (s-join "" (mapcar (lambda (ss) (string-substring ss 0 1)) (s-split-words s))))


