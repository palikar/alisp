(println (string-upper "uppper"))
(println (string-lower "LOWER"))
(println (string-capitalize "capitalize"))

(println "Should contain: " (string-contains "this is sick" "sick"))
(println "Should not contain: " (string-contains "this is sick" "not-sick"))

(println "Should contain: " (string-endswith "this is sick" "sick"))
(println "Should not contain: " (string-endswith "this is sick" "not-sick"))

(println "Should contain: " (string-startswith "this is sick" "this"))
(println "Should not contain: " (string-startswith "this is sick" "not-sick"))

(println "Len: " (string-length "this is sick"))
(println "Find: " (string-find "this is sick" "sick"))

(println "Replace: " (string-replace "this is sick" "this" "that"))
(println "Replace: " (string-replaceall "this is sick" "is" "_"))

(dump (string-split "this is sick" " "))
(println "Substr [3, 5]: " (string-substring "this is sick" 0 5))
(dump (string-splitlines "line 1\nline 2\nline 3"))

(println "Stripping: \"" (string-strip "    this is sick    ") "\"")
(println "Joining: " (string-join "this" " is" " sick"))

(println "This is alpha: " (char-isalpha ?a))
(println "This is not alpha: " (char-isalpha ?1))
(println "This is digit: " (char-isdigit ?1))
(println "This is not digit: " (char-isdigit ?a))



