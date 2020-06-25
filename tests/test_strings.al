(import 'assert-utils :all)
(import 'asserts :all)

(assert-equal "hello-no" (string-append "hello" "-no"))
(assert-equal "no-hello" (string-prepend "hello" "no-"))
(assert (string-equals "hello" "hello"))
(assert (string-less "a" "b"))
(assert (string-contains "ass" "as"))
(assert (string-startswith "abc" "a"))
(assert (string-endswith "abc" "c"))
(assert-== 3 (string-length "abc"))
(assert-equal "Abc" (string-capitalize "abc"))

(assert  (char-isalpha ?a))
(assert-not  (char-isalpha ?3))

(assert  (char-isdigit ?3))
(assert-not  (char-isdigit ?a))

(assert-== 1 (string-find "abc" "b"))
(assert-equal "cba" (string-reverse "abc"))
(assert-equal "a?c" (string-replace "abc" "b" "?"))
(assert-equal "a?c?" (string-replaceall "abcb" "b" "?"))

(assert-list-contain (string-split "abc" "b") "a")
(assert-list-contain (string-split "abc" "b") "c")

(assert-equal "abc" (string-substring "abcdef" 0 3))

(assert-== 3 (length (string-splitlines "a\nb\nc")) )

(assert-equal "ABC" (string-upper "abc") )
(assert-equal "abc" (string-lower "aBC") )

(assert-equal "abc" (string-strip "  abc ") )

(assert-equal "abc" (string-join "a" "b" "c") )




