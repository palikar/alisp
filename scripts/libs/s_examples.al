(import 's :all)

(dump (s-word-initials "camelCasedWords")) ;; => "cCW"
(dump (s-trim " space  "));; (dump (s-trim-left " space  "))
(dump (s-trim-right " space  "))
(dump (s-chomp "new\n"))
(dump (s-chomp "new\r"))
(dump (s-chomp "new\r\n"))
(dump (s-collapse-whitespace "only   one space   please"))
(dump (s-word-wrap 10 "This is too long"))
(dump (s-word-wrap 10 "This is way way too long"))
(dump (s-word-wrap 10 "It-wraps-words-but-does-not-break-them"))
(dump (s-center 5 "ab"))
(dump (s-pad-left 3 "0" "3"))
(dump (s-pad-left 3 "0" "23"))
(dump (s-pad-left 3 "0" "1234"))
(dump (s-pad-right 3 "." "3") )
(dump (s-pad-right 3 "." "23"))
(dump (s-pad-right 3 "." "1234"))
(dump (s-truncate 6 "This is too long"))
(dump (s-truncate 16 "This is also too long this is also too long "))
(dump (s-truncate 16 "But this is not!"))
(dump (s-left 3 "lib/file.js"))
(dump (s-left 3 "li"))
(dump (s-right 3 "lib/file.js"))
(dump (s-right 3 "li"))
(dump (s-chop-suffix "-test.js" "penguin-test.js"))
(dump (s-chop-suffix "\n" "no newlines\n"))
(dump (s-chop-suffix "\n" "some newlines\n\n"))
(dump (s-chop-suffixes '("_test.js" "-test.js" "Test.js") "penguin-test.js"))
(dump (s-chop-suffixes '("\r" "\n") "penguin\r\n"))
(dump (s-chop-suffixes '("\n" "\r") "penguin\r\n"))
(dump (s-chop-prefix "/tmp" "/tmp/file.js"))
(dump (s-chop-prefix "/tmp" "/tmp/tmp/file.js"))
(dump (s-chop-prefixes '("/tmp" "/my") "/tmp/my/file.js"))
(dump (s-chop-prefixes '("/my" "/tmp") "/tmp/my/file.js"))
(dump (s-shared-start "bar" "baz"))
(dump (s-shared-start "foobar" "foo"))
(dump (s-shared-start "bar" "foo"))
(dump (s-shared-end "bar" "var"))
(dump (s-shared-end "foo" "foo"))
(dump (s-shared-end "bar" "foo"))
(dump (s-match "^def" "abcdefg"))
(dump (s-match "^abc" "abcdefg"))
(dump (s-match "/.*/([a-z]+)\\.([a-z]+)" "/some/weird/file.html"))
(dump (s-slice-at "-" "abc"))
(dump (s-slice-at "-" "abc-def"))
(dump (s-slice-at "[.#]" "abc.def.ghi#id"))
(dump (s-split "|" "a|bc|12|3"))
(dump (s-split ":" "a,c,d"))
(dump (s-split "," "a,c,d"))
(dump (s-split "\n" "z\nefg\n"))
(dump (s-split "\n" "z\nefg\n" t))
(dump (s-split-up-to "-" "Author-Track-number-one" 1)) ;; => '("Author" "Track-number-one")
(dump (s-split-up-to "-" "Author-Track-number-one" 2)) ;; => '("Author" "Track" "number-one"))
(dump (s-split-up-to "|" "foo||bar|baz|qux" 3  t)) ;; => '("foo" "bar" "baz|qux")
(dump (s-matches? "^[0-9]+$" "123"))
(dump (s-matches? "^[0-9]+$" "a123"))
(dump (s-matches? "1" "10" 1))
(dump (s-lowercase? "file")) ;; => t
(dump (s-blank? "")) ;; => t
(dump (s-blank? nil)) ;; => t
(dump (s-blank? " ")) ;; => nil
(dump (s-lowercase? "File")) ;; => nil
(dump (s-lowercase? "fila")) ;; => t
(dump (s-uppercase? "HULKSMASH")) ;; => t
(dump (s-uppercase? "Bruce no smash")) ;; => nil
(dump (s-uppercase? "FoB")) ;; => nil
(dump (s-mixedcase? "HULK SMASH")) ;; => nil
(dump (s-mixedcase? "Bruce no smash")) ;; => t
(dump (s-mixedcase? "BRÜCE")) ;; => nil
(dump (s-capitalized? "Capitalized")) ;; => t
(dump (s-with "My car is a Toyota" (s-replace "car" "name")  (s-replace "a Toyota" "Bond") (s-append ", James Bond"))) ;; => "My name is Bond, James Bond"
(dump (s-count-matches "a" "aba")) ;; => 2
(dump (s-capitalized? "I am capitalized")) ;; => t
(dump (s-capitalized? "I Am Titleized")) ;; => nil
(dump (s-capitalized-words "some words")) ;; => "Some words"
(dump (s-count-matches "a" "aba" 0 2)) ;; => 1
(dump (s-count-matches "\\w{2}[0-9]+" "ab1bab2frobinator")) ;; => 2
(dump (s-wrap "foo" "\"")) ;; => "\"foo\""
(dump (s-wrap "foo" "(" ")")) ;; => "(foo)"
(dump (s-wrap "foo" "bar")) ;; => "barfoobar"
(dump (s-capitalized-words "some some some words")) ;; => "Some words"
(dump (s-titleized-words "some words")) ;; => "Some Words"
(dump (s-titleized-words "under scored words")) ;; => "Under Scored Words"
(dump (s-titleized-words "camel Cased Words")) ;; => "Camel Cased Words"
(dump (s-word-initials "some words")) ;; => "sw"
(dump (s-word-initials "under scored words")) ;; => "usw" 
