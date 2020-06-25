(import 'assert-utils :all)
(import 'asserts :all)
(import 're :all)


(assert-int (re-compile "\\[string\\]"))

(let ((s (re-compile "fun(\\{\\d\\})")))
  (assert-int s)
  (assert (re-search s "fun{3} this is [string]"))
  (assert (re-match s "fun{3}"))
  (assert-equal "no_fun this is [sick]" (re-replace s "fun{3} this is [sick]" "no_fun")))


(assert (re-search "\\[string\\]" "fun{3} this is [string]"))
(assert (re-match "fun(\\{\\d\\})" "fun{3}"))

(assert-equal "no_fun this is [sick]" (re-replace "fun(\\{\\d\\})" "fun{3} this is [sick]" "no_fun"))

(assert-list-contain (nth (re-search-all "fun(\\{\\d\\})" "fun{3} this fun{5} is [string]") 1) "{5}")
(assert-list-contain (nth (re-search-all "fun(\\{\\d\\})" "fun{3} this fun{5} is [string]") 1) "fun{5}")
