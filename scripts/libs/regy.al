

(import 're :all)


(dump (re-compile "\\[string\\]"))

(let ((s (re-compile "fun(\\{\\d\\})")))
  (dump s)
  (dump (re-search s "fun{3} this is [string]"))
  (dump (re-match s "fun{3}"))
  (dump (re-replace s "fun{3} this is [sick]" "no_fun")))

(dump (re-search "\\[string\\]" "fun{3} this is [string]"))
(dump (re-match "fun(\\{\\d\\})" "fun{3}"))

(dump (re-replace "fun(\\{\\d\\})" "fun{3} this is [sick]" "no_fun"))
(dump (nth (re-search-all "fun(\\{\\d\\})" "fun{3} this fun{5} is [string]") 1))
