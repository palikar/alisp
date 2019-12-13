(if (psym 'a) "this is sym")

(if (pstr "str") "this is str")

(if (pint 42) "this is int")

(if (preal 3.14) "this is real")

(if (pfunction (lambda (a) (print a))) "this is function")

(if (plist '(a b c)) "this is list")
