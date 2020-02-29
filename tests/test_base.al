(import 'base64 :all)


(assert (equal (base16-decode-string (base16-encode-string "I love bubence")) "I love bubence" ))
(assert (equal (base32-decode-string (base32-encode-string "I love bubence")) "I love bubence" ))
(assert (equal (base64-decode-string (base64-encode-string "I love bubence")) "I love bubence" ))

(assert (equal (base16-decode-string (base16-encode-bytes '(97 98 99))) "abc" ))
(assert (equal (base32-decode-string (base32-encode-bytes '(97 98 99))) "abc" ))
(assert (equal (base64-decode-string (base64-encode-bytes '(97 98 99))) "abc" ))

