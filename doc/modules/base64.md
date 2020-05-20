### Base64

#### Description

The `base64` module provides several codecs for encoding byte-data -- base64, base32 and base16. There are functions that operate on strings as well as ones that take raw byte lists. The decoding functions return either a string representation of the original input or byte list that contains the pure bytes of the decoded information.

Example:
```elisp

(println (base64-decode-string (base64-encode-string "this is a string")))
(println (base16-decode-string (base16-encode-string "this is a string")))

(mapc println (base64-decode-bytes (base64-encode-bytes '(97 98 99))))
(mapc println (base16-decode-bytes (base16-encode-bytes '(97 98 99))))
```
#### Functions

**base32-decode-bytes** : *(base32-decode-bytes BYTES_LIST)*
Decode the base32 encoded bytes `BYTES_LIST` and return the result as string.


**base32-encode-bytes** : *(base32-encode-bytes STRING)*

Return the bytes of encoding the string `STRING` in base32.


**base16-decode-bytes** : *(base16-decode-bytes BYTES_LIST)*

Decode the base16 encoded bytes `BYTES_LIST` and return the result as string.


**base16-decode-string** : *(base16-decode-string STRING)*

Decode the base16 encoded string `STRING` and return the result as string.


**base16-encode-bytes** : *(base16-encode-string STRING)*

Return the bytes of encoding the string `STRING` in base16.


**base16-encode-string** : *(base16-encode-string STRING)*

Return base16 encoded version of the string `STRING`.


**base64-decode-bytes** : *(base64-decode-bytes BYTES_LIST)*

Decode the base64 encoded bytes `BYTES_LIST` and return the result as string.


**base32-decode-string** : *(base32-decode-string STRING)*

Decode the base32 encoded string `STRING` and return the result as string.


**base32-encode-string** : *(base32-encode-string STRING)*

Return base32 encoded version of the string `STRING`.


**base64-decode-string** : *(base64-decode-string STRING)*

Decode the base64 encoded string `STRING` and return the result as string.


**base64-encode-bytes** : *(base64-encode-bytes STRING)*

Return the bytes of encoding the string `STRING` in base64.


**base64-encode-string** : *(base64-encode-string STRING)*

Return base64 encoded version of the string `STRING`.


#### Constants


