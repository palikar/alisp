(import 'json)

(assert (plist (json.json-parse "{\"key-1\" : \"value-1\"}")))
(assert (pstring (json.json-dump (json.json-parse "{\"key-1\" : \"value-1\"}" ))))
