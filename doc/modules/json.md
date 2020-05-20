### Json

#### Description

The `json` module can be used to parse and handle json-formated text. It can transoform JSON to an equvalent representation through s-expressions.

The s-exp representation that this module uses for an dict-like strucure is (plist)[https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node108.html]. A dictonary with keys and values can be viewed as a list of values like `(:key-1 "value-1" :key-2 "value-2")`. For example, this json snippet:
```json
{
"key-1" : "value-1",
"key-2" : 42,
"key-3" : [42 ,42],
"key-4" : ["42" , "42"]
}
```

will be represented throught the following s-expressions structure.

```elisp
(:key-1 "value-1" :key-2 42 :key-3 (42 42) :key-4 ("42" "42"))
```

The resulting representaion can be handeld through some of the functions that the module provides.

#### Functions

**dump-file** : *(dump-file PATH ALIST)*

Save the a json formated string representation of `ALIST` in the file pointed by `PATH`.


**load-file** : *(load-file PATH)*

Parse the contents of a file as json and return a alist representation of the json.


**json-dump** : *(json-dump ALIST)*

Convert a alist to a json formated string. Return the formated string.


**json-parse** : *(json-parse STRING)*

Parse a json formated string and return a alist representation of the json

#### Constants
**json-signal** : Signal raised when the json parser encounters an error.



