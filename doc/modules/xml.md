### XML

#### Description

The `xml` module enables the handling of XML-formated data. It
provides functionality for parsing and dumping s-expressions as XML.

Internally `xml` uses the
[tinyxml2](https://github.com/leethomason/tinyxml2) library.


#### Functions

**dump-file** : **

Save the xml-formated string representation of `ALIST` in the file pointed by `PATH`.


**load-file** : **

Parse the contents of a file as xml and return a alist representation of the xml.


**xml-dump** : **
Convert a alist to a xml-formated string. Return the formated string.


**xml-parse** : **

Parse a xml-formated string and return a alist representation of the xml

#### Constants


