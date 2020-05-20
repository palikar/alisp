### S

#### Description

S helps you work with strings. This module is entirely based on the [s.el](https://github.com/magnars/s.el) library for emacs lisp. The functions in the module are implemented pureley in alisp.
#### Functions

**s-word-initials** : *(s-word-initials S)*

Convert s to its initials.

**s-dashed-words** : *(s-dashed-words S)*

Convert s to dashed-words.

**s-upper-camel-case** : *(s-upper-camel-case S)*

Convert s to UpperCamelCase.

**s-split-words** : *(s-split-words S)*

Split s into list of words.

**s-lines** : *(s-lines S)*

Splits s into a list of strings on newline characters.

**s-concat** : *(s-concat &REST STRINGS)*

Join all the string arguments into one string.

**s-shared-start** : *(s-shared-start S1 S2)*

Returns the longest prefix s1 and s2 have in common.

**s-numeric?** : *(s-numeric? S)*

Is s a number?

**s-count-matches** : *(s-count-matches REGEXP S &OPTIONAL START END)*

Count occurrences of regexp in `s'.

start, inclusive, and end, exclusive, delimit the part of s to match.

**s-chop-prefixes** : *(s-chop-prefixes PREFIXES S)*

Remove prefixes one by one in order, if they are at the start of s.

**s-chop-suffix** : *(s-chop-suffix SUFFIX S)*

Remove suffix if it is at end of s.

**s-left** : *(s-left LEN S)*

Returns up to the len first chars of s.

**s-shared-end** : *(s-shared-end S1 S2)*

Returns the longest suffix s1 and s2 have in common.

**s-capitalized?** : *(s-capitalized? S)*

In s, is the first letter upper case, and all other letters lower case?

**s-truncate** : *(s-truncate LEN S)*

If s is longer than len, cut it down to len - 3 and add ... at the end.

**s-trim** : *(s-trim S)*

Remove whitespace at the beginning and end of s.

**s-snake-case** : *(s-snake-case S)*

Convert s to snake_case.

**s-trim-right** : *(s-trim-right S)*

Remove whitespace at the end of s.

**s-match** : *(s-match REGEXP S &OPTIONAL START)*

When the given expression matches the string, this function returns a list of the whole matching string and a string for each matched subexpressions. If it did not match the returned value is an empty list (nil).

When start is non-nil the search will start at that index.

**s-capitalized-words** : *(s-capitalized-words S)*

Convert s to Capitalized words.

**s-equals?** : *(s-equals? S1 S2)*

Is s1 equal to s2?

**s--mapcar-head** : *Like MAPCAR, but applies a different function to the first element.*

**s-pad-right** : *(s-pad-right LEN PADDING S)*

If s is shorter than len, pad it with padding on the right.

**s-titleized-words** : *(s-titleized-words S)*

Convert s to Titleized Words.

**s-right** : *(s-right LEN S)*

Returns up to the len last chars of s.

**s-present?** : *(s-present? S)*

Is s anything but nil or the empty string?

**s-mixedcase?** : *(s-mixedcase? S)*

Are there both lower case and upper case letters in s?

**s-chomp** : *(s-chomp S)*

Remove whitespace at the end of s.

**s-prepend** : *(s-prepend PREFIX S)*

Concatenate prefix and s.

**s-match-strings-all** : *(s-match-strings-all REGEX STRING)*

Return a list of matches for regex in string.

Each element itself is a list of matches, as per match-string. Multiple matches at the same position will be ignored after the first.

**s-chop-prefix** : *(s-chop-prefix PREFIX S)*

Remove prefix if it is at the start of s.

**s-blank?** : *(s-blank? S)*

Is s nil or the empty string?

**s-center** : *(s-center LEN S)*

If s is shorter than len, pad it with spaces so it is centered.

**s-downcase** : *(s-downcase S)*

Convert s to lower case.

**s-collapse-whitespace** : *(s-collapse-whitespace S)*

Convert all adjacent whitespace characters to a single space.

**s-repeat** : *(s-repeat NUM S)*

Make a string of s repeated num times.

**s-slice-at** : *(s-slice-at REGEXP S)*

Slices s up at every index matching regexp.

**s--times** : **

**s-word-wrap** : *(s-word-wrap LEN S)*

If s is longer than len, wrap the words with newlines.

**s-upcase** : *(s-upcase S)*

Convert s to upper case.

**s-split-up-to** : *(s-split-up-to SEPARATOR S N &OPTIONAL OMIT-NULLS)*

Split s up to n times into substrings bounded by matches for regexp separator.

If omit-nulls is non-nil, zero-length substrings are omitted.

**s-append** : *(s-append SUFFIX S)*

Concatenate s and suffix.

**s-join** : *(s-join SEPARATOR STRINGS)*

Join all the strings in strings with separator in between.

**s--join-with-sep** : **

**s-ends-with?** : *(s-ends-with? SUFFIX S &OPTIONAL IGNORE-CASE)*

Does s end with suffix?

If ignore-case is non-nil, the comparison is done without paying attention to case differences.

**s-matches?** : *(s-matches? REGEXP S &OPTIONAL START)*

Does regexp match s? If start is non-nil the search starts at that index.

**s-starts-with?** : *(s-starts-with? PREFIX S &OPTIONAL IGNORE-CASE)*

Does s start with prefix?

If ignore-case is non-nil, the comparison is done without paying attention to case differences.

**s-contains?** : *(s-contains? NEEDLE S &OPTIONAL IGNORE-CASE)*

Does s contain needle?

If ignore-case is non-nil, the comparison is done without paying attention to case differences.

**list-concat** : **

**s-lowercase?** : *(s-lowercase? S)*

Are all the letters in s in lower case?

**s-uppercase?** : *(s-uppercase? S)*

Are all the letters in s in upper case?

**s-reverse** : *(s-reverse S)*

Return the reverse of s.

**s-replace-all** : *(s-replace-all REPLACEMENTS S)*

replacements is a list of cons-cells. Each car is replaced with cdr in s.

**s-chop-suffixes** : *(s-chop-suffixes SUFFIXES S)*

Remove suffixes one by one in order, if they are at the end of s.

**s-trim-left** : *(s-trim-left S)*

Remove whitespace at the beginning of s.

**s-split** : *(s-split SEPARATOR S &OPTIONAL OMIT-NULLS)*

Split s into substrings bounded by matches for regexp separator. If omit-nulls is non-nil, zero-length substrings are omitted.

**s-capitalize** : *(s-capitalize S)*

Convert the first word's first character to upper case and the rest to lower case in s.

**s-titleize** : *(s-titleize S)*

Convert each word's first character to upper case and the rest to lower case in s.

**s-with** : *Threads s through the forms. Inserts s as the last item in the first form, making a list of it if it is not a list already. If there are more forms, inserts the first form as the last item in second form, etc.*

**s-pad-left** : *(s-pad-left LEN PADDING S)*

If s is shorter than len, pad it with padding on the left.

**s-less?** : *(s-less? S1 S2)*

Is s1 less than s2?

**s-presence** : *(s-presence S)*

Return s if it's s-present?, otherwise return nil.

**s-replace** : *(s-replace OLD NEW S)*

Replaces old with new in s.

**s-index-of** : *(s-index-of NEEDLE S &OPTIONAL IGNORE-CASE)*

Returns first index of needle in s, or nil.

**s-wrap** : *(s-wrap S PREFIX &OPTIONAL SUFFIX)*

Wrap string s with prefix and optionally suffix.

Return string s with prefix prepended. If suffix is present, it is appended, otherwise prefix is used as both prefix and suffix.

#### Constants


