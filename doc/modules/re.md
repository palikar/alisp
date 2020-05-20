### re

#### Description

The `re` module provides support for working with regular
epxression. Compiled expressions are supported and one can customized
the compiling as well as the mathcing with the expresions through
certain flags.

Internally `re` uses the C++ standard library for compiling and
matching with regex. Through flags, you can build a regex acording
one of several standards.

The symbols starting with `re-match-` modify the matching process, the
symbols starting with `re-regex-` modify the building of a regex.

#### Functions

**re-replace** : *(re-replace [REGEX|STRING] STRING REPLACEMENT [MATCH_FLAGS])*

Try matching a part of `STRING` with the regex object or regex-string
and replace it with `REPLACEMENT`. Return the new string.

Optional flags for the mathing can be passed throught the `MATCH_FLAGS` list.


**re-match** : *(match [REGEX|STRING] STRING [MATCH_FLAGS])*

Try to match the whole of string `STRING` with the given regex object
or regex-string. Return nil if the match fails and return a list of
the match result if the match succeeds. The first element of the list
will be the whole match, subsequent elements will correspond to the
matched groups.

Optional flags for the mathing can be passed throught the `MATCH_FLAGS` list.


**re-search** : *(re-search [REGEX|STRING] STRING [MATCH_FLAGS])*

Search for matching substring in `STRING` with the regex object or
regex-string. In contrast to `re-match`, this functions does not try
to match the whole string but find a part of the string that matches
the regex. Return a list with the resutls of the searching.

Optional flags for the mathing can be passed throught the `MATCH_FLAGS` list.


**re-search-all** : *(re-search-all [REGEX|STRING] STRING [MATCH_FLAGS])*

Search for all the matches of a regexc in a string. This function is
like applying re-serach several times and finding all the matches of
the regex in a given string. Return a list of lists that are the
results of the individual matches.

Optional flags for the mathing can be passed throught the `MATCH_FLAGS` list.

**re-compile** : *(re-compile REGEX_STRING [BUILD_FLAGS_LIST])*

Compile the regex given in the string and return a resource object to
the created regex. Optionaly, build flags can be passed through the
`BUILD_FLAGS_LIST` list.


#### Constants
**re-regex-egrep** : Build flag: Use the regular expression grammar used by the grep
utility, with the -E option, in POSIX. This is effectively the same as
the extended option with the addition of newline '\n' as an
alternation separator in addtion to '|'.

**re-regex-extended** : Build flag: Use the basic POSIX regular expression grammar

**re-regex-collate** : Build flag: Character ranges of the form "[a-b]" will be locale sensitive. 

**re-regex-optimize** : Build flag: Instructs the regular expression engine to make matching
faster, with the potential cost of making construction slower. For
example, this might mean converting a non-deterministic FSA to a
deterministic FSA.

**re-regex-nosubs** : Build flag: When performing matches, all marked sub-expressions
(expr) are treated as non-marking sub-expressions (?:expr)

**re-regex-grep** : Build flag: Use the regular expression grammar used by the grep
utility in POSIX. This is effectively the same as the basic option
with the addition of newline '\n' as an alternation separator. 

**re-regex-icase** : Build flag: Character matching should be performed without regard to case. 

**re-regex-awk** : Build flag: Use the regular expression grammar used by the awk
utility in POSIX

**re-match-format-first-only** : Matching flag: Only replace the first match in re-replace



**re-match-not-bol** : Matching flag: The first character in [first,last) will be treated as
if it is not at the beginning of a line (i.e. ^ will not match
[first,first)

**re-match-default** : Default flag when matching a regex.

**re-match-not-eol** : Matching flag: The last character in [first,last) will be treated as
if it is not at the end of a line (i.e. $ will not match [last,last) 

**re-match-any** : Matching flag: If more than one match is possible, then any match is
an acceptable result 

**re-match-continous** : Matching flag: Only match a sub-sequence that begins at first 

**re-match-not-bow** : Matching flag: \b" will not match [first,first)

**re-match-prev-avail** : Matching flag: --first is a valid iterator position. When set, causes
match_not_bol and match_not_bow to be ignored 

**re-match-not-eow** : Matching flag: "\b" will not match [last,last) 

**re-match-format-sed** : Matching flag: Use POSIX sed utility rules in re-replace. (syntax
documentation) 

**re-match-not-null** : Matching flag: Do not match empty sequences 

**re-match-format-default** : Matching flag: Use ECMAScript rules to construct strings in
re-replace (syntax documentation) 

**re-regex-basic** : Build flag: Use the Modified [ECMAScript regular expression grammar](https://en.cppreference.com/w/cpp/regex/ecmascript)

**re-regex-ecma_script** : Build flag: Specifies that ^ shall match the beginning of a line and $ shall match the end of a line, if the ECMAScript engine is selected.

**re-match-format-no-copy** : Matching flag: Do not copy un-matched strings to the output in
re-replace 

