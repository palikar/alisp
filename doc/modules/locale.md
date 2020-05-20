### Locale

#### Description

The `locale` module provides support for dealing with POSIX
locales. This allows developers to handle culture specific issues in
an application.

Internally the module uses the standard C++ library and proves
"sensible" access to the underlying functions.

Locales can be created through the `locale` function with a name such
as `en_US.utf8` or 'en_GB.utf8'. You can execute `locale -a` in a
terminal to see all of the locales that the host system supports. A
valid locale id is any one of the these locales.


#### Functions

**num-true-name** : *(num-true-name [LOCALE])*

Return the string used to represent `true` according to the global locale or the
`LOCALE` if given.


**isdigit** : *(isdigit CHAR [LOCALE])*

Check if `CHAR` is a digit character according to the default
locale or to `LOCALE` if given.


**isxdigit** : *(isxdigit CHAR [LOCALE])*

Check if `CHAR` is a hexdecimal digit character according to the default
locale or to `LOCALE` if given.


**isalpha** : *(isalpha CHAR [LOCALE])*

Check if `CHAR` is a graphical character according to the default
locale or to `LOCALE` if given.


**isprint** : *(isprint CHAR [LOCALE])*

Check if `CHAR` is a printable character according to the default
locale or to `LOCALE` if given.


**ispunct** : *(ispunct CHAR [LOCALE])*

Check if `CHAR` is a punctuation character according to the default
locale or to `LOCALE` if given.


**num-false-name** : *(num-false-name [LOCALE])*

Return the string used to represent `false` according to the global locale or the
`LOCALE` if given.


**isgraph** : *(isgraph CHAR [LOCALE])*

Check if `CHAR` is a graphical character according to the default
locale or to `LOCALE` if given.


**reset-locale** : *(reset-locale)*

Reset the global locate to the default locale.


**put-num** : *(put-num NUMBER [LOCALE])*

Return a textural representation of `NUMBER` as a number according to
the global locale or `LOCALE` if given.


**locale-name** : *(locale-name [LOCALE]*

Return the name of the global default locale. If `LOCALE` is given,
return its name.
)

**money-thousand-sep** : *(money-thousand-sep [LOCALE])*

Return the character used for deciaml point in money strings as used
by the global locale or the `LOCALE` if given.


**set-preffered-locale** : *(set-preffered-locale)*

Set the global locale to the preffered (according to the host system) locale.


**money-positive-sign** : *(money-positive-sign [LOCALE])*

Return the character used to represent positive amount of money
accrding by the global locale or the `LOCALE` if given.
 

**set-locale** : *(set-locale LOCALE)*

Change the current global default locale to `LOCALE`.


**islower** : *(islower CHAR [LOCALE])*

Check if `CHAR` is a lower case character according to the default
locale or to `LOCALE` if given.


**iscntrl** : *(iscntrl CHAR [LOCALE])*

Check if `CHAR` is a control character according to the default
locale or to `LOCALE` if given.


**locale** : *(locale ID)*

Create a new locale with the given id as s tring and return a resource
object for it.


**isblank** : *(isblank CHAR [LOCALE])*

Check if `CHAR` is a blank character according to the default
locale or to `LOCALE` if given.

**isspace** : *(isspace CHAR [LOCALE])*

Check if `CHAR` is a space character according to the default
locale or to `LOCALE` if given.


**put-time** : *(put-time TIME FORMAT-STRING [LOCALE])*

Return a fromated string of `FORMAT-STRING` with `TIME` acording to
the global locale or `LOCALE` if given.


**isalnum** : *(isalnum CHAR [LOCALE])*

Check if `CHAR` is alpha-numerical character according to the default
locale or to `LOCALE` if given.


**put-money** : *(put-money NUMBER [LOCALE])*

Return a textural representation of `NUMBER` as currency according to
the global locale or `LOCALE` if given.


**money-decimal-point** : *(money-decimal-point [LOCALE])*

Return the character used for deciaml point in money strings as used
by the global locale or the `LOCALE` if given.


**num-thousand-sep** : *(num-thousand-sep [LOCALE])*

Return the character used to separate the thousands in the textual
representation of real numbers according to the global locale or the
`LOCALE` if given.


**money-symobl** : *(money-symbol [LOCALE])*

Return the character used to represent the local currrency accrding by
the global locale or the `LOCALE` if given.


**isupper** : *(isupper CHAR [LOCALE])*

Check if `CHAR` is a upper case character according to the default
locale or to `LOCALE` if given.


**money-negative-sign** : *(money-negative-sign [LOCALE])*

Return the character used to represent negative amount of money
accrding by the global locale or the `LOCALE` if given.


**num-decimal-point** : *(num-decimal-point [LOCALE])*

Return the character used for decimal point in textural representation
of real numbers according to the global locale or the `LOCALE` if
given.


#### Constants


