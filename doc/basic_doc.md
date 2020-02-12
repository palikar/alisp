# Basic builit in functions.
## Language constructs

Basic fuctions that provide the backbone of the language. These include global and local variable definition, flow control structures and loops.

+ *import*: (import NAME [:file file] [:all] [( [(SYM MAPPED)]... )])

+ *modref*: (modref MODUE [[MODUE] ...] SYMBOL [[symbol] ...] )

+ *defun*: (defun NAME (ARGLIST) [DOC] BODY)

+ *eval*: (eval FORM)

+ *setq*: (setq SYMBOL VALUE)

+ *set*: ((set SYMBOL VALUE))

+ *setq*: (setq SYMBOL VALUE)

+ *quote*: (quote OBJECT)

+ *function*: (funttion OBJECT)

+ *lambda*: (lambda (ARGLIST) BODY)

+ *if*: (if CONDITION THEN ELSE)

+ *while*: (while CONDITION BODY)

+ *dolist*: (dolist (SYMBOL LIST) BODY)

+ *cond*: (cond [[CODITION BODY] ... ])

+ *when*: (when CONDITION BODY)

+ *unless*: (unless CONDITION BODY)

+ *let*: (let ([[VAR]...] [[(VAR VALUE)] ...] ) BODY)

+ *let**: (let* ([[VAR]...] [[(VAR VALUE)] ...] ) BODY)

+ *or*: (or [[VALUE]...])

+ *and*: (and [[VALUE]...])

+ *not*: (not VALUE)

+ *parse-int*: (parse-int STRING)

+ *parse-float*: (parse-float STRING)

+ *to-string*: (to-string VALUE)

+ *to-char*: (to-char INT)

+ *funcall*: (funcall SYMBOL LIST)

+ *backquote*: (`LIST)

+ *return*: (return [VALUE])

+ *exit*: (exit [VALUE])

## Printing

Functions to interact with the stanard input and output.

+ *print*: (print VALUE [[VALUE] ...])

+ *println*: (println VALUE [[VALUE] ...])

+ *eprint*: (eprint VALUE [[VALUE] ...])

+ *eprintln*: (eprintln VALUE [[VALUE] ...])

+ *read-line*: (read-line)

## Lists

Functions to interact with the stanard input and output.

+ *length*: (length LIST)

+ *cons*: (cons LIST)

+ *head*: (head LIST)

+ *last*: (last LIST)

+ *init*: (init LIST)

+ *tail*: (tail LIST)

+ *nth*: (nth LIST INDEX)

+ *mapc*: (mapc FUNCTION LIST)

+ *mapcar*: (mapcar FUNCTION LIST)

+ *push*: (push LIST ELEMENT)

+ *delete*: (delete LIST ELEMENT)

+ *remove*: (remove LIST ELEMENT)

+ *range*: (range FROM TO)

## Object Properties

Functions for accessing the properties of objects.

+ *prop-get*: (prop-get SYM PROPERTY)

+ *prop-set*: (prop-set SYM PROPERTY VALUE)

+ *prop-list*: (prop-list SYM)

## Object Properties

Functions for type inspecting. These functions can be used to check whether an object is from a certain type.

+ *pstring*: (pstring SYMBOL)

+ *plist*: (plist SYMBOL)

+ *pint*: (pint SYMBOL)

+ *preal*: (preal SYMBOL)

+ *psym*: (psym SYMBOL)

+ *pfunction*: (pfunction SYMBOL)

## Strings

Functions for basic string handling.

+ *string-length*: (string-length STRING)

+ *string-contains*: (string-contains STRING SUBSTRING)

+ *string-endswith*: (string-contains STRING SUFFIX)

+ *string-startswith*: (string-contains STRING PREFIX)

+ *string-length*: (string-length STRING)

+ *string-capitalize*: (string-capitalize STRING)

+ *string-find*: (string-find STRING SUBSTRING)

+ *string-replace*: (string-replace STRING SUBSTRING NEWSTRING)

+ *string-replaceall*: (string-replaceall STRING SUBSTRING NEWSTRING)

+ *string-split*: (string-split STRING DELIMETER)

+ *string-substring*: (string-substring STRING FROM TO)

+ *string-splitlines*: (string-splitlines STRING)

+ *string-upper*: (string-upper STRING)

+ *string-lower*: (string-lower STRING)

+ *string-strip*: (string-strip STRING)

+ *string-join*: (string-join STRING [[STRING] ...])

+ *char-isalpha*: (char-isalpha CHAR)

+ *char-isdigit*: (char-isdigit CHAR)

## Basic Math

Functions that realise simple math operations.

+ *+*: (+ [[VALUE]...])

+ *-*: (- [[VALUE]...])

+ */*: (/ [[VALUE]...])

+ ***: (* [[VALUE]...])

+ *<*: (< VALUE1 VALUE2)

+ *<=*: (<= VALUE1 VALUE2)

+ *>*: (> VALUE1 VALUE2)

+ *>=*: (>= VALUE1 VALUE2)

+ *==*: (== VALUE1 VALUE2)

+ *!=*: (!= VALUE1 VALUE2)

+ *mod*: (mod VALUE1)

+ *pow*: (pow VALUE1 VALUE2)

+ *min*: (min [[VALUE]...])

+ *max*: (max [[VALUE]...])

+ *round*: (round VALUE PLACES)

## Algorithms

Several functions for basic algorithms for working with lists.

+ *slice*: (slice LIST FROM TO)

+ *sort*: (sort LIST)

+ *sort*: (sort LIST)

+ *zip*: (zip [[LIST] ...])

+ *filter*: (filter FUNCTION LIST)

+ *any*: (any FUNCTION LIST)

+ *all*: (all FUNCTION LIST)

