# Basic builit in functions.
## Language constructs

Basic fuctions that provide the backbone of the language. These include global and local variable definition, flow control structures and loops.

> ##### **import** : *(import MODULE [:file file] [:all] [( [(SYM MAPPED)]... )])*

Import the module MODULE. MODULE should be a symbol and the imported
module should be in a file with the name of this symbol. The file
should be located somewhere on the ALISPPATH. An alternative file name
can be given through the :file keyword-argument. If the :all
keyword-argument is given. all of the symbols in MODULE will be
imported in the root scope of the current module. The last argument is
an optional list of mappings between symbols in the imported modules
and new symbols to be imported in the current module.

Example:
```elisp
(import 'fileio)
(import 'fileio :all)
(import 'fileio :file "../fileio.al")
(import 'fileio :all (f-exists exists))
```


> ##### **modref** : *(modref MODUE [[MODUE] ...] SYMBOL [[symbol] ...] )*

Refrence a symbol in another module. The function can also be used to
reference symbols from submodules. That is, if a module imports
another module for itself, symbols in it can also be referenced. In
most circumstances you won't need this function as there is a
syntactic sugar for it - the dot syntax.

Example:
```elisp
(import 'fileio)

; those two are equivalent
((modref fileio f-exists) "../file.al")
(fileio.f-exists "../file.al")
```

The last argument of `modref` must be the symbol name. The previous
arguments should module names.


> ##### **defun** : *(defun NAME (ARGLIST) [DOC] [BODY])*

Define a new functions with a name `NAME` in the current
module. `ARGLIST` should be a valid argument list definition. `DOC` is
an optional docscring and `BODY` is a list of forms to be evaluated
when the function is called.

Example:
```elisp
(defun fun (x)
   "This is a new function"
   (println x))
```
 

> ##### **eval** : *(eval FORM)*

Evaluate the form `FORM`. The usual form for evaluation apply.


> ##### **setq** : *(setq SYMBOL VALUE [[SYMBOL VALUE] ... ])*

Set the value of the variable pointed by `SYMBOL` to
`VALUE`. `SYMBOL` will not be evaluated. `setq` can also be used to
set the value of multiple variables at once. All of the variables
should be defined beforehand.

Example:
```elisp
(defvar new-var 42)
(setq new-var 43)
```


> ##### **set** : *((set FORM VALUE))*

Set the value of the variable pointed by `FORM` to `VALUE`. `FORM`
will be evaluated and should return a symbol. `setq` can also be used
to set the value of multiple variables at once. All of the variables
should be defined beforehand.

Example:
```elisp
(defvar new-var 42)
(set 'new-var 43)
```


> ##### **setq** : *(setq SYMBOL VALUE [[SYMBOL VALUE] ... ])*

Set the value of the variable pointed by `SYMBOL` to
`VALUE`. `SYMBOL` will not be evaluated. `setq` can also be used to
set the value of multiple variables at once. All of the variables
should be defined beforehand.

Example:
```elisp
(defvar new-var 42)
(setq new-var 43)
```


> ##### **quote** : *(quote FORM)*

Return `FORM`, without evaluating it. `(quote x)` yields ‘x’. `x is a
syntactic sugar for this function.


> ##### **function** : *(funtion OBJECT)*

Return `OBJECT`, without evaluating it but setting its function flag
to true. `function` should be used to quote lambdas and other
callables.


> ##### **lambda** : *(lambda (ARGLIST) BODY)*




> ##### **if** : *(if CONDITION THEN ELSE)*

Evaluate `CONDITION` and if its value is *truthy*, evaluate and return
the value of `THEN`. Otherwise evaluate and return the value of
`ELSE`.


> ##### **while** : *(while CONDITION BODY)*

Evaluate `BODY` as long as `CONDITION` evaluates to a value that is
*truthy*. `while` returns `nil`.


> ##### **dolist** : *(dolist (SYMBOL LIST) BODY)*

Evaluate `BODY` for each symbol in `LIST` while bonding the respective
element to `SYMBOL`.

Example:
```elisp
(dolist (s '(1 2 3 4))
   (println s))
```



> ##### **cond** : *(cond [[CODITION BODY] ... ])*

> ##### **when** : *(when CONDITION BODY)*

Evaluate `BODY` if `CONDITION` evaluates to *truthy* value.


> ##### **unless** : *(unless CONDITION BODY)*

Evaluate `BODY` if `CONDITION` evaluates to *falsey* value.


> ##### **let** : *(let ([[VAR]...] [[(VAR VALUE)] ...] ) BODY)*

Bind local variables and execute `BODY`. The second argument is a list
of forms like `(VARIABLE VALUE)`. Each `VALUE` will be evaluated and
its value will be bound to `VARIABLE`. `nil` variables can also be
declared without initial value.

Example:
```elisp

(let ((var-1 42)
      (var-2 "43")
       var-3)         ; nil variable
   (println var-1)    ; 42
   (println var-2))   ; 43

```


> ##### **let*** : *(let* ([[VAR]...] [[(VAR VALUE)] ...] ) BODY)*

Bind local variables and execute `BODY`. In contrast `let`, each
variable can be used in the definition of the following variables. 

(let ((var-1 42)
      (var-2 var-1)
       var-3)         ; nil variable
   (println var-1)    ; 42
   (println var-2))   ; 43



> ##### **or** : *(or [[VALUE]...])*

> ##### **and** : *(and [[VALUE]...])*

> ##### **not** : *(not VALUE)*

> ##### **parse-int** : *(parse-int STRING)*

> ##### **parse-float** : *(parse-float STRING)*

> ##### **to-string** : *(to-string VALUE)*

> ##### **to-char** : *(to-char INT)*

> ##### **funcall** : *(funcall SYMBOL LIST)*

Call the function pointed by `SYMBOL` and pass the symbols in `LIST`
as arguments.


> ##### **backquote** : *(backquote LIST)*

Backquote the list `LIST`. `LIST is syntactic sugar for this function.

Example:
```elisp

`(val-1 ,val-2 ,@(val-3 val-3 )) ; '(val-1 (eval val-2) val-3 val-3)
```


> ##### **return** : *(return [FROM])*

Return an optional value from a function. If `FROM` is given, it will
be evaluated and its value will be the return value of the
function. Otherwise `nil` is the returned value.



> ##### **exit** : *(exit [FORM])*

Exit the program. If `FORM` is given, its value will be the return
code of the process. Otherwise the return code will be 0.


## Printing

Functions to interact with the stanard input and output.

> ##### **print** : *(print VALUE [[VALUE] ...])*

> ##### **println** : *(println VALUE [[VALUE] ...])*

> ##### **eprint** : *(eprint VALUE [[VALUE] ...])*

> ##### **eprintln** : *(eprintln VALUE [[VALUE] ...])*

> ##### **read-line** : *(read-line)*

## Lists

Functions to interact with the stanard input and output.

> ##### **length** : *(length LIST)*

> ##### **cons** : *(cons LIST)*

> ##### **head** : *(head LIST)*

> ##### **last** : *(last LIST)*

> ##### **init** : *(init LIST)*

> ##### **tail** : *(tail LIST)*

> ##### **nth** : *(nth LIST INDEX)*

> ##### **mapc** : *(mapc FUNCTION LIST)*

> ##### **mapcar** : *(mapcar FUNCTION LIST)*

> ##### **push** : *(push LIST ELEMENT)*

> ##### **delete** : *(delete LIST ELEMENT)*

> ##### **remove** : *(remove LIST ELEMENT)*

> ##### **range** : *(range FROM TO)*

## Object Properties

Functions for accessing the properties of objects.

> ##### **prop-get** : *(prop-get SYM PROPERTY)*

> ##### **prop-set** : *(prop-set SYM PROPERTY VALUE)*

> ##### **prop-list** : *(prop-list SYM)*

## Object Properties

Functions for type inspecting. These functions can be used to check whether an object is from a certain type.

> ##### **pstring** : *(pstring SYMBOL)*

> ##### **plist** : *(plist SYMBOL)*

> ##### **pint** : *(pint SYMBOL)*

> ##### **preal** : *(preal SYMBOL)*

> ##### **psym** : *(psym SYMBOL)*

> ##### **pfunction** : *(pfunction SYMBOL)*

## Strings

Functions for basic string handling.

> ##### **string-length** : *(string-length STRING)*

> ##### **string-contains** : *(string-contains STRING SUBSTRING)*

> ##### **string-endswith** : *(string-contains STRING SUFFIX)*

> ##### **string-startswith** : *(string-contains STRING PREFIX)*

> ##### **string-length** : *(string-length STRING)*

> ##### **string-capitalize** : *(string-capitalize STRING)*

> ##### **string-find** : *(string-find STRING SUBSTRING)*

> ##### **string-replace** : *(string-replace STRING SUBSTRING NEWSTRING)*

> ##### **string-replaceall** : *(string-replaceall STRING SUBSTRING NEWSTRING)*

> ##### **string-split** : *(string-split STRING DELIMETER)*

> ##### **string-substring** : *(string-substring STRING FROM TO)*

> ##### **string-splitlines** : *(string-splitlines STRING)*

> ##### **string-upper** : *(string-upper STRING)*

> ##### **string-lower** : *(string-lower STRING)*

> ##### **string-strip** : *(string-strip STRING)*

> ##### **string-join** : *(string-join STRING [[STRING] ...])*

> ##### **char-isalpha** : *(char-isalpha CHAR)*

> ##### **char-isdigit** : *(char-isdigit CHAR)*

## Basic Math

Functions that realise simple math operations.

> ##### **+** : *(+ [[VALUE]...])*

> ##### **-** : *(- [[VALUE]...])*

> ##### **/** : *(/ [[VALUE]...])*

> ##### ***** : *(* [[VALUE]...])*

> ##### **<** : *(< VALUE1 VALUE2)*

> ##### **<=** : *(<= VALUE1 VALUE2)*

> ##### **>** : *(> VALUE1 VALUE2)*

> ##### **>=** : *(>= VALUE1 VALUE2)*

> ##### **==** : *(== VALUE1 VALUE2)*

> ##### **!=** : *(!= VALUE1 VALUE2)*

> ##### **mod** : *(mod VALUE1)*

> ##### **pow** : *(pow VALUE1 VALUE2)*

> ##### **min** : *(min [[VALUE]...])*

> ##### **max** : *(max [[VALUE]...])*

> ##### **round** : *(round VALUE PLACES)*

## Algorithms

Several functions for basic algorithms for working with lists.

> ##### **slice** : *(slice LIST FROM TO)*

> ##### **sort** : *(sort LIST)*

> ##### **sort** : *(sort LIST)*

> ##### **zip** : *(zip [[LIST] ...])*

> ##### **filter** : *(filter FUNCTION LIST)*

> ##### **any** : *(any FUNCTION LIST)*

> ##### **all** : *(all FUNCTION LIST)*

