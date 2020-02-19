# Basic builtin functions.
## Language constructs

Basic fuctions that provide the backbone of the language. These include global and local variable definition, flow control structures and loops.

- ###**import** : *(import MODULE [:file file] [:all] [( [(SYM MAPPED)]... )])*

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


- ###**modref** : *(modref MODUE [[MODUE] ...] SYMBOL [[symbol] ...] )*

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


- ###**defun** : *(defun NAME (ARGLIST) [DOC] [BODY])*

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
 

- ###**eval** : *(eval FORM)*

Evaluate the form `FORM`. The usual form for evaluation apply.


- ###**setq** : *(setq SYMBOL VALUE [[SYMBOL VALUE] ... ])*

Set the value of the variable pointed by `SYMBOL` to
`VALUE`. `SYMBOL` will not be evaluated. `setq` can also be used to
set the value of multiple variables at once. All of the variables
should be defined beforehand.

Example:
```elisp
(defvar new-var 42)
(setq new-var 43)
```


- ###**set** : *((set FORM VALUE))*

Set the value of the variable pointed by `FORM` to `VALUE`. `FORM`
will be evaluated and should return a symbol. `setq` can also be used
to set the value of multiple variables at once. All of the variables
should be defined beforehand.

Example:
```elisp
(defvar new-var 42)
(set 'new-var 43)
```


- ###**setq** : *(setq SYMBOL VALUE [[SYMBOL VALUE] ... ])*

Set the value of the variable pointed by `SYMBOL` to
`VALUE`. `SYMBOL` will not be evaluated. `setq` can also be used to
set the value of multiple variables at once. All of the variables
should be defined beforehand.

Example:
```elisp
(defvar new-var 42)
(setq new-var 43)
```


- ###**quote** : *(quote FORM)*

Return `FORM`, without evaluating it. `(quote x)` yields ‘x’. `x is a
syntactic sugar for this function.


- ###**function** : *(funtion OBJECT)*

Return `OBJECT`, without evaluating it but setting its function flag
to true. `function` should be used to quote lambdas and other
callables.


- ###**lambda** : *(lambda (ARGLIST) BODY)*




- ###**if** : *(if CONDITION THEN ELSE)*

Evaluate `CONDITION` and if its value is *truthy*, evaluate and return
the value of `THEN`. Otherwise evaluate and return the value of
`ELSE`.


- ###**while** : *(while CONDITION BODY)*

Evaluate `BODY` as long as `CONDITION` evaluates to a value that is
*truthy*. `while` returns `nil`.


- ###**dolist** : *(dolist (SYMBOL LIST) BODY)*

Evaluate `BODY` for each symbol in `LIST` while bonding the respective
element to `SYMBOL`.

Example:
```elisp
(dolist (s '(1 2 3 4))
   (println s))
```



- ###**cond** : *(cond [ ( [CODITION BODY] ) ... ])*

Chooses what to evaluate among an arbitrary number of
alternatives. Each clause must a list. The first element of each list
will be evaluated and if its value is truthy, the rest of the elements
of the corresponging list will also be evaluated. The evaluation of
`cond` is then finished.

Example:
```elisp
(cond
((== (1  2)) (println "This won't print"))
((== (2  2)) (println "This will print")))
```


- ###**when** : *(when CONDITION BODY)*

Evaluate `BODY` if `CONDITION` evaluates to *truthy* value.


- ###**unless** : *(unless CONDITION BODY)*

Evaluate `BODY` if `CONDITION` evaluates to *falsey* value.


- ###**let** : *(let ([[VAR]...] [[(VAR VALUE)] ...] ) BODY)*

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


- ###**let*** : *(let* ([[VAR]...] [[(VAR VALUE)] ...] ) BODY)*

Bind local variables and execute `BODY`. In contrast `let`, each
variable can be used in the definition of the following variables. 

Example:
```elisp
(let* ((var-1 42)
      (var-2 var-1)
       var-3)         ; nil variable
   (println var-1)    ; 42
   (println var-2))   ; 43
```


- ###**or** : *(or [[VALUE]...])*

Return `t` if at least one of the arguments evaluates to a truthy
value. The arguments are lazily evaluated.



- ###**and** : *(and [[VALUE]...])*

Return `t` if all of the arguments evaluates to a truthy
value. The arguments are lazily evaluated.


- ###**not** : *(not FORM)*

Return `t` if FORM evaluate to a falsey value and `nil` otherwise. 


- ###**parse-int** : *(parse-int STRING)*

Return the int value represented by STRING.

Example:
```elisp
(parse-int "12")
```


- ###**parse-float** : *(parse-float STRING)*

Return the real value represented by STRING.

Example:
```elisp
(parse-int "12.32")


- ###**to-string** : *(to-string VALUE)*

Convert VALUE to string

Example:
```elisp
(to-string 42)
(to-string 42.32)
(to-string "string")
```


- ###**to-char** : *(to-char INT)*

Convert INT to a character (ASCII encoding). INT must be a value in
the range [0, 255].

Example:
```elisp
(to-char 65)
(to-char 97)
```


- ###**funcall** : *(funcall SYMBOL LIST)*

Call the function pointed by `SYMBOL` and pass the symbols in `LIST`
as arguments.


- ###**backquote** : *(backquote LIST)*

Backquote the list `LIST`. `LIST is syntactic sugar for this function.

Example:
```elisp

`(val-1 ,val-2 ,@(val-3 val-3 )) ; '(val-1 (eval val-2) val-3 val-3)
```


- ###**return** : *(return [FROM])*

Return an optional value from a function. If `FROM` is given, it will
be evaluated and its value will be the return value of the
function. Otherwise `nil` is the returned value.



- ###**exit** : *(exit [FORM])*

Exit the program. If `FORM` is given, its value will be the return
code of the process. Otherwise the return code will be 0.


## Printing

Functions to interact with the stanard input and output.

- ###**print** : *(print FORM [[FORM] ...])*

Print the value of VALUE of form on the standard output stream.


- ###**println** : *(println VALUE [[VALUE] ...])*

Print the value of VALUE of form on the standard output stream and put a new
line character.


- ###**eprint** : *(eprint VALUE [[VALUE] ...])*

Print the value of VALUE of form on the standard error stream.


- ###**eprintln** : *(eprintln VALUE [[VALUE] ...])*

Print the value of VALUE of form on the standard error stream and put a new
line character.


- ###**read-line** : *(read-line)*

Read a single line form the standard input stream and return it.


## Lists

Functions to interact with the stanard input and output.

- ###**length** : *(length LIST)*

Return the number of elements in LIST.

Example:
```elisp
(length '(1 2 3 4 5))
```


- ###**cons** : *(cons LIST)*

Return a sublist of LIST with all of its elements but the first one.

Example:
```elisp
(cons '(1 2 3 4 5))
```


- ###**head** : *(head LIST)*

Return the fist element of the list LIST.

Example:
```elisp
(head '(1 2 3 4 5))
```


- ###**last** : *(last LIST)*

Return the last element of the list LIST.

Example:
```elisp
(last '(1 2 3 4 5))
```


- ###**init** : *(init LIST)*

Return a sublist of LIST with all of its elements but the last one.

Example:
```elisp
(init '(1 2 3 4 5))
```


- ###**tail** : *(tail LIST)*

Return a sublist of LIST with all of its elements but the first one.

Example:
```elisp
(tail '(1 2 3 4 5))
```


- ###**nth** : *(nth LIST INDEX)*

Return the element of LIST that is at position INDEX.

Example:
```elisp
(nth '(1 2 3 4 5) 1)
```


- ###**mapc** : *(mapc FUNCTION LIST)*

Call FUNCTION for each element of LIST. The element is passed as an
argument to the function. `mapc` return `t` and its executed only for side effects.

Example:
```elisp
(mapc println '(1 2 3 4 5))
(mapc (lambda (x) (print x)) '(1 2 3 4 5))
```


- ###**mapcar** : *(mapcar FUNCTION LIST)*


Call FUNCTION for each element of LIST while collecting the results of
the calls and building a new list. The new list is returned.

Example:
```elisp
(mapcar (lambda (x) (+ x 1)) '(1 2 3 4 5))
```


- ###**push** : *(push LIST ELEMENT)*

Add ELEMENT to the end of the LIST. This function changes the LIST
rather than to create a new one.

Example:
```elisp
(push '(1 2 3 4 5) 6)
```


- ###**delete** : *(delete LIST ELEMENT)*

Remove an element from LIST that is equal (with `equal`) to
ELEMENT. This function operates inplace, so list is changed and no new
list is created. 
Example:
```elisp
(delete '(1 2 3 4 5) 5)
```


- ###**remove** : *(remove LIST ELEMENT)*

Remove an element from LIST that is equal (with `equal`) to
ELEMENT. This function __does not__ operate inplace, so a new list is
created.

Example:
```elisp
(delete '(1 2 3 4 5) 5)
```



- ###**range** : *(range FROM TO STEP)*

Generate the range of numbers [FROM, TO) with a step STEP. All of
the arguments must be ints.

Example:
```elisp
(range 0 100 2)
```


## Object Properties

Functions for accessing the properties of objects.

- ###**prop-get** : *(prop-get SYM PROPERTY)*

Return the property with name PROPERTY of SYM.


- ###**prop-set** : *(prop-set SYM PROPERTY VALUE)*

Set the property with name PROPERTY of SYM to VALUE.


- ###**prop-list** : *(prop-list SYM)*

Return a list with all of the properties of SYM.


## Object predicates

Functions for type inspecting. These functions can be used to check whether an object is from a certain type.

- ###**pstring** : *(pstring FORM)*

Return `t` if FORM is a string and `nil` otherwise.


- ###**plist** : *(plist FORM)*

Return `t` if FORM is a list and `nil` otherwise.


- ###**pint** : *(pint FORM)*

Return `t` if FORM is a integer value and `nil` otherwise.


- ###**preal** : *(preal FORM)*

Return `t` if FORM is a real value and `nil` otherwise.


- ###**psym** : *(psym FORM)*

Return `t` if FORM is a symbol and `nil` otherwise.


- ###**pfunction** : *(pfunction FORM)*

Return `t` if FORM is a function and `nil` otherwise.


## Strings

Functions for basic string handling.

- ###**string-length** : *(string-length STRING)*

- ###**string-contains** : *(string-contains STRING SUBSTRING)*

- ###**string-endswith** : *(string-contains STRING SUFFIX)*

- ###**string-startswith** : *(string-contains STRING PREFIX)*

- ###**string-length** : *(string-length STRING)*

- ###**string-capitalize** : *(string-capitalize STRING)*

- ###**string-find** : *(string-find STRING SUBSTRING)*

- ###**string-replace** : *(string-replace STRING SUBSTRING NEWSTRING)*

- ###**string-replaceall** : *(string-replaceall STRING SUBSTRING NEWSTRING)*

- ###**string-split** : *(string-split STRING DELIMETER)*

- ###**string-substring** : *(string-substring STRING FROM TO)*

- ###**string-splitlines** : *(string-splitlines STRING)*

- ###**string-upper** : *(string-upper STRING)*

- ###**string-lower** : *(string-lower STRING)*

- ###**string-strip** : *(string-strip STRING)*

- ###**string-join** : *(string-join STRING [[STRING] ...])*

- ###**char-isalpha** : *(char-isalpha CHAR)*

- ###**char-isdigit** : *(char-isdigit CHAR)*

## Basic Math

Functions that realise simple math operations.

- ###**+** : *(+ [[VALUE]...])*

Retrun the sum of the values of all the provided arguments. 

Example:
```elisp
(+ 10 20 30)
```


- ###**-** : *(- [[VALUE]...])*

Subsract the values of the folloring arguments from the value of the
first argument.

Example:
```elisp
(- 10 20 30)
```


- ###**/** : *(/ [[VALUE]...])(- [[VALUE]...])*

Devide the value of the first argument to the values of the following
arguements.

Example:
```elisp
(/ 10 20 30)
```


- ###***** : *(* [[VALUE]...])*

Retrun the product of the values of all the provided arguments. 

Example:
```elisp
(* 10 20 30)
```


- ###**<** : *(< VALUE1 VALUE2)*

Return `t` if `VALUE1` is less in value than `VALUE2`. Return `nil`
otherwise.


- ###**<=** : *(<= VALUE1 VALUE2)*

Return `t` if `VALUE1` is less or equal in value than `VALUE2`. Return `nil`
otherwise.


- ###**>** : *(> VALUE1 VALUE2)*

Return `t` if `VALUE1` is grater in value than `VALUE2`. Return `nil`
otherwise.


- ###**>=** : *(>= VALUE1 VALUE2)*

Return `t` if `VALUE1` is grater or equal in value than `VALUE2`. Return `nil`
otherwise.


- ###**==** : *(== VALUE1 VALUE2)*

Return `t` if `VALUE1` is equal in value than `VALUE2`. Return `nil`
otherwise.


- ###**!=** : *(!= VALUE1 VALUE2)*

Return `t` if `VALUE1` is not equal in value than `VALUE2`. Return `nil`
otherwise.


- ###**mod** : *(mod VALUE1 VALUE2)*

Return the remainder by devision of `VALUE1` to `VALUE2`


- ###**pow** : *(pow VALUE1 VALUE2)*

Return `VALUE1` to the power of `VALUE2`.


- ###**min** : *(min [[VALUE]...])*

Evaluate the provided arguemnts and return the minimum value.

Example:
```elisp
(min 10 20 30) ; 10
```


- ###**max** : *(max [[VALUE]...])*

Evaluate the provided arguemnts and return the maximum value.

Example:
```elisp
(max 10 20 30) ; 30
```


- ###**round** : *(round VALUE PLACE)*

Round the real value `VALUE` to the `PALCE`-th decimal place.

Example:
```elisp
(round 42.1345 2) ; 42.13
```


## Algorithms

Several functions of basic algorithms for working with lists.

- ###**slice** : *(slice LIST FROM TO)*

Select a subsection of the list `LIST` and return a new list with the
elements of the subsection.

Example:
```elisp
(slice '(10 20 30  40 50 60 70 80 90) 1 5) 
```


- ###**sort** : *(sort LIST)*

Sort the elements of `LIST` in ascending order. This function will
change LIST and won't generate a new object.

Example:
```elisp
(sort '(20 12 2 43 56 10 68 30))
```


- ###**sort** : *(sort LIST)*

Sort the elements of `LIST` in ascending order. This function will
change LIST and won't generate a new object.

Example:
```elisp
(sort '(20 12 2 43 56 10 68 30))
```


- ###**zip** : *(zip [[LIST] ...])*

Take mutliple lists and build pairs of their elements at corresponding
positions. The pairs are put into a new list and this list is
returned.


- ###**filter** : *(filter PREDICATE LIST)*

Collect the elements of `LIST` that fullfil the predicate `PREDICATE`
and return a new list of them.


- ###**any** : *(any PREDICATE LIST)*

Return `t` if at leas one of the elements in `LIST` fulfull the
predicate `PREDICATE`. Return `nil` otherwise.


- ###**all** : *(all PREDICATE LIST)*

Return `t` if all elements in `LIST` fulfull the predicate
`PREDICATE`. Return `nil` otherwise.


