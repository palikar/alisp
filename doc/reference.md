- [General](#org39b61a2)
- [Hello world](#org2735434)
- [Comments](#org323e321)
- [Literals](#orgedcf560)
  - [Char literals](#orgda47916)
  - [Integer literals](#org01d492b)
  - [Real literals](#orgcfa0954)
  - [String literals](#org08e42a0)
- [Symbols](#orgb051757)
- [Variables](#org1a4cc1b)
  - [Local variables](#orgbd5c11f)
  - [Setting variables](#orgbdbbe5e)
- [Functions](#org27e9fb6)
  - [Arguments](#org43e2741)
  - [Optional arguments](#org29d52b9)
  - [Rest arguments](#org4843760)
- [Lists](#org9fae8ba)
- [Flow control](#org6ad0e0a)
  - [Evaluating expressions](#org628222f)
  - [Conditionals](#orgca4ce1e)
  - [Switch](#org9ad44fe)
  - [While](#orgdda786d)
  - [Iterating over list](#org8334c9b)
- [Modules](#org22cf0b0)



<a id="org39b61a2"></a>

# General

Alisp is a general programming language of the [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)) kind. To quote Wikipedia:

> Lisp is an expression oriented language. Unlike most other languages, no distinction is made between &ldquo;expressions&rdquo; and &ldquo;statements&rdquo;; all code and data are written as expressions. When an expression is evaluated, it produces a value, which can then be embedded into other expressions. Each value can be any data type.

This means that each lisp program (respectively each alisp program) is comprised of nested [s-expression](https://en.wikipedia.org/wiki/S-expression). The have the general form:

```lisp
(atom-1 atom-2 atom-3)
```

Each atomic expression can either be a symbol, a literal or result of the evaluation of another s-expression. An s-expression is evaluated by calling the function which is pointed by the head of the list. In the above case, the function pointed by `atom-1`. The arguments passed to the function will be the values of `atom-2` and `atom-3`. If those are literals, their values are the objects themselves. If they are symbols, the value is the thing pointed by the symbol. Think of symbols as *ids* or variable names (more on them later).

Another example for s-expression is:

```lisp
(atom-1 atom-2 (atom-3 atom-4 atom-5))
```

Here the inner most expression is e valuated first, and the result of that becomes the third value of the outermost:

```
(atom-1 atom-2 (atom-3 atom-4 atom-5)) -> (atom-1 atom-2 value-1)
```

A lisp interpreter always evaluates inner most expression first.



S-expression build the basic syntax of the Alisp language. This makes the syntax minimal in a sense.


<a id="org2735434"></a>

# Hello world

Open your favorite text editor and type the following:

```emacs-lisp
(println "Hello, Alisp")
```

Save the contents in a file &ldquo;hello.al&rdquo;. You can execute the script as Alisp program by executing:

```sh
alisp hello.al
```

in a terminal.

To briefly explain the example:

-   `(println "Hello, Alisp")` is a list of two atoms &#x2013; the symbol `println` and the string `"Hello, Alisp"`.
-   As this is a list form, the interpreter will execute the function pointed by the symbol at the head of the list &#x2013; `println` &#x2013; and the arguments for the function will be the rest of the list. In this case, `"Hello, Alisp"` is the only argument.
-   `println` is a built in function that writes a string to the standard output, so &ldquo;Hello, Alisp&rdquo; gets printed out to the terminal.


<a id="org323e321"></a>

# Comments

The comment syntax corresponds to the one of [Emacs-lisp](https://www.gnu.org/software/emacs/manual/html_node/elisp/). The symbol `;` is used to start a comment:

```lisp

; (println "Hello world")
; comment line
```

In Alisp there is no notion of multi-line comment. For multi-line comment, just use `;;` at the start of each line.


<a id="orgedcf560"></a>

# Literals

There are several types of literals in Alisp. When the parser read a literal, it creates an object of the corresponding data-type. Examples for literals in the code:

```elisp
42       ;; this is integer value
42.42    ;; this is real value
"string" ;; this is real value
```


<a id="orgda47916"></a>

## Char literals

There is a spacial syntax for char literals. While in most languages `'a'` is seen as the char literal for &ldquo;a&rdquo;, in Alisp the `'` character is reserved and thus the syntax for char literals is a bit different. The question mark is used to indicate a char literal. The next symbol after `?` is considered to be a char and an object with this char is constructed.X

```emacs-lisp
?a     ;; this is a char value
```

Special characters can be escaped with `\`:

```emacs-lisp
?\n ;; new line
?\t ;; tab
?\\ ;; the character "\"
```


<a id="org01d492b"></a>

## Integer literals

Integers can be written in a straight forward form, just like in any other language. `32` means the integer 32. There are, however, other way to write an integer. For example, a sequence of symbols prefixed with `#x` or `#X` will be read as an integer given in hexadecimal format. One can also write an integer in binary and octal format.

```emacs-lisp
#b1001 ;; the integer 9 (1001 in binary)
#B1001 ;; the integer 9 (1001 in binary)

#xAA    ;; the integer 170 (AA in binary)
#XAA    ;; the integer 170 (AA in binary)

#o17    ;; the integer 15 (17 in ocal)
#O17    ;; the integer 15 (17 in ocal)
```

There are several builtin symbols that evaluate to themselves. Those are generally used to represent `true` and `false`.

```emacs-lisp
t      ;; used as 'true' in most cotexts
nil    ;; used as 'false' in most cotexts
```


<a id="orgcfa0954"></a>

## Real literals

Real numbers can also be given in a simple manner:

```elisp
42.42 ;; the value 42.42
```

Real values can also be written in scientific notation:

```emacs-lisp
1.3e3   ;; the value 1300.0
1.3E-3  ;; the value 0.0013
```


<a id="org08e42a0"></a>

## String literals

String literals are sequence of symbols enclosed in quotation marks

```emacs-lisp
"string" ;; this is string with content 'string'
```

The &ldquo;general&rdquo; rules for string literals apply. You can escape characters with `\` and the literal &ldquo;\\&rdquo; characters is written as &ldquo;\\\\&rdquo; in a string. Characters in string literals can be given in octal or hexadecimal form with `\o` or `\x` followed by the sequence of symbols representing a number. The ASCII character with this number will be read. 32 bit Unicode is also allowed with prefacing a sequence with `\u` and giving a value the [32 bit Unicode standard](https://en.wikipedia.org/wiki/UTF-32).

```emacs-lisp
"string with new line\n"
"string with hex char \xA"
"string with oct char \x27"
"string with unicode char \uAABE"
```


<a id="orgb051757"></a>

# Symbols

As previously said, symbols can be thought as variable names. In code those are given as a plain sequence of characters. Later we&rsquo;ll see how variables are defined and symbols are used as names. For now, all you need to know is that symbols can contain any character from `A-Z, a-z, 0-9, -, _, +, *, /, =, \ !, $, ?, \, |, @, :, <, >, &, ], +` (characters are separated with commas).

Example of symbols:

```
this-is-symbol
sym
--this-is-symbol
1--a
a--1
```


<a id="org1a4cc1b"></a>

# Variables

Variables can be defined with the built-in construct `defvar`.



For example:

```emacs-lisp
(defvar sym "initial value"
  "Documentaion for the varuable sym")
```

The documentation string is optional. The value can be any valid Alisp value including literal, quoted symbol or list. The value will be evaluated when defining a variable



Some more examples:

```emacs-lisp
(defvar sym-1 "initial value")
(defvar sym-2 12)
(defvar sym-3 42.23)
(defvar sym-3 ?a)
```



Once a variable is defined, the symbol for this variable will evaluate to the value of the variables:

```emacs-lisp
(defvar sym-1 "initial value")
(println sym-1) ;; Prints 'initial value'
```


<a id="orgbd5c11f"></a>

## Local variables

`defvar` defines a global variable that will live till the end of the program. It is also possible to create local variables that are valid only within a scope. This is done through the `let` form. The general form is `(let ([(var-name var-value)]...) BODY)`. In this case, the forms in `BODY` will be executed after the variables in the list are bound with the respective values. The variables will be accessible only in the body and will be destroyed after the `let` form has finish its execution.

Example:

```emacs-lisp
(let ((sym-new-1 "new-variable 1")    ; sym-new-1 will be bound to "new-variable 1"
      (sym-new-2 "new-variable 2"))   ; sym-new-2 will be bound to "new-variable 2"
  (println sym-new-1))                ; -> new-variable 1
```

To note is that in the above example, one cannot use the value of `sym-new-1` in the initialization of `sym-new-2`. However, if this is necessary, the `let*` form exists for this exact reason. With `let*` you can do something like:

```emacs-lisp
(let* ((sym-new-1 "new-variable 1")
      (sym-new-2 sym-new-1))
  (println sym-new-1))
```


<a id="orgbdbbe5e"></a>

## Setting variables

The value of and variable can be changed through the `setq` form. It takes a variable as a first argument and a new value as its second argument. The variable has to be bound before either through `let=/=let*` or `defvar`.

Example:

```emacs-lisp
(let ((sym-new-1 "new-variable 1")
      (sym-new-2 "new-variable 2"))
  (setq sym-new-1 "new value")   ; set sym-new-1 to "new value"
  (println sym-new-1))       ; -> new value
```


<a id="org27e9fb6"></a>

# Functions

A function is defined with the built-in construct `(defun ([ARG_LIST]) [DOC_STRING] [[S-EXPRESSION]...])`. The body of the function is just a sequence of expressions that are evaluated one after another Once defined, a function can be called by placing it at the start of an s-expression.

Example of function definition:

```emacs-lisp
(defun fun-1 ()     ;; defining a function
  "Documentaion"
  (println "Hello from function"))

(fun-1)             ;; calling defined function
```


<a id="org43e2741"></a>

## Arguments

A function can also define an argument list. The value of each argument will be bound on function call.

```emacs-lisp
(defvar fun-1 (a b c)
  "Documentaion"
  (println a)
  (println b)
  (println c))

(fun-1 "a" "b" "c")
```


<a id="org29d52b9"></a>

## Optional arguments

The argument list can contain optional arguments. All arguments after the keyword `&optional` are considered optional. On function call, the optional arguments will either be bound to nil or to the value supplied.

```emacs-lisp
(defun fun-1 (a b c &optional d e)
  "Documentaion"
  (println a)
  (println b)
  (println c)
  (println d)
  (println e))

(fun-1 "a" "b" "c" "d")      ;; d -> "d", e -> nil
(fun-1 "a" "b" "c" "d" "e")  ;; d -> "d", e -> "e"
(fun-1 "a" "b" "c")          ;; d -> nil, e -> nil
```


<a id="org4843760"></a>

## Rest arguments

The argument list can also use the `&rest` keyword in order for the function to capture all of passed arguments on function call.

```emacs-lisp
(defun fun-1 (a b c &optional d e &rest r)
  "Documentaion"
  (println a)
  (println b)
  (println c)
  (println d)
  (println e))

(fun-1 "a" "b" "c" "d")              ;; d -> "d", e -> nil
(fun-1 "a" "b" "c" "d" "e")          ;; d -> "d", e -> "e"
(fun-1 "a" "b" "c")                  ;; d -> nil, e -> nil
(fun-1 "a" "b" "c" "d" "e" "f")      ;; d -> "d", e -> "e", r -> ("f")
(fun-1 "a" "b" "c" "d" "e" "f" "g")  ;; d -> "d", e -> "e", r -> ("f" "g")
```


<a id="org9fae8ba"></a>

# Lists

In ALisp lists are just s-expressions that are not evaluated. To create a list with elements, just quote a regular s-exp:

```emacs-lisp
'("s1" "s2" "s3" "s3")
```

This is equivalent to

```emacs-lisp
(quote ("s1" "s2" "s3" "s3"))
```

and quote just returns it&rsquo;s first argument without evaluating it. This means that be evaluating `'("s1" "s2" "s3" "s3")`, you simply get `("s1" "s2" "s3" "s3")`. The list can be then manipulated with the list-functions that alisp provides.

Lists can also contain arbitrary elements:

```emacs-lisp
'("s1" 42 42.2 a)   ; -> ("s1" 42 42.2 a)
```


<a id="org6ad0e0a"></a>

# Flow control

As any other language, alisp provides several constructs for controlling the flow of the execution of a program. Those constructs include conditional statements and loops. The next sections present and explain them.


<a id="org628222f"></a>

## Evaluating expressions

In certain situations you&rsquo;ll want to evaluate several forms at a place where a single form is required. For those situations, the `progn` from is provided. `progn` simply evaluates all of its arguments and returns the value of the last one. It can be used anywhere.

```emacs-lisp
(progn
    (println "body")
    (println "body")
    42)              ; -> 42
```


<a id="orgca4ce1e"></a>

## Conditionals

The basic conditional statement of alisp is the `if` form &#x2013; `(if COND THEN ELSE)`. If the form `COND` evaluates to something true, the **single** `THEN` form is evaluated. If `COND` evaluates to false, the `ELSE` form is evaluated. The ELSE part can be actually be a sequence of forms that will get evaluated. If you want to evaluate several forms in the THEN part, you&rsquo;ll have to use the `prog` form.

Example:

```emacs-lisp

(if t (println "true") (println "false"))
(if nil (progn
          (println "true")
          (println "true"))
  (println "false"))

(if nil  (println "true")
  (println "false")
  (println "false agian"))

```

For convince there are also the `when` and `unlsess` forms. Both take a condition and a list of forms that are to be evaluated. For `when`, the forms will be evaluated if the condition is true, and for else - if the condition is false. Both return the value of the last evaluated form.

```emacs-lisp
(when t (println "true") (println "true again"))

(unless nil (println "false") (println "false again"))
```


<a id="org9ad44fe"></a>

## Switch

In alisp a switch statement is acts a little bit differently than the usual. The `cond` form is used to choose among several alternatives. It takes an arbitrary number of clauses of the form `(CONDITION BODY-FROMS)`. `cond` will execute the body of the first form for which the condition evaluates to true. The clauses are checked in the order they are given in.

Example:

```emacs-lisp
(cond
 ((!= 10 10) (println "10 == 10 is false"))
 ((== 10 10) (println "10 == 10 is true"))
 (t (println "executes always")))
```

If the condition of a clause is simple `t`, it is essentially like the default clause in a switch expression. If it is at the end, it will be executed if none of the other conditions were evaluated to something truthy.


<a id="orgdda786d"></a>

## While

The while statement is provided by the `while` form &#x2013; `(while COND BODY)`. Body will be executed repeatedly until the condition evaluates to true.

Example:

```emacs-lisp
(while 't
  (println "body")
  (println "body"))
```


<a id="org8334c9b"></a>

## Iterating over list

The equivalent of a ranged-for loop in alisp is the `dolist` form &#x2013; `(dolist (EL LIST) BODY)`. Body will be executed for each element in LIST. `EL` should be a valid symbol name and it will be bound to echo element of the list for the corresponding execution.

Example:

```emacs-lisp
(dolist (s '("a" "b" "c"))
  (print s "-"))
;  -> a-b-c-
```


<a id="org22cf0b0"></a>

# Modules

```emacs-lisp
(import 'math)

(import 'math :as 'new-math)
(import 'math :from "./math.al")

(import 'math (sin))
(import 'math (sin :as new-sin))
```

```emacs-lisp
(import 'math)

(math.sin 72.0)
```

```emacs-lisp
((modref 'math 'sin) 72.0)
```
