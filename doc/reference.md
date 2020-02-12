
# Table of Contents

1.  [General](#org177dd6c)
2.  [Hello world](#org31f6237)
3.  [Comments](#orgf5086f4)
4.  [Literals](#org9acf507)
    1.  [Char literals](#org9e92ae7)
    2.  [Integer literals](#orgcc3bcee)
    3.  [Real literals](#orgb32b4b6)
    4.  [String literals](#orgc6e9866)
5.  [Symbols](#org4ad28cd)
6.  [Variables](#orgf48f619)
    1.  [Local variables](#org9412709)
    2.  [Setting variables](#orgc1dae33)
7.  [Functions](#orgb74df42)
    1.  [Arguments](#orgefce400)
    2.  [Optional arguments](#org58043ee)
    3.  [Rest arguments](#org71f5ce9)
8.  [Lists](#orgbac9b8d)
9.  [Flow control](#org7eea048)
    1.  [Conditionals](#org1702746)
    2.  [Evaluating expressions](#org592ce79)
    3.  [Switch](#orgce54a1b)
10. [Loops](#org8103b3b)
    1.  [While](#org4379d55)
    2.  [Iterating over list](#orga4fc79e)
11. [Modules](#orgec682ec)



<a id="org177dd6c"></a>

# General

Alisp is a general programming language of the [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)) kind. To quote Wikipedia:

> Lisp is an expression oriented language. Unlike most other languages, no distinction is made between &ldquo;expressions&rdquo; and &ldquo;statements&rdquo;; all code and data are written as expressions. When an expression is evaluated, it produces a value, which can then be embedded into other expressions. Each value can be any data type.

This means that each lisp program (respectively each alisp program) is comprised of nested [s-expression](https://en.wikipedia.org/wiki/S-expression). The have the general form:

    (atom-1 atom-2 atom-3)

Each atomic expression can either be a symbol, a literal or result of the evaluation of another s-expression. An s-expression is evaluated by calling the function which is pointed by the head of the list. In the above case, the function pointed by `atom-1`. The arguments passed to the function will be the values of `atom-2` and `atom-3`. If those are literals, their values are the objects themselves. If they are symbols, the value is the thing pointed by the symbol. Think of symbols as *ids* or variable names (more on them later).

  
Another example for s-expression is:

    (atom-1 atom-2 (atom-3 atom-4 atom-5))

Here the inner most expression is e valuated first, and the result of that becomes the third value of the outermost:

    (atom-1 atom-2 (atom-3 atom-4 atom-5)) -> (atom-1 atom-2 value-1)

A lisp interpreter always evaluates inner most expression first.

  

S-expression build the basic syntax of the Alisp language. This makes the syntax minimal in a sense. 


<a id="org31f6237"></a>

# Hello world

Open your favorite text editor and type the following:

    (println "Hello, Alisp")

Save the contents in a file &ldquo;hello.al&rdquo;. You can execute the script as Alisp program by executing:

    alisp hello.al

in a terminal.


<a id="orgf5086f4"></a>

# Comments

The comment syntax corresponds to the one of [Emacs-lisp](https://www.gnu.org/software/emacs/manual/html_node/elisp/). The symbol `;` is used to start a comment:

    
    ; (println "Hello world")
    ; comment line

In Alisp there is no notion of multi-line comment. For multi-line comment, just use `;;` at the start of each line.


<a id="org9acf507"></a>

# Literals

There are several types of literals in Alisp. When the parser read a literal, it creates an object of the corresponding data-type. Examples for literals in the code:

    42       ;; this is integer value
    42.42    ;; this is real value
    "string" ;; this is real value


<a id="org9e92ae7"></a>

## Char literals

There is a spacial syntax for char literals. While in most languages `'a'` is seen as the char literal for &ldquo;a&rdquo;, in Alisp the `'` character is reserved and thus the syntax for char literals is a bit different. The question mark is used to indicate a char literal. The next symbol after `?` is considered to be a char and an object with this char is constructed.X

    ?a     ;; this is a char value

Special characters can be escaped with `\`: 

    ?\n ;; new line
    ?\t ;; tab
    ?\\ ;; the character "\"


<a id="orgcc3bcee"></a>

## Integer literals

Integers can be written in a straight forward form, just like in any other language. `32` means the integer 32. There are, however, other way to write an integer. For example, a sequence of symbols prefixed with `#x` or `#X` will be read as an integer given in hexadecimal format. One can also write an integer in binary and octal format.

    #b1001 ;; the integer 9 (1001 in binary)
    #B1001 ;; the integer 9 (1001 in binary)
    
    #xAA    ;; the integer 170 (AA in binary)
    #XAA    ;; the integer 170 (AA in binary)
    
    #o17    ;; the integer 15 (17 in ocal)
    #O17    ;; the integer 15 (17 in ocal)

There are several builtin symbols that evaluate to themselves. Those are generally used to represent `true` and `false`.

    t      ;; used as 'true' in most cotexts
    nil    ;; used as 'false' in most cotexts


<a id="orgb32b4b6"></a>

## Real literals

Real numbers can also be given in a simple manner:

    42.42 ;; the value 42.42

Real values can also be written in scientific notation:

    1.3e3   ;; the value 1300.0
    1.3E-3  ;; the value 0.0013


<a id="orgc6e9866"></a>

## String literals

String literals are sequence of symbols enclosed in quotation marks

    "string" ;; this is string with content 'string'

The &ldquo;general&rdquo; rules for string literals apply. You can escape characters with `\` and the literal &ldquo;\\&rdquo; characters is written as &ldquo;\\\\&rdquo; in a string. Characters in string literals can be given in octal or hexadecimal form with `\o` or `\x` followed by the sequence of symbols representing a number. The ASCII character with this number will be read. 32 bit Unicode is also allowed with prefacing a sequence with `\u` and giving a value the [32 bit Unicode standard](https://en.wikipedia.org/wiki/UTF-32).

    "string with new line\n"
    "string with hex char \xA"
    "string with oct char \x27"
    "string with unicode char \uAABE"


<a id="org4ad28cd"></a>

# Symbols

As previously said, symbols can be thought as variable names. In code those are given as a plain sequence of characters. Later we&rsquo;ll see how variables are defined and symbols are used as names. For now, all you need to know is that symbols can contain any character form `A-Z, a-z, 0-9, -, _, +, *, /, =, \ !, $, ?, \, |, @, :, <, >, &, ], +` (characters are separated with commas).

  
Example of symbols:

    this-is-symbol
    sym
    --this-is-symbol
    1--a
    a--1


<a id="orgf48f619"></a>

# Variables

Variables can be defined with the built-in construct `defvar`.

  

For example:

    (defvar sym "initial value" 
      "Documentaion for the varuable sym")

The documentation string is optional. The value can be any valid Alisp value including literal, quoted symbol or list. The value will be evaluated when defining a variable

  

Some more examples:

    (defvar sym-1 "initial value")
    (defvar sym-2 12)
    (defvar sym-3 42.23)
    (defvar sym-3 ?a)

  

Once a variable is defined, the symbol for this variable will evaluate to the value of the variables:

    (defvar sym-1 "initial value")
    (println sym-1) ;; Prints 'initial value'


<a id="org9412709"></a>

## Local variables

    (let ((sym-new-1 "new-variable 1")
          (sym-new-2 "new-variable 2"))
      (println sym-new-1))

    (let* ((sym-new-1 "new-variable 1")
          (sym-new-2 sym-new-1))
      (println sym-new-1))


<a id="orgc1dae33"></a>

## Setting variables

    (let ((sym-new-1 "new-variable 1")
          (sym-new-2 "new-variable 2"))
      (setq sym-1 "new value")
      (println sym-new-1))


<a id="orgb74df42"></a>

# Functions

A function is defined with the built-in construct `(defun ([ARG_LIST]) [DOC_STRING] [[S-EXPRESSION]...])`. The body of the function is just a sequence of expressions that are evaluated on after another. Once defines, a function can be called by placing it at the start of and s-expression.

  

Example of function definition:

    (defun fun-1 ()  ;; defining function
      "Documentaion"
    )
    
    (fun-1)          ;; calling function


<a id="orgefce400"></a>

## Arguments

A function can also defines an argument list. The value of each argument will be bound on function call.

    (defvar fun-1 (a b c)
      "Documentaion"
      (println a)
      (println b)
      (println c))
    
    (fun-1 "a" "b" "c")


<a id="org58043ee"></a>

## Optional arguments

The argument list can contain optional arguments. All arguments after the keyword `&optional` are considered optional. On function call, the optional arguments will either be bound to nil or to the value supplied.

  

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


<a id="org71f5ce9"></a>

## Rest arguments

The argument list can also use the `&rest` keyword in order for the function to capture all of passed arguments on function call.

  

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
    (fun-1 "a" "b" "c" "d" "e" "f" "g")  ;; d -> "d", e -> "e", r -> ("f" "g")


<a id="orgbac9b8d"></a>

# Lists

In ALisp lists are just s-expressions that are not evaluated. To create a list with elements, just quote a regular s-exp:

    '("s1" "s2" "s3" "s3")

  

Lists can contain arbitrary elements:

    '("s1" 42 42.2 a)


<a id="org7eea048"></a>

# Flow control


<a id="org1702746"></a>

## Conditionals

    (if t (println "true") (println "false"))
    (if nil  (println "true") (println "false"))

    (when t (println "true") (println "true"))
    (unless nil (println "false") (println "false"))


<a id="org592ce79"></a>

## Evaluating expressions

    (progn 
        (println "body")
        (println "body")
        (println "body"))


<a id="orgce54a1b"></a>

## Switch


<a id="org8103b3b"></a>

# Loops


<a id="org4379d55"></a>

## While

    (while 't
      (println "body")
      (println "body"))


<a id="orga4fc79e"></a>

## Iterating over list

    (dolist (s '("a" "b" "c"))
      (println s))


<a id="orgec682ec"></a>

# Modules

    (import 'math)
    
    (import 'math :as 'new-math)
    (import 'math :from "./math.al")
    
    (import 'math (sin))
    (import 'math (sin :as new-sin))

    (import 'math)
    
    (math.sin 72.0)

    ((modref 'math 'sin) 72.0)

