/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
n the Free Software Foundation; either version 2 of the License, or
 (at your option) any prior version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#pragma once

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_macros.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_env.hpp"


namespace alisp
{


/*   ____                _              _        */
/*  / ___|___  _ __  ___| |_ __ _ _ __ | |_ ___  */
/* | |   / _ \| '_ \/ __| __/ _` | '_ \| __/ __| */
/* | |__| (_) | | | \__ \ || (_| | | | | |_\__ \ */
/*  \____\___/|_| |_|___/\__\__,_|_| |_|\__|___/ */


DEFVAR(Qt, Vt, "t", make_object(Qt, "Used to represent truthy value."));
DEFVAR(Qnil, Vnil, "nil", make_object(Qnil, "Used to represent falsey value."));

DEFVAR(Qmodpaths, Vmodpaths, "--modpaths--", make_object("", AL_EXTRA_MODPATHS));
DEFVAR(Qcurrent_module, Vcurrent_module, "--module--", make_string(""));
DEFVAR(Qcommand_line_args, Vcommand_line_args, "--argv--", make_list());
DEFVAR(Qlicense, Vlicense, "--al-license--", make_string(AL_LICENSE));

DEFVAR(Qload_signal, Vload_signal, "load-signal", make_symbol("load-signal"));
DEFVAR(Qdefun_signal, Vdefun_signal, "defun-signal", make_symbol("defun-signal"));

DEFSYM(Qoptional, "&optional", R"(Used in an argument list to signify that the next arguments are optional.)");
DEFSYM(Qrest, "&rest", R"(Used in an argument list to signify that the next arguemnt should be
bound to any left arguments by function call.)");
DEFSYM(Qcomma, ",", R"(Used by the backquote syntax in order to evaluate and expression.)");
DEFSYM(Qcomma_at, ",@", R"(Used by the backquote syntax in order to slice in a list inside of
another list.)");


/*  _                                                ____                _                   _        */
/* | |    __ _ _ __   __ _ _   _  __ _  __ _  ___   / ___|___  _ __  ___| |_ _ __ _   _  ___| |_ ___  */
/* | |   / _` | '_ \ / _` | | | |/ _` |/ _` |/ _ \ | |   / _ \| '_ \/ __| __| '__| | | |/ __| __/ __| */
/* | |__| (_| | | | | (_| | |_| | (_| | (_| |  __/ | |__| (_) | | | \__ \ |_| |  | |_| | (__| |_\__ \ */
/* |_____\__,_|_| |_|\__, |\__,_|\__,_|\__, |\___|  \____\___/|_| |_|___/\__|_|   \__,_|\___|\__|___/ */
/*                   |___/             |___/                                                          */


DEFUN(import, "import", R"((import MODULE [:file file] [:all] [( [(SYM MAPPED)]... )])

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
)");

DEFUN(modref, "modref", R"((modref MODUE [[MODUE] ...] SYMBOL [[symbol] ...] )

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
)");


DEFUN(defun, "defun", R"((defun NAME (ARGLIST) [DOC] [BODY])

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
 )");
DEFUN(defmacro, "defmacro", R"((defmacro NAME (ARGLIST) [DOC] BODY)

Define a new macro with a name `NAME` in the current
module. `ARGLIST` should be a valid argument list definition. `DOC` is
an optional docscring and `BODY` is a list of forms to be evaluated
when the function is called. As oppose to a function, the arguments of
a macro are not evaluated when the macro is called.

Example:
```elisp
(defmacro inc (x)
    `(setq x (+ 1 ,x)))
```
)");

DEFUN(defvar, "defvar", R"((defvar NAME VALUE [DOC])

Define a new variable with a name `NAME` in the current
module. `VALUE` is the initial value of the variable and `DOC` is an
optional docstring. A variable *has* to be defines before used. A
variable defined through `defvar` will live till the end of the
program.

Example:
```elisp
(defvar new-var 42)
```

)");

DEFUN(eval, "eval", R"((eval FORM)

Evaluate the form `FORM`. The usual form for evaluation apply.
)");

DEFUN(setq, "setq", R"((setq SYMBOL VALUE [[SYMBOL VALUE] ... ])

Set the value of the variable pointed by `SYMBOL` to
`VALUE`. `SYMBOL` will not be evaluated. `setq` can also be used to
set the value of multiple variables at once. All of the variables
should be defined beforehand.

Example:
```elisp
(defvar new-var 42)
(setq new-var 43)
```
)");


DEFUN(set, "set", R"(((set FORM VALUE))

Set the value of the variable pointed by `FORM` to `VALUE`. `FORM`
will be evaluated and should return a symbol. `setq` can also be used
to set the value of multiple variables at once. All of the variables
should be defined beforehand.

Example:
```elisp
(defvar new-var 42)
(set 'new-var 43)
```
)");

DEFUN(quote, "quote", R"((quote FORM)

Return `FORM`, without evaluating it. `(quote x)` yields ‘x’. `x is a
syntactic sugar for this function.
)");

DEFUN(function, "function", R"((funtion OBJECT)

Return `OBJECT`, without evaluating it but setting its function flag
to true. `function` should be used to quote lambdas and other
callables.
)");

DEFUN(lambda, "lambda", R"((lambda (ARGLIST) BODY)


)");

DEFUN(if, "if", R"((if CONDITION THEN ELSE)

Evaluate `CONDITION` and if its value is *truthy*, evaluate and return
the value of `THEN`. Otherwise evaluate and return the value of
`ELSE`.
)");

DEFUN(while, "while", R"((while CONDITION BODY)

Evaluate `BODY` as long as `CONDITION` evaluates to a value that is
*truthy*. `while` returns `nil`.
)");

DEFUN(dolist, "dolist", R"((dolist (SYMBOL LIST) BODY)

Evaluate `BODY` for each symbol in `LIST` while bonding the respective
element to `SYMBOL`.

Example:
```elisp
(dolist (s '(1 2 3 4))
   (println s))
```

)");

DEFUN(cond, "cond", R"((cond [ ( [CODITION BODY] ) ... ])

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
)");

DEFUN(unless, "unless", R"((unless CONDITION BODY)

Evaluate `BODY` if `CONDITION` evaluates to *falsey* value.
)");

DEFUN(when, "when", R"((when CONDITION BODY)

Evaluate `BODY` if `CONDITION` evaluates to *truthy* value.
)");

DEFUN(progn, "progn", R"((progn BODY)

Evaluate the forms in `BODY` sequentially and return the value of the
last one.
)");

DEFUN(let, "let", R"((let ([[VAR]...] [[(VAR VALUE)] ...] ) BODY)

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
)");

DEFUN(letx, "let*", R"((let* ([[VAR]...] [[(VAR VALUE)] ...] ) BODY)

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
)");

DEFUN(funcall, "funcall", R"((funcall SYMBOL LIST)

Call the function pointed by `SYMBOL` and pass the symbols in `LIST`
as arguments.
)");

DEFUN(backquote, "backquote", R"((backquote LIST)

Backquote the list `LIST`. `LIST is syntactic sugar for this function.

Example:
```elisp

`(val-1 ,val-2 ,@(val-3 val-3 )) ; '(val-1 (eval val-2) val-3 val-3)
```
)");

DEFUN(signal, "signal", R"((signal SYMBOL LIST)

Emit a signal with symbol `SYMBOL` and some arbitrary data stores in
`LIST`.

Example:
```elisp
(signal 'my-error '("there was an error" 42))
```
)");

DEFUN(return, "return", R"((return [FROM])

Return an optional value from a function. If `FROM` is given, it will
be evaluated and its value will be the return value of the
function. Otherwise `nil` is the returned value.

)");

DEFUN(break, "break", R"((break)

Break out of a loop.
)");

DEFUN(continue, "continue", R"((continue)

Start a new loop iteration.
)");

DEFUN(exit, "exit", R"((exit [FORM])

Exit the program. If `FORM` is given, its value will be the return
code of the process. Otherwise the return code will be 0.
)");

DEFUN(assert, "assert", R"((assert FORM)

Assert that the value of `FORM` is *truthy*. If not, an assert signal
is emitted.

Example:
```elisp
(assert t)
(assert nil)
```

)");

DEFUN(eq, "eq", R"((equal FORM1 FORM2)

Check if the values of `FORM1` and `FORM2` point to the same
object. If the values are ints or doubles, the actual values will be
tested for equality and `t` is return if they are equal. In all other
cases, return `t` only if the two objects are the same i.e a change in
one of the objects, will also change the other one.
)");

DEFUN(equal, "equal", R"((equal FORM1 FORM2)

Return `t` if `FORM1`a and `FORM2` have the same value. Return `nil`
otherwise.
)");

DEFUN(sym_list, "symbols-list", R"((symbols-list [PACKAGE])

Return a list the symbols that are defines in PACKAGE.
)");

// /*  ____       _       _   _              */
// /* |  _ \ _ __(_)_ __ | |_(_)_ __   __ _  */
// /* | |_) | '__| | '_ \| __| | '_ \ / _` | */
// /* |  __/| |  | | | | | |_| | | | | (_| | */
// /* |_|   |_|  |_|_| |_|\__|_|_| |_|\__, | */
// /*                                 |___/  */


DEFUN(print, "print", R"((print FORM [[FORM] ...])

Print the value of VALUE of form on the standard output stream.
)");

DEFUN(println, "println", R"((println VALUE [[VALUE] ...])

Print the value of VALUE of form on the standard output stream and put a new
line character.
)");

DEFUN(eprint, "eprint", R"((eprint VALUE [[VALUE] ...])

Print the value of VALUE of form on the standard error stream.
)");

DEFUN(eprintln, "eprintln", R"((eprintln VALUE [[VALUE] ...])

Print the value of VALUE of form on the standard error stream and put a new
line character.
)");

DEFUN(dump, "dump", R"((dump FORM))");
DEFUN(dumpstack, "dumpstack", R"((dumpstack)

Print a formatted version of the current state of the execution
environment. This is where the stack frames and the scopes live.

)");
DEFUN(dumpcallstack, "dumpcallstack", R"((dumpcallstack)

Print a formatted version of the current call stack on the standard
output. This function is meant for debugging.
)");

DEFUN(dumplicense, "dumplicense", R"((dumplicense)

Print the license of Alisp on the standard output.

)");

DEFUN(dumpcredits, "dumpcredits", R"((dumpcredits)

Print the contributors information for Alisp on the standard output.
)");

DEFUN(read_line, "read-line", R"((read-line)

Read a single line form the standard input stream and return it.
)");


/*  _     _     _        */
/* | |   (_)___| |_ ___  */
/* | |   | / __| __/ __| */
/* | |___| \__ \ |_\__ \ */
/* |_____|_|___/\__|___/ */


DEFUN(mapc, "mapc", R"((mapc FUNCTION LIST)

Call FUNCTION for each element of LIST. The element is passed as an
argument to the function. `mapc` return `t` and its executed only for side effects.

Example:
```elisp
(mapc println '(1 2 3 4 5))
(mapc (lambda (x) (print x)) '(1 2 3 4 5))
```
)");

DEFUN(mapcar, "mapcar", R"((mapcar FUNCTION LIST)


Call FUNCTION for each element of LIST while collecting the results of
the calls and building a new list. The new list is returned.

Example:
```elisp
(mapcar (lambda (x) (+ x 1)) '(1 2 3 4 5))
```
)");

DEFUN(car, "car", R"((mapc LIST)

Return the fist element of the list LIST.
 
Example:
```elisp
(car '(1 2 3 4 5))
```
)");

DEFUN(cons, "cons", R"((cons LIST)

Return a sublist of LIST with all of its elements but the first one.

Example:
```elisp
(cons '(1 2 3 4 5))
```
)");

DEFUN(head, "head", R"((head LIST)

Return the fist element of the list LIST.

Example:
```elisp
(head '(1 2 3 4 5))
```
)");

DEFUN(last, "last", R"((last LIST)

Return the last element of the list LIST.

Example:
```elisp
(last '(1 2 3 4 5))
```
)");

DEFUN(init, "init", R"((init LIST)

Return a sublist of LIST with all of its elements but the last one.

Example:
```elisp
(init '(1 2 3 4 5))
```
)");

DEFUN(tail, "tail", R"((tail LIST)

Return a sublist of LIST with all of its elements but the first one.

Example:
```elisp
(tail '(1 2 3 4 5))
```
)");

DEFUN(push, "push", R"((push LIST ELEMENT)

Add ELEMENT to the end of the LIST. This function changes the LIST
rather than to create a new one.

Example:
```elisp
(push '(1 2 3 4 5) 6)
```
)");

DEFUN(shove, "shove", R"((shove LIST ELEMENT)

Add ELEMENT at the beginning of the LIST. This function changes the LIST
rather than to create a new one.

Example:
```elisp
(shove '(1 2 3 4 5) 0)
```
)");

DEFUN(delete, "delete", R"((delete LIST ELEMENT)

Remove an element from LIST that is equal (with `equal`) to
ELEMENT. This function operates inplace, so list is changed and no new
list is created. 
Example:
```elisp
(delete '(1 2 3 4 5) 5)
```
)");

DEFUN(remove, "remove", R"((remove LIST ELEMENT)

Remove an element from LIST that is equal (with `equal`) to
ELEMENT. This function __does not__ operate inplace, so a new list is
created.

Example:
```elisp
(delete '(1 2 3 4 5) 5)
```

)");

DEFUN(nth, "nth", R"((nth LIST INDEX)

Return the element of LIST that is at position INDEX.

Example:
```elisp
(nth '(1 2 3 4 5) 1)
```
)");

DEFUN(range, "range", R"((range FROM TO STEP)

Generate the range of numbers [FROM, TO) with a step STEP. All of
the arguments must be ints.

Example:
```elisp
(range 0 100 2)
```
)");
DEFUN(length, "length", R"((length LIST)

Return the number of elements in LIST.

Example:
```elisp
(length '(1 2 3 4 5))
```
)");

DEFUN(clear, "clear", R"((clear LIST)

Remove all elements from `LIST`.

Example:
```elisp
(clear '(1 2 3 4 5))
```
)");

DEFUN(list, "list", R"((list [[ELEMENT] ...])

Creates a list with arbitrary  number of elements.

Example:
```elisp
(list 1 2 3)
```
)");

DEFUN(contains, "contains", R"((contains LIST ELEMENT)

Check if LIST contains ELEMENT (according to `equal`). If yes, return
`t`, and `nil` otherwise.

Example:
```elisp
(contains '(1 2 3 4 5) 3)
```
)");


/*  ____  _                                 */
/* / ___|| |_ _ __ ___  __ _ _ __ ___  ___  */
/* \___ \| __| '__/ _ \/ _` | '_ ` _ \/ __| */
/*  ___) | |_| | |  __/ (_| | | | | | \__ \ */
/* |____/ \__|_|  \___|\__,_|_| |_| |_|___/ */

DEFUN(stream, "stream", R"((stream [:from-string STRING] [:from-file FILE]))");

DEFUN(close_stream, "stream-close", R"((stream-close STREAM))");

DEFUN(with_cout, "with-cout", R"((with-cout STREAM))");

DEFUN(with_cin, "with-cin", R"((with-cin STREAM))");

DEFUN(stream_content, "stream-content", R"(((content STREAM))");

DEFUN(stream_write, "stream-write", R"((stream-write VALUE))");

DEFUN(stream_write_line, "stream-write-line", R"((stream-write-line VALUE))");

DEFUN(stream_write_lines, "stream-write-lines", R"((stream-write-line VALUE [[VALUE] ...]))");

DEFUN(stream_read, "stream-read", R"((stream-read))");

DEFUN(stream_read_line, "stream-read-line", R"((stream-read-line))");

DEFUN(stream_read_lines, "stream-read-lines", R"((stream-read-lines))");


/*  _____ _ _            */
/* |  ___(_) | ___  ___  */
/* | |_  | | |/ _ \/ __| */
/* |  _| | | |  __/\__ \ */
/* |_|   |_|_|\___||___/ */

DEFUN(file_open, "file-open", R"((file-open PATH [:out] [:in])

Open a file from the filesystem. If `:out` is specified, the file will
be opened for writing. If `:in` is specified the file will be opened
for reading. Provind both keyword arguemnts is also possible. The
function returns a resrouse descriptor that can be used to access the underlying file.

```elisp
(defvar file-1 (file-open "./file-1.al" :out)
(defvar file-2 (file-open "./file-2.al" :in)
```
)");

DEFUN(file_close, "file-close", R"((file-close FILE)

Close an opened file and release the file descriptor. `FILE` should be
a valid resource descriptor pointing to a file.

```elisp
(defvar file-1 (file-open "./file-1.al" :out)
(file-close file-1)
```
)");

DEFUN(file_read_line, "file-read-line", R"((file-read-line FILE)

Read a single line from a file an return it.`FILE` should be a valid
resource descriptor pointing to a file. This function also moves the
position of the underlying file stream after the read line.

)");

DEFUN(file_write_line, "file-write-line", R"((file-write-line FILE STRING)

Write `STRING` to a file, followed by a new line. `FILE` should be
a valid resource descriptor pointing to a file.

)");

DEFUN(file_has_more, "file-has-more", R"((file-has-more FILE)

Check if there is more data to read of a `FILE`. `FILE` should be a
valid resource descriptor pointing to a file. Return `t` if the stream
pointer has reached to the end of the file and `nil` otherwise.
)");


/*  ____                       */
/* |  _ \ _ __ ___  _ __  ___  */
/* | |_) | '__/ _ \| '_ \/ __| */
/* |  __/| | | (_) | |_) \__ \ */
/* |_|   |_|  \___/| .__/|___/ */
/*                 |_| */


DEFUN(prop_get, "prop-get", R"((prop-get SYM PROPERTY)

Return the property with name PROPERTY of SYM.
)");

DEFUN(prop_set, "prop-set", R"((prop-set SYM PROPERTY VALUE)

Set the property with name PROPERTY of SYM to VALUE.
)");

DEFUN(prop_exists, "prop-exists", R"((prop-get SYM PROPERTY)

Return `t` if SYM has the property PROPERTY and `nil` otherwise.
)");

DEFUN(prop_list, "prop-list", R"((prop-list SYM)

Return a list with all of the properties of SYM.
)");


/*  ____               _ _           _             */
/* |  _ \ _ __ ___  __| (_) ___ __ _| |_ ___  ___  */
/* | |_) | '__/ _ \/ _` | |/ __/ _` | __/ _ \/ __| */
/* |  __/| | |  __/ (_| | | (_| (_| | ||  __/\__ \ */
/* |_|   |_|  \___|\__,_|_|\___\__,_|\__\___||___/ */


DEFUN(pfunction, "pfunction", R"((pfunction FORM)

Return `t` if FORM is a function and `nil` otherwise.
)");

DEFUN(psym, "psym", R"((psym FORM)

Return `t` if FORM is a symbol and `nil` otherwise.
)");

DEFUN(plist, "plist", R"((plist FORM)

Return `t` if FORM is a list and `nil` otherwise.
)");

DEFUN(pint, "pint", R"((pint FORM)

Return `t` if FORM is a integer value and `nil` otherwise.
)");

DEFUN(preal, "preal", R"((preal FORM)

Return `t` if FORM is a real value and `nil` otherwise.
)");

DEFUN(pstring, "pstring", R"((pstring FORM)

Return `t` if FORM is a string and `nil` otherwise.
)");

/*  _                _       */
/* | |    ___   __ _(_) ___  */
/* | |   / _ \ / _` | |/ __| */
/* | |__| (_) | (_| | | (__  */
/* |_____\___/ \__, |_|\___| */
/*             |___/         */

DEFUN(or, "or", R"((or [[VALUE]...])

Return `t` if at least one of the arguments evaluates to a truthy
value. The arguments are lazily evaluated.

)");
DEFUN(and, "and", R"((and [[VALUE]...])

Return `t` if all of the arguments evaluates to a truthy
value. The arguments are lazily evaluated.
)");

DEFUN(not, "not", R"((not FORM)

Return `t` if FORM evaluate to a falsey value and `nil` otherwise. 
)");


/*  __  __       _   _      */
/* |  \/  | __ _| |_| |__   */
/* | |\/| |/ _` | __| '_ \  */
/* | |  | | (_| | |_| | | | */
/* |_|  |_|\__,_|\__|_| |_| */

DEFUN(plus, "+", R"((+ [[VALUE]...])

Retrun the sum of the values of all the provided arguments. 

Example:
```elisp
(+ 10 20 30)
```
)");

DEFUN(minus, "-", R"((- [[VALUE]...])

Subsract the values of the folloring arguments from the value of the
first argument.

Example:
```elisp
(- 10 20 30)
```
)");

DEFUN(dev, "/", R"((/ [[VALUE]...])(- [[VALUE]...])

Devide the value of the first argument to the values of the following
arguements.

Example:
```elisp
(/ 10 20 30)
```
)");

DEFUN(multiply, "*", R"((* [[VALUE]...])

Retrun the product of the values of all the provided arguments. 

Example:
```elisp
(* 10 20 30)
```
)");

DEFUN(gt, ">", R"((> VALUE1 VALUE2)

Return `t` if `VALUE1` is grater in value than `VALUE2`. Return `nil`
otherwise.
)");

DEFUN(geq, ">=", R"((>= VALUE1 VALUE2)

Return `t` if `VALUE1` is grater or equal in value than `VALUE2`. Return `nil`
otherwise.
)");

DEFUN(lt, "<", R"((< VALUE1 VALUE2)

Return `t` if `VALUE1` is less in value than `VALUE2`. Return `nil`
otherwise.
)");

DEFUN(leq, "<=", R"((<= VALUE1 VALUE2)

Return `t` if `VALUE1` is less or equal in value than `VALUE2`. Return `nil`
otherwise.
)");

DEFUN(eq_math, "==", R"((== VALUE1 VALUE2)

Return `t` if `VALUE1` is equal in value than `VALUE2`. Return `nil`
otherwise.
)");

DEFUN(neq, "!=", R"((!= VALUE1 VALUE2)

Return `t` if `VALUE1` is not equal in value than `VALUE2`. Return `nil`
otherwise.
)");

DEFUN(mod, "mod", R"((mod VALUE1 VALUE2)

Return the remainder by devision of `VALUE1` to `VALUE2`
)");

DEFUN(pow, "pow", R"((pow VALUE1 VALUE2)

Return `VALUE1` to the power of `VALUE2`.
)");

DEFUN(round, "round", R"((round VALUE PLACE)

Round the real value `VALUE` to the `PALCE`-th decimal place.

Example:
```elisp
(round 42.1345 2) ; 42.13
```
)");

DEFUN(leftshift, "<<", R"((<< VALUE1 VALUE2)

Shift the bits of `VALUE` to the left `VALUE2` times.

Example:
```elisp
(>> 16 2)   ;  4
```
)");

DEFUN(rightshift, ">>", R"((>> VALUE1 VALU2)

Shift the bits of `VALUE` to the right `VALUE2` times.

Example:
```elisp
(<< 2 2)   ;  8
```
)");

DEFUN(min, "min", R"((min [[VALUE]...])

Evaluate the provided arguemnts and return the minimum value.

Example:
```elisp
(min 10 20 30) ; 10
```
)");

DEFUN(max, "max", R"((max [[VALUE]...])

Evaluate the provided arguemnts and return the maximum value.

Example:
```elisp
(max 10 20 30) ; 30
```
)");


/*  ____  _        _                  */
/* / ___|| |_ _ __(_)_ __   __ _ ___  */
/* \___ \| __| '__| | '_ \ / _` / __| */
/*  ___) | |_| |  | | | | | (_| \__ \ */
/* |____/ \__|_|  |_|_| |_|\__, |___/ */
/*                         |___/      */

DEFUN(string_equals, "string-equals", R"((string-equals STRING1 STRING2)

Return `t` if the proviced strings equal lexicographically. Return `nil` otherwise.
)");

DEFUN(string_contains, "string-contains", R"((string-contains STRING SUBSTRING)

Return `t` if `STRING` contains `SUBSTRING` as a substring. Return `nil` otherwise.
)");

DEFUN(string_endswith, "string-endswith", R"((string-contains STRING SUFFIX)

Return `t` if `STRING` ends with `SUFFIX`. Return `nil` otherwise.
)");

DEFUN(string_startswith, "string-startswith", R"((string-contains STRING PREFIX)

Return `t` if `STRING` starts with `PREFIX`. Return `nil` otherwise.
)");

DEFUN(string_length, "string-length", R"((string-length STRING)

Return the length of the provided string.
)");

DEFUN(string_capitalize, "string-capitalize", R"((string-capitalize STRING)

Capitalized the first letter of the provided string.
)");

DEFUN(string_find, "string-find", R"((string-find STRING SUBSTRING)

Return the first index where `SUBSTRING` is contained in `STRINGE`.
)");

DEFUN(string_replace, "string-replace", R"((string-replace STRING SUBSTRING NEWSTRING)

Replace one occurrence of `SUBSTRING` in STRING with `NEWSTRING`. The
new string is returned.
)");

DEFUN(string_replaceall, "string-replaceall", R"((string-replaceall STRING SUBSTRING NEWSTRING)

Replace all occurrences of `SUBSTRING` in STRING with `NEWSTRING`. The
new string is returned.
)");

DEFUN(string_split, "string-split", R"((string-split STRING DELIMETER)

Split `STRING` based on `DELIMETER` and return a list of the parts.
)");

DEFUN(string_substring, "string-substring", R"((string-substring STRING FROM TO)

Return a new string that is the subsection [`FROM`, `TO`) of `STRING`.
)");

DEFUN(string_splitlines, "string-splitlines", R"((string-splitlines STRING)

Split `STRING` based on `\n` and return a list of the lines.
)");

DEFUN(string_upper, "string-upper", R"((string-upper STRING)

Capitalize every letter of `STRING`.
)");

DEFUN(string_lower, "string-lower", R"((string-lower STRING)

Lower every letter of `STRING`.
)");

DEFUN(string_strip, "string-strip", R"((string-strip STRING)

Remove and trailing or preceding whitespace of string.
)");

DEFUN(string_join, "string-join", R"((string-join STRING [[STRING] ...])

Concatenate the provided string to a new string.
)");

DEFUN(char_isalpha, "char-isalpha", R"((char-isalpha CHAR)

Check if the character CHAR is a letter.
)");

DEFUN(char_isdigit, "char-isdigit", R"((char-isdigit CHAR)

Check if the character CHAR is a digit.
)");


/*     _    _                  _ _   _                    */
/*    / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___  ___  */
/*   / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \/ __| */
/*  / ___ \| | (_| | (_) | |  | | |_| | | | | | | | \__ \ */
/* /_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_|___/ */
/*            |___/                                       */


DEFUN(slice, "slice", R"((slice LIST FROM TO)

Select a subsection of the list `LIST` and return a new list with the
elements of the subsection.

Example:
```elisp
(slice '(10 20 30  40 50 60 70 80 90) 1 5) 
```
)");

DEFUN(sort, "sort", R"((sort LIST)

Sort the elements of `LIST` in ascending order. This function will
change LIST and won't generate a new object.

Example:
```elisp
(sort '(20 12 2 43 56 10 68 30))
```
)");

DEFUN(zip, "zip", R"((zip [[LIST] ...])

Take mutliple lists and build pairs of their elements at corresponding
positions. The pairs are put into a new list and this list is
returned.
)");

DEFUN(filter, "filter", R"((filter PREDICATE LIST)

Collect the elements of `LIST` that fullfil the predicate `PREDICATE`
and return a new list of them.
)");

DEFUN(any, "any", R"((any PREDICATE LIST)

Return `t` if at leas one of the elements in `LIST` fulfull the
predicate `PREDICATE`. Return `nil` otherwise.
)");

DEFUN(all, "all", R"((all PREDICATE LIST)

Return `t` if all elements in `LIST` fulfull the predicate
`PREDICATE`. Return `nil` otherwise.
)");

/*  ____                      */
/* |  _ \ __ _ _ __ ___  ___  */
/* | |_) / _` | '__/ __|/ _ \ */
/* |  __/ (_| | |  \__ \  __/ */
/* |_|   \__,_|_|  |___/\___| */


DEFUN(int_parse, "parse-int", R"((parse-int STRING)

Return the int value represented by STRING.

Example:
```elisp
(parse-int "12")
```
)");

DEFUN(float_parse, "parse-float", R"((parse-float STRING)

Return the real value represented by STRING.

Example:
```elisp
(parse-int "12.32")
)");

DEFUN(to_string, "to-string", R"((to-string VALUE)

Convert VALUE to string

Example:
```elisp
(to-string 42)
(to-string 42.32)
(to-string "string")
```
)");

DEFUN(to_char, "to-char", R"((to-char INT)

Convert INT to a character (ASCII encoding). INT must be a value in
the range [0, 255].

Example:
```elisp
(to-char 65)
(to-char 97)
```
)");


}  // namespace alisp
