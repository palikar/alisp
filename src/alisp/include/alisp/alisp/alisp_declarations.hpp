/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
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

(let ((var-1 42)
      (var-2 var-1)
       var-3)         ; nil variable
   (println var-1)    ; 42
   (println var-2))   ; 43

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


DEFUN(mapc, "mapc", R"((mapc FUNCTION LIST))");
DEFUN(mapcar, "mapcar", R"((mapcar FUNCTION LIST))");
DEFUN(car, "car", R"((mapc LIST))");
DEFUN(cons, "cons", R"((cons LIST))");
DEFUN(head, "head", R"((head LIST))");
DEFUN(last, "last", R"((last LIST))");
DEFUN(init, "init", R"((init LIST))");
DEFUN(tail, "tail", R"((tail LIST))");
DEFUN(push, "push", R"((push LIST ELEMENT))");
DEFUN(delete, "delete", R"((delete LIST ELEMENT))");
DEFUN(remove, "remove", R"((remove LIST ELEMENT))");
DEFUN(nth, "nth", R"((nth LIST INDEX))");
DEFUN(range, "range", R"((range FROM TO))");
DEFUN(length, "length", R"((length LIST))");
DEFUN(contains, "contains", R"((contains LIST ELEMENT))");


/*  ____  _                                 */
/* / ___|| |_ _ __ ___  __ _ _ __ ___  ___  */
/* \___ \| __| '__/ _ \/ _` | '_ ` _ \/ __| */
/*  ___) | |_| | |  __/ (_| | | | | | \__ \ */
/* |____/ \__|_|  \___|\__,_|_| |_| |_|___/ */

DEFUN(stream, "stream", R"((stream [:from-string STRING] [:from-file FILE]))");
DEFUN(close_stream, "stream-close", R"((stream-close STREAM))");
// DEFUN(with_stream, "with-streaRm(())");
DEFUN(with_cout, "with-cout", R"((with-cout STREAM))");
DEFUN(with_cin, "with-cin", R"((with-cin STREAM))");

DEFUN(stream_content, "stream-content", R"(((content STRREAM))");

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

DEFUN(file_open, "file-open", R"((file-open PATH [:out] [:in]))");
DEFUN(file_close, "file-close", R"((file-close FILE))");
DEFUN(file_read_line, "file-read-line", R"((file-read-line FILE VALUE))");
DEFUN(file_write_line, "file-write-line", R"((file-write-line FILE))");
DEFUN(file_has_more, "file-has-more", R"((file-has-more))");


/*  ____                       */
/* |  _ \ _ __ ___  _ __  ___  */
/* | |_) | '__/ _ \| '_ \/ __| */
/* |  __/| | | (_) | |_) \__ \ */
/* |_|   |_|  \___/| .__/|___/ */
/*                 |_| */


DEFUN(prop_get, "prop-get", R"((prop-get SYM PROPERTY))");
DEFUN(prop_set, "prop-set", R"((prop-set SYM PROPERTY VALUE))");
DEFUN(prop_list, "prop-list", R"((prop-list SYM))");


/*  ____               _ _           _             */
/* |  _ \ _ __ ___  __| (_) ___ __ _| |_ ___  ___  */
/* | |_) | '__/ _ \/ _` | |/ __/ _` | __/ _ \/ __| */
/* |  __/| | |  __/ (_| | | (_| (_| | ||  __/\__ \ */
/* |_|   |_|  \___|\__,_|_|\___\__,_|\__\___||___/ */


DEFUN(pfunction, "pfunction", R"((pfunction SYMBOL))");
DEFUN(psym, "psym", R"((psym SYMBOL))");
DEFUN(plist, "plist", R"((plist SYMBOL))");
DEFUN(pint, "pint", R"((pint SYMBOL))");
DEFUN(preal, "preal", R"((preal SYMBOL))");
DEFUN(pstring, "pstring", R"((pstring SYMBOL))");

/*  _                _       */
/* | |    ___   __ _(_) ___  */
/* | |   / _ \ / _` | |/ __| */
/* | |__| (_) | (_| | | (__  */
/* |_____\___/ \__, |_|\___| */
/*             |___/         */

DEFUN(or, "or", R"((or [[VALUE]...]))");
DEFUN(and, "and", R"((and [[VALUE]...]))");
DEFUN(not, "not", R"((not VALUE))");


/*  __  __       _   _      */
/* |  \/  | __ _| |_| |__   */
/* | |\/| |/ _` | __| '_ \  */
/* | |  | | (_| | |_| | | | */
/* |_|  |_|\__,_|\__|_| |_| */

DEFUN(plus, "+", R"((+ [[VALUE]...]))");
DEFUN(minus, "-", R"((- [[VALUE]...]))");
DEFUN(dev, "/", R"((/ [[VALUE]...]))");
DEFUN(multiply, "*", R"((* [[VALUE]...]))");
DEFUN(gt, ">", R"((> VALUE1 VALUE2))");
DEFUN(geq, ">=", R"((>= VALUE1 VALUE2))");
DEFUN(lt, "<", R"((< VALUE1 VALUE2))");
DEFUN(leq, "<=", R"((<= VALUE1 VALUE2))");
DEFUN(eq_math, "==", R"((== VALUE1 VALUE2))");
DEFUN(neq, "!=", R"((!= VALUE1 VALUE2))");
DEFUN(mod, "mod", R"((mod VALUE1))");
DEFUN(pow, "pow", R"((pow VALUE1 VALUE2))");

DEFUN(leftshift, "<<", R"((<< VALUE))");
DEFUN(rightshift, ">>", R"((>> VALUE))");

DEFUN(min, "min", R"((min [[VALUE]...]))");
DEFUN(max, "max", R"((max [[VALUE]...]))");
DEFUN(round, "round", R"((round VALUE PLACES))");


/*  ____  _        _                  */
/* / ___|| |_ _ __(_)_ __   __ _ ___  */
/* \___ \| __| '__| | '_ \ / _` / __| */
/*  ___) | |_| |  | | | | | (_| \__ \ */
/* |____/ \__|_|  |_|_| |_|\__, |___/ */
/*                         |___/      */

DEFUN(string_equals, "string-equals", R"((string-equals STRING1 STRING2))");
DEFUN(string_contains, "string-contains", R"((string-contains STRING SUBSTRING))");
DEFUN(string_endswith, "string-endswith", R"((string-contains STRING SUFFIX))");
DEFUN(string_startswith, "string-startswith", R"((string-contains STRING PREFIX))");
DEFUN(string_length, "string-length", R"((string-length STRING))");
DEFUN(string_capitalize, "string-capitalize", R"((string-capitalize STRING))");
DEFUN(string_find, "string-find", R"((string-find STRING SUBSTRING))");
DEFUN(string_replace, "string-replace", R"((string-replace STRING SUBSTRING NEWSTRING))");
DEFUN(string_replaceall, "string-replaceall", R"((string-replaceall STRING SUBSTRING NEWSTRING))");
DEFUN(string_split, "string-split", R"((string-split STRING DELIMETER))");
DEFUN(string_substring, "string-substring", R"((string-substring STRING FROM TO))");
DEFUN(string_splitlines, "string-splitlines", R"((string-splitlines STRING))");
DEFUN(string_upper, "string-upper", R"((string-upper STRING))");
DEFUN(string_lower, "string-lower", R"((string-lower STRING))");
DEFUN(string_strip, "string-strip", R"((string-strip STRING))");
DEFUN(string_join, "string-join", R"((string-join STRING [[STRING] ...]))");

DEFUN(char_isalpha, "char-isalpha", R"((char-isalpha CHAR))");
DEFUN(char_isdigit, "char-isdigit", R"((char-isdigit CHAR))");


/*     _    _                  _ _   _                    */
/*    / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___  ___  */
/*   / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \/ __| */
/*  / ___ \| | (_| | (_) | |  | | |_| | | | | | | | \__ \ */
/* /_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_|___/ */
/*            |___/                                       */


DEFUN(slice, "slice", R"((slice LIST FROM TO))");
DEFUN(sort, "sort", R"((sort LIST))");
DEFUN(zip, "zip", R"((zip [[LIST] ...]))");
DEFUN(filter, "filter", R"((filter FUNCTION LIST))");
DEFUN(any, "any", R"((any FUNCTION LIST))");
DEFUN(all, "all", R"((all FUNCTION LIST))");

/*  ____                      */
/* |  _ \ __ _ _ __ ___  ___  */
/* | |_) / _` | '__/ __|/ _ \ */
/* |  __/ (_| | |  \__ \  __/ */
/* |_|   \__,_|_|  |___/\___| */


DEFUN(int_parse, "parse-int", R"((parse-int STRING))");
DEFUN(float_parse, "parse-float", R"((parse-float STRING))");
DEFUN(to_string, "to-string", R"((to-string VALUE))");
DEFUN(to_char, "to-char", R"((to-char INT))");


}  // namespace alisp
