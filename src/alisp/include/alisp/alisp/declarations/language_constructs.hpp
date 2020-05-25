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


/*  _                                              */
/* | |    __ _ _ __   __ _ _   _  __ _  __ _  ___  */
/* | |   / _` | '_ \ / _` | | | |/ _` |/ _` |/ _ \ */
/* | |__| (_| | | | | (_| | |_| | (_| | (_| |  __/ */
/* |_____\__,_|_| |_|\__, |\__,_|\__,_|\__, |\___| */
/*                   |___/             |___/       */
/*   ____                _                   _        */
/*  / ___|___  _ __  ___| |_ _ __ _   _  ___| |_ ___  */
/* | |   / _ \| '_ \/ __| __| '__| | | |/ __| __/ __| */
/* | |__| (_) | | | \__ \ |_| |  | |_| | (__| |_\__ \ */
/*  \____\___/|_| |_|___/\__|_|   \__,_|\___|\__|___/ */

DEFUN(make_symbol, "make-symbol", R"((make-symbol NAME)

Return a new symbol with the name NAME.

Example:
```elisp
(make-symbol "sym")
```
)");

DEFUN(intern, "intern", R"(intern NAME)");

DEFUN(import,
      "import",
      R"((import MODULE [:file file] [:all] [( [(SYM MAPPED)]... )])

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

DEFUN(defconst, "defconst", R"((defconst NAME VALUE [DOC])

Define a new constant variable with a name `NAME` in the current
module. `VALUE` is the initial value of the variable and `DOC` is an
optional docstring. A variable *has* to be defines before used. A
variable defined through `defconst` will live till the end of the
program. If another part of the porgram tries to chang a constant
variable, an error signal will be emitted.

Example:
```elisp
(defconst new-var 42)
```

)");

DEFUN(eval, "eval", R"((eval FORM)

Evaluate the form `FORM`. The usual form for evaluation apply.
)");


DEFUN(eval_file, "eval-file", R"((eval-file FILE)

Execute the file `FILE` as a alisp-script in the current
environment. `FILE` should be a valid path
)");

DEFUN(eval_string, "eval-string", R"((eval-string STRING)

Execute the string `STRING` as a alisp-statement in the current
environment.
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

DEFUN(dotimes, "dotimes", R"((dotimes (SYMBOL COUNT) BODY)

Evaluate `BODY` once for each integer from 0 (inclusive) to `COUNT` (exclusive), binding the variable `SYMBOL` to the integer for the current iteration.

Example:
```elisp
(dotimes (i 100)
   (println "i:" i ))
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

DEFUN(progn1, "progn1", R"((progn1 BODY)

Evaluate the forms in `BODY` sequentially and return the value of the
fist one.
)");

DEFUN(progn2, "progn2", R"((progn2 BODY)

Evaluate the forms in `BODY` sequentially and return the value of the
second one.
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

DEFUN(apply, "apply", R"((apply SYMBOL (LIST))

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


DEFUN(condition_case,
      "condition-case",
      R"((condition-case SYMBOL BODY [[HANDLERS]...])
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

DEFUN(assert_not, "assert-not", R"((assert-not FORM)

Assert that the value of `FORM` is *falsey*. If not, an assert signal
is emitted.

Example:
```elisp
(assert nil)
(assert t)
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


DEFVAR(
  Qlanguage_all,
  Vlanguage_all,
  "--language-all--",
  make_sym_list({ "import",    "modref",   "defun",   "defconst",  "eval",   "setq",   "set",       "setq",
                  "quote",     "function", "lambda",  "if",        "while",  "dolist", "cond",      "when",
                  "unless",    "let",      "let*",    "or",        "and",    "not",    "parse-int", "parse-float",
                  "to-string", "to-char",  "funcall", "backquote", "return", "exit",   "intern",    "make-symbol" }),
  R"(Basic fuctions that provide the backbone of the language. These include global and local variable definition, flow control structures and loops.)");


}  // namespace alisp
