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

DEFUN(insert, "insert", R"((insert LIST INDEX ELEMENT))");
DEFUN(find, "find", R"((find LIST))");

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


DEFVAR(Qprinting_all,
       Vprinting_all,
       "--printing-all--",
       make_sym_list({ "print", "println", "eprint", "eprintln", "read-line" }),
       R"(Functions to interact with the stanard input and output.)");


}  // namespace alisp
