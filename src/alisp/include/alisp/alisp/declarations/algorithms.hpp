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


/*     _    _                  _ _   _                    */
/*    / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___  ___  */
/*   / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \/ __| */
/*  / ___ \| | (_| | (_) | |  | | |_| | | | | | | | \__ \ */
/* /_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_|___/ */
/*            |___/                                       */


DEFUN(reverse, "reverse", R"((reverse LIST)

)");

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


}  // namespace alisp
