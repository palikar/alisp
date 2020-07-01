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
#include "alisp/alisp/alisp_signature.hpp"


namespace alisp
{


/*  __  __       _   _      */
/* |  \/  | __ _| |_| |__   */
/* | |\/| |/ _` | __| '_ \  */
/* | |  | | (_| | |_| | | | */
/* |_|  |_|\__,_|\__|_| |_| */

DEFUN(plus, "+", (Signature{Rest{}, Numbers{}}), R"((+ [[VALUE]...])

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

DEFUN(bit_or, "or*", R"()");

DEFUN(bit_and, "and*", R"()");

DEFUN(bit_xor, "xor*", R"()");

DEFUN(bit_inv, "inv*", R"()");

DEFVAR(Qmath_all,
       Vmath_all,
       "--math-all--",
       make_sym_list({ "+", "-", "/", "*", "<", "<=", ">", ">=", "==", "!=", "mod", "pow", "min", "max", "round" }),
       R"(Functions that realise simple math operations.)");

}  // namespace alisp
