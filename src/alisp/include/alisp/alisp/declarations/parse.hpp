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


DEFVAR(Qcasts_all,
       Vcasts_all,
       "--casts-all--",
       make_sym_list({ "parse-int", "parse-float", "to-string", "to-char" }),
       R"(Functions for casting between types.)");


}  // namespace alisp
