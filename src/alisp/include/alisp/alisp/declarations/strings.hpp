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

/*  ____  _        _                  */
/* / ___|| |_ _ __(_)_ __   __ _ ___  */
/* \___ \| __| '__| | '_ \ / _` / __| */
/*  ___) | |_| |  | | | | | (_| \__ \ */
/* |____/ \__|_|  |_|_| |_|\__, |___/ */
/*                         |___/      */

DEFUN(string_append, "string-append", R"((string-append STRING1 STRING2)

Return a new string by concatenatig `STRING1` to `STRING2`.
)");

DEFUN(string_prepend, "string-prepend", R"((string-prepend STRING1 STRING2)

Return a new string by prepending `STRING1` to `STRING2`.
)");

DEFUN(string_reverse, "string-reverse", R"((string-reverse STRING)

Rerturn a new string with the elements of STRING in reverse order
)");

DEFUN(string_equals, "string-equals", R"((string-equals STRING1 STRING2)

Return `t` if the proviced strings equal lexicographically. Return `nil` otherwise.
)");

DEFUN(string_less, "string-less", R"((string-less STRING1 STRING2))");

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

DEFUN(string_replace,
      "string-replace",
      R"((string-replace STRING SUBSTRING NEWSTRING)

Replace one occurrence of `SUBSTRING` in STRING with `NEWSTRING`. The
new string is returned.
)");

DEFUN(string_replaceall,
      "string-replaceall",
      R"((string-replaceall STRING SUBSTRING NEWSTRING)

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
e)");


DEFVAR(Qstrings_all,
       Vstrings_all,
       "--strings-all--",
       make_sym_list({ "string-length",
                       "string-contains",
                       "string-endswith",
                       "string-startswith",
                       "string-length",
                       "string-capitalize",
                       "string-find",
                       "string-replace",
                       "string-replaceall",
                       "string-split",
                       "string-substring",
                       "string-splitlines",
                       "string-upper",
                       "string-lower",
                       "string-strip",
                       "string-join",
                       "char-isalpha",
                       "char-isdigit" }),
       R"(Functions for basic string handling.)");


}  // namespace alisp
