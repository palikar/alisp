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

DEFUN(pfile, "pfile", R"((pfile FORM)

Return `t` if FORM is a string and `nil` otherwise.
)");

DEFUN(pstream, "pstream", R"((pstream FORM)

Return `t` if FORM is a string and `nil` otherwise.
)");

DEFUN(pmemory, "pmemory", R"((pmemory FORM)

Return `t` if FORM is a string and `nil` otherwise.
)");

DEFUN(pbyte, "pbyte", R"((pbyte FORM)

Return `t` if FORM is a string and `nil` otherwise.
)");


DEFVAR(
  Qpredicates_all,
  Vpredicates_all,
  "--predicates-all--",
  make_sym_list({ "pstring", "plist", "pint", "preal", "psym", "pfunction" }),
  R"(Functions for type inspecting. These functions can be used to check whether an object is from a certain type.)");


}  // namespace alisp
