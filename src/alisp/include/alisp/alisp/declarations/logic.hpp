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


DEFVAR(Qlogic_all,
       Vlogic_all,
       "--logic-all--",
       make_sym_list({ "or", "and", "not" }),
       R"(Functions to combine boolean types.)");


}  // namespace alisp
