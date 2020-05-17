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


}  // namespace alisp
