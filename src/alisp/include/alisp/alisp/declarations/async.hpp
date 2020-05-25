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

DEFUN(set_timeout, "set-timeout", R"((set-timeout CALLBACK MILLISECONDS)

Execute `CALLBACK` after `SECONDS`.
)");

DEFUN(future_int, "future-int", R"((future-int)

Simple future int.
)");

DEFUN(future_await, "future-await", R"((future-await FUTURE)

Block the main thread till `FUTURE` is complete.
)");

DEFUN(future_then, "future-then", R"((future-then FUTURE SUCCESS REJECT)
)");

DEFUN(async_start, "async-start", R"((async-start)

)");

// DEFUN(future_poll, "future-poll", R"((future-poll FUTURE)
// )");

}  // namespace alisp
