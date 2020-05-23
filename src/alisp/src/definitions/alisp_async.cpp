/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2 of the License, or
     (at your option) any prior version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License along
     with this program; if not, write to the Free Software Foundation, Inc.,
     51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#include <algorithm>
#include <string>

#include "alisp/alisp/declarations/async.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include "alisp/alisp/async/timing.hpp"

namespace alisp
{


ALObjectPtr Fset_timeout(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto time = eval->eval(obj->i(1));
    auto fun  = eval->eval(obj->i(0));
    AL_CHECK(assert_int(time));
    AL_CHECK(assert_function(fun));

    async::dispatch<set_timeout>(eval->async(), static_cast<size_t>(time->to_int()), fun);

    return Qt;
}


}  // namespace alisp
