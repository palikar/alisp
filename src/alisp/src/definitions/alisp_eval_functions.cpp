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

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"
#include "alisp/alisp/alisp_streams.hpp"

#include "alisp/utility/macros.hpp"

#include "alisp/alisp/declarations/language_constructs.hpp"


namespace alisp
{

ALObjectPtr Feval_string(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str = eval->eval(obj->i(0));
    AL_CHECK(assert_string(str));

    return eval->eval_string(str->to_string());
}

ALObjectPtr Feval_file(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str = eval->eval(obj->i(0));
    AL_CHECK(assert_string(str));

    return eval->eval_file(str->to_string());
}


}  // namespace alisp
