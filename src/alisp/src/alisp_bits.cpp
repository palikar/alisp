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
#include <cmath>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/utility/macros.hpp"
#include "alisp/utility/math_utils.hpp"


namespace alisp
{

ALObjectPtr Fleftshift(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<2>(obj));
    auto lhs = evl->eval(obj->i(0));
    auto rhs = evl->eval(obj->i(1));
    AL_CHECK(assert_int(rhs));
    AL_CHECK(assert_int(lhs));
    return make_int(SHIFT_LEFT(lhs, rhs));
}

ALObjectPtr Frightshift(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<2>(obj));
    auto lhs = evl->eval(obj->i(0));
    auto rhs = evl->eval(obj->i(1));
    AL_CHECK(assert_int(rhs));
    AL_CHECK(assert_int(lhs));
    return make_int(SHIFT_RIGHT(lhs, rhs));
}

ALObjectPtr Fbit_or(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<2>(obj));
    auto lhs = evl->eval(obj->i(0));
    auto rhs = evl->eval(obj->i(1));
    AL_CHECK(assert_int(rhs));
    AL_CHECK(assert_int(lhs));
    return make_int(BIT_OR(lhs, rhs));
}

ALObjectPtr Fbit_and(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<2>(obj));
    auto lhs = evl->eval(obj->i(0));
    auto rhs = evl->eval(obj->i(1));
    AL_CHECK(assert_int(rhs));
    AL_CHECK(assert_int(lhs));
    return make_int(BIT_AND(lhs, rhs));
}

ALObjectPtr Fbit_xor(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<2>(obj));
    auto lhs = evl->eval(obj->i(0));
    auto rhs = evl->eval(obj->i(1));
    AL_CHECK(assert_int(rhs));
    AL_CHECK(assert_int(lhs));
    return make_int(BIT_XOR(lhs, rhs));
}

ALObjectPtr Fbit_inv(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    auto lhs = evl->eval(obj->i(0));
    AL_CHECK(assert_int(lhs));
    return make_int(BIT_INV(lhs));
}


}  // namespace alisp
