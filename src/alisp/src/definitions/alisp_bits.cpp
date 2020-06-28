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
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/utility/macros.hpp"
#include "alisp/utility/math_utils.hpp"

#include "alisp/alisp/declarations/math.hpp"


namespace alisp
{

ALObjectPtr Fleftshift(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto lhs = eval_check(eval, obj, 0, &assert_int<int>);
    auto rhs = eval_check(eval, obj, 1, &assert_int<int>);
    return make_int(SHIFT_LEFT(lhs, rhs));
}

ALObjectPtr Frightshift(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto lhs = eval_check(eval, obj, 0, &assert_int<int>);
    auto rhs = eval_check(eval, obj, 1, &assert_int<int>);

    return make_int(SHIFT_RIGHT(lhs, rhs));
}
ALObjectPtr Fbit_or(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto lhs = eval_check(eval, obj, 0, &assert_int<int>);
    auto rhs = eval_check(eval, obj, 1, &assert_int<int>);
    return make_int(BIT_OR(lhs, rhs));
}

ALObjectPtr Fbit_and(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto lhs = eval_check(eval, obj, 0, &assert_int<int>);
    auto rhs = eval_check(eval, obj, 1, &assert_int<int>);
    return make_int(BIT_AND(lhs, rhs));
}

ALObjectPtr Fbit_xor(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto lhs = eval_check(eval, obj, 0, &assert_int<int>);
    auto rhs = eval_check(eval, obj, 1, &assert_int<int>);
    return make_int(BIT_XOR(lhs, rhs));
}

ALObjectPtr Fbit_inv(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto lhs = eval_check(eval, obj, 0, &assert_int<int>);
    return make_int(BIT_INV(lhs));
}


}  // namespace alisp
