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

struct Sleftshift
{
    inline static const std::string name = "<<";

    inline static const std::string doc{ R"((<< VALUE1 VALUE2)

Shift the bits of `VALUE` to the left `VALUE2` times.

Example:
```elisp
(>> 16 2)   ;  4
```
)" };

    static ALObjectPtr Fleftshift(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto lhs = eval_check(eval, obj, 0, &assert_int<size_t>);
        auto rhs = eval_check(eval, obj, 1, &assert_int<size_t>);
        return make_int(SHIFT_LEFT(lhs, rhs));
    }
};

struct Srightshift
{
    inline static const std::string name = ">>";

    inline static const std::string doc{ R"((>> VALUE1 VALU2)

Shift the bits of `VALUE` to the right `VALUE2` times.

Example:
```elisp
(<< 2 2)   ;  8
```
)" };

    static ALObjectPtr Frightshift(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto lhs = eval_check(eval, obj, 0, &assert_int<size_t>);
        auto rhs = eval_check(eval, obj, 1, &assert_int<size_t>);

        return make_int(SHIFT_RIGHT(lhs, rhs));
    }
};

struct Sbit_or
{
    inline static const std::string name = "or*";

    inline static const std::string doc{ R"()" };

    static ALObjectPtr bit_or(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto lhs = eval_check(eval, obj, 0, &assert_int<size_t>);
        auto rhs = eval_check(eval, obj, 1, &assert_int<size_t>);
        return make_int(BIT_OR(lhs, rhs));
    }
};

struct Sbit_and
{
    inline static const std::string name = "and*";

    inline static const std::string doc{ R"()" };

    static ALObjectPtr Fbit_and(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto lhs = eval_check(eval, obj, 0, &assert_int<size_t>);
        auto rhs = eval_check(eval, obj, 1, &assert_int<size_t>);
        return make_int(BIT_AND(lhs, rhs));
    }
};

struct Sbit_xor
{
    inline static const std::string name = "xor*";

    inline static const std::string doc{ R"()" };

    static ALObjectPtr Fbit_xor(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto lhs = eval_check(eval, obj, 0, &assert_int<size_t>);
        auto rhs = eval_check(eval, obj, 1, &assert_int<size_t>);
        return make_int(BIT_XOR(lhs, rhs));
    }
};

struct Sbit_inv
{
    inline static const std::string name = "inv*";

    inline static const std::string doc{ R"()" };

    static ALObjectPtr Fbit_inv(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto lhs = eval_check(eval, obj, 0, &assert_int<size_t>);
        return make_int(BIT_INV(lhs));
    }
};


}  // namespace alisp
