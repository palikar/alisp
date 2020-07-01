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

#include "alisp/alisp/declarations/math.hpp"

#include "alisp/utility/macros.hpp"
#include "alisp/utility/math_utils.hpp"


namespace alisp
{


struct Smultiply
{
    inline static const std::string name = "*";

    inline static const std::string doc{ R"((* [[VALUE]...])

Retrun the product of the values of all the provided arguments. 

Example:
```elisp
(* 10 20 30)
```
)" };

    static ALObjectPtr Fmultiply(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        auto eval_obj = eval_transform(evl, obj);

        AL_CHECK(assert_numbers(eval_obj));

        if (are_objects_int(eval_obj))
        {
            const ALObject::int_type product =
              reduce<false>(evl, eval_obj, MUL_OBJ_FUN, static_cast<ALObject::int_type>(1));
            return make_int(product);
        }
        else if (are_objects_numbers(eval_obj))
        {
            const ALObject::real_type product =
              reduce<false>(evl, eval_obj, MUL_OBJ_FUN_D, static_cast<ALObject::real_type>(1));
            return make_double(product);
        }
        warn::warn_math("Multiplying non-numbers ");
        return Qnil;
    }
};

struct Splus
{
    inline static const std::string name = "+";

    inline static const std::string doc{ R"((+ [[VALUE]...])

Retrun the sum of the values of all the provided arguments. 

Example:
```elisp
(+ 10 20 30)
```
)" };

    static ALObjectPtr Fplus(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        auto eval_obj = eval_transform(evl, obj);

        AL_CHECK(assert_numbers(eval_obj));

        if (are_objects_int(eval_obj))
        {
            const ALObject::int_type sum =
              reduce<false>(evl, eval_obj, ADD_OBJ_FUN, static_cast<ALObject::int_type>(0));
            return make_int(sum);
        }
        else if (are_objects_numbers(eval_obj))
        {
            const ALObject::real_type sum =
              reduce<false>(evl, eval_obj, ADD_OBJ_FUN_D, static_cast<ALObject::real_type>(0));
            return make_double(sum);
        }

        warn::warn_math("Adding non-numbers ");
        return Qnil;
    }
};

struct Sminus
{
    inline static const std::string name = "-";

    inline static const std::string doc{ R"((- [[VALUE]...])

Subsract the values of the folloring arguments from the value of the
first argument.

Example:
```elisp
(- 10 20 30)
```
)" };

    static ALObjectPtr Fminus(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        auto eval_obj = eval_transform(evl, obj);

        AL_CHECK(assert_numbers(eval_obj));

        if (are_objects_int(eval_obj))
        {
            const ALObject::int_type sum = reduce<false>(evl, eval_obj, SUB_OBJ_FUN, eval_obj->i(0)->to_int(), 1);
            return make_int(sum);
        }
        else if (are_objects_numbers(eval_obj))
        {
            const ALObject::real_type sum = reduce<false>(evl, eval_obj, SUB_OBJ_FUN_D, eval_obj->i(0)->to_real(), 1);
            return make_double(sum);
        }

        warn::warn_math("Subtracting non-numbers ");
        return Qnil;
    }
};

struct Sdev
{
    inline static const std::string name = "/";

    inline static const std::string doc{ R"((/ [[VALUE]...])(- [[VALUE]...])

Devide the value of the first argument to the values of the following
arguements.

Example:
```elisp
(/ 10 20 30)
```
)" };

    static ALObjectPtr Fdev(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        auto eval_obj = eval_transform(evl, obj);

        AL_CHECK(assert_numbers(eval_obj));

        if (are_objects_int(eval_obj))
        {
            const ALObject::int_type sum = reduce<false>(evl, eval_obj, DIV_OBJ_FUN, eval_obj->i(0)->to_int(), 1);
            return make_int(sum);
        }
        else if (are_objects_numbers(eval_obj))
        {
            const ALObject::real_type sum = reduce<false>(evl, eval_obj, DIV_OBJ_FUN_D, eval_obj->i(0)->to_real(), 1);
            return make_double(sum);
        }
        warn::warn_math("Dividing non-numbers ");
        return Qnil;
    }
};

struct Slt
{
    inline static const std::string name = "*";

    inline static const std::string doc{ R"((* [[VALUE]...])

Retrun the product of the values of all the provided arguments. 

Example:
```elisp
(* 10 20 30)
```
)" };

    static ALObjectPtr Flt(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));

        const auto one = eval_check(eval, obj, 0, &assert_number<size_t>);

        const auto two = eval_check(eval, obj, 1, &assert_number<size_t>);


        if (one->to_real() < two->to_real())
        {
            return Qt;
        }
        else
        {
            warn::warn_math("Comparing non-numbers ");
            return Qnil;
        };
    }
};

struct Sleq
{
    inline static const std::string name = "<=";

    inline static const std::string doc{ R"((<= VALUE1 VALUE2)

Return `t` if `VALUE1` is less or equal in value than `VALUE2`. Return `nil`
otherwise.
)" };

    static ALObjectPtr Fleq(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<0>(obj));

        const auto one = eval_check(eval, obj, 0, &assert_number<size_t>);

        const auto two = eval_check(eval, obj, 1, &assert_number<size_t>);


        if (one->to_real() <= two->to_real())
        {
            return Qt;
        }
        else
        {
            warn::warn_math("Comparing non-numbers ");
            return Qnil;
        }
    }
};

struct Sgt
{
    inline static const std::string name = ">";

    inline static const std::string doc{ R"((> VALUE1 VALUE2)

Return `t` if `VALUE1` is grater in value than `VALUE2`. Return `nil`
otherwise.
)" };

    static ALObjectPtr Fgt(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<0>(obj));

        const auto one = eval_check(eval, obj, 0, &assert_number<size_t>);

        const auto two = eval_check(eval, obj, 1, &assert_number<size_t>);


        if (one->to_real() > two->to_real())
        {
            return Qt;
        }
        else
        {
            warn::warn_math("Comparing non-numbers ");
            return Qnil;
        }
    }
};

struct Sgeq
{
    inline static const std::string name = ">=";

    inline static const std::string doc{ R"((>= VALUE1 VALUE2)

Return `t` if `VALUE1` is grater or equal in value than `VALUE2`. Return `nil`
otherwise.
)" };

    static ALObjectPtr Fgeq(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<0>(obj));

        const auto one = eval_check(eval, obj, 0, &assert_number<size_t>);

        const auto two = eval_check(eval, obj, 1, &assert_number<size_t>);


        if (one->to_real() >= two->to_real())
        {
            return Qt;
        }
        else
        {
            warn::warn_math("Comparing non-numbers ");
            return Qnil;
        }
    }
};

struct Seq_math
{
    inline static const std::string name = "==";

    inline static const std::string doc{ R"((== VALUE1 VALUE2)

Return `t` if `VALUE1` is equal in value than `VALUE2`. Return `nil`
otherwise.
)" };

    static ALObjectPtr Feq_math(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<0>(obj));

        const auto one = eval_check(eval, obj, 0, &assert_number<size_t>);

        const auto two = eval_check(eval, obj, 1, &assert_number<size_t>);


        if (one->to_real() == two->to_real())
        {
            return Qt;
        }
        else
        {
            warn::warn_math("Comparing non-numbers ");
            return Qnil;
        }
    }
};

struct Sneq
{
    inline static const std::string name = "!=";

    inline static const std::string doc{ R"((!= VALUE1 VALUE2)

Return `t` if `VALUE1` is not equal in value than `VALUE2`. Return `nil`
otherwise.
)" };

    static ALObjectPtr Fneq(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<0>(obj));

        const auto one = eval_check(eval, obj, 0, &assert_number<size_t>);

        const auto two = eval_check(eval, obj, 1, &assert_number<size_t>);


        if (one->to_real() != two->to_real())
        {
            return Qt;
        }
        else
        {
            warn::warn_math("Comparing non-numbers ");
            return Qnil;
        }
    }
};

struct Smod
{
    inline static const std::string name = "mod";

    inline static const std::string doc{ R"((mod VALUE1 VALUE2)

Return the remainder by devision of `VALUE1` to `VALUE2`
)" };

    static ALObjectPtr Fmod(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));

        const auto one = eval_check(eval, obj, 0, &assert_int<size_t>);

        const auto two = eval_check(eval, obj, 1, &assert_int<size_t>);

        auto res = one->to_int() % two->to_int();

        return make_object(res);
    }
};

struct Spow
{
    inline static const std::string name = "pow";

    inline static const std::string doc{ R"((pow VALUE1 VALUE2)

Return `VALUE1` to the power of `VALUE2`.
)" };

    static ALObjectPtr Fpow(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));

        const auto one = eval_check(eval, obj, 0, &assert_number<size_t>);

        const auto two = eval_check(eval, obj, 1, &assert_number<size_t>);


        return make_object(std::pow(one->to_real(), two->to_real()));
    }
};

struct Smin
{
    inline static const std::string name = "-";

    inline static const std::string doc{ R"((- [[VALUE]...])

Subsract the values of the folloring arguments from the value of the
first argument.

Example:
```elisp
(- 10 20 30)
```
)" };

    static ALObjectPtr Fmin(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<1>(obj));

        auto eval_obj = eval_transform(eval, obj);
        AL_CHECK(assert_numbers(eval_obj));
        auto is_int = are_objects_int(eval_obj);
        if (is_int)
        {
            auto min_element = eval_obj->i(0)->to_int();
            for (auto el : *eval_obj)
            {
                auto current = el->to_int();
                if (current < min_element)
                {
                    min_element = current;
                }
            }
            return make_int(min_element);
        }
        else
        {

            auto min_element = eval_obj->i(0)->to_real();
            for (auto el : *eval_obj)
            {
                auto current = el->to_real();
                if (current < min_element)
                {
                    min_element = current;
                }
            }
            return make_double(min_element);
        }
    }
};

struct Smax
{
    inline static const std::string name = "max";

    inline static const std::string doc{ R"((max [[VALUE]...])

Evaluate the provided arguemnts and return the maximum value.

Example:
```elisp
(max 10 20 30) ; 30
```
)" };

    static ALObjectPtr Fmax(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<1>(obj));

        auto eval_obj = eval_transform(eval, obj);
        AL_CHECK(assert_numbers(eval_obj));
        auto is_int = are_objects_int(eval_obj);
        if (is_int)
        {
            auto max_element = eval_obj->i(0)->to_int();
            for (auto el : *eval_obj)
            {
                auto current = el->to_int();
                if (max_element < current)
                {
                    max_element = current;
                }
            }
            return make_int(max_element);
        }
        else
        {

            auto max_element = obj->i(0)->to_real();
            for (auto el : *eval_obj)
            {
                auto current = el->to_real();
                if (max_element < current)
                {
                    max_element = current;
                }
            }
            return make_double(max_element);
        }
    }
};

struct Sround
{
    inline static const std::string name = "round";

    inline static const std::string doc{ R"((round VALUE PLACE)

Round the real value `VALUE` to the `PALCE`-th decimal place.

Example:
```elisp
(round 42.1345 2) ; 42.13
```
)" };

    static ALObjectPtr Fround(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));

        const auto one = eval_check(eval, obj, 0, &assert_number<size_t>);
        const auto two = eval_check(eval, obj, 1, &assert_int<size_t>);


        return make_double(utility::round_nplaces(one->to_real(), two->to_int()));
    }
};


}  // namespace alisp
