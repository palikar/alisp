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


ALObjectPtr Fmultiply(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
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

ALObjectPtr Fplus(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
{
    auto eval_obj = eval_transform(evl, obj);

    AL_CHECK(assert_numbers(eval_obj));

    if (are_objects_int(eval_obj))
    {
        const ALObject::int_type sum = reduce<false>(evl, eval_obj, ADD_OBJ_FUN, static_cast<ALObject::int_type>(0));
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

ALObjectPtr Fminus(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
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

ALObjectPtr Fdev(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
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

ALObjectPtr Flt(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));

    const auto one = eval_check(eval, obj,0, &assert_number<int>);
    
    const auto two = eval_check(eval, obj,1, &assert_number<int>);
    


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

ALObjectPtr Fleq(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<0>(obj));

    const auto one = eval_check(eval, obj,0, &assert_number<int>);
    
    const auto two = eval_check(eval, obj,1, &assert_number<int>);
    


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

ALObjectPtr Fgt(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<0>(obj));

    const auto one = eval_check(eval, obj,0, &assert_number<int>);
    
    const auto two = eval_check(eval, obj,1, &assert_number<int>);
    


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

ALObjectPtr Fgeq(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<0>(obj));

    const auto one = eval_check(eval, obj,0, &assert_number<int>);
    
    const auto two = eval_check(eval, obj,1, &assert_number<int>);
    


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

ALObjectPtr Feq_math(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<0>(obj));

    const auto one = eval_check(eval, obj,0, &assert_number<int>);
    
    const auto two = eval_check(eval, obj,1, &assert_number<int>);
    


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

ALObjectPtr Fneq(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<0>(obj));

    const auto one = eval_check(eval, obj,0, &assert_number<int>);
    
    const auto two = eval_check(eval, obj,1, &assert_number<int>);
    


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

ALObjectPtr Fmod(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));

    const auto one = eval_check(eval, obj,0, &assert_int<int>);
    
    const auto two = eval_check(eval, obj,1, &assert_int<int>);
    
    auto res = one->to_int() % two->to_int();

    return make_object(res);
}

ALObjectPtr Fpow(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));

    const auto one = eval_check(eval, obj,0, &assert_number<int>);
    
    const auto two = eval_check(eval, obj,1, &assert_number<int>);
    


    return make_object(std::pow(one->to_real(), two->to_real()));
}

ALObjectPtr Fmin(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmax(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fround(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));

    const auto one = eval_check(eval, obj,0, &assert_number<int>);
    const auto two = eval_check(eval, obj,1, &assert_int<int>);    


    return make_double(utility::round_nplaces(one->to_real(), two->to_int()));
}


}  // namespace alisp
