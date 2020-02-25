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

#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

namespace alisp
{

ALObjectPtr Fslice(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    CHECK(assert_min_size<2>(obj));

    auto list = eval->eval(obj->i(0));
    CHECK(assert_list(list));

    auto ind_1 = eval->eval(obj->i(1));
    CHECK(assert_int(ind_1));
    size_t start = static_cast<size_t>(ind_1->to_int());

    size_t end = list->children().size();
    if (std::size(*obj) == 3)
    {
        auto ind_2 = eval->eval(obj->i(2));
        CHECK(assert_int(ind_2));
        end = static_cast<size_t>(ind_2->to_int());
    }

    ALObject::list_type new_list{};

    auto &child = list->children();
    for (auto i = start; i < end; ++i) { new_list.push_back(child[i]); }

    return make_object(new_list);
}

ALObjectPtr Fsort(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    CHECK(assert_size<1>(obj));

    auto list = eval->eval(obj->i(0));

    CHECK(assert_numbers(list));

    std::sort(std::begin(*list), std::end(*list), [&](auto &obj_1, auto &obj_2) { return obj_1->to_real() < obj_2->to_real(); });

    return list;
}

ALObjectPtr Fzip(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    CHECK(assert_min_size<2>(obj));

    auto eval_list = eval_transform(eval, obj);
    ALObject::list_type new_list{};
    size_t min_size = eval_list->i(0)->children().size();
    for (auto &l : *eval_list)
    {
        auto curr_size = std::size(l->children());
        if (curr_size < min_size) { min_size = curr_size; }
    }

    for (size_t i = 0; i < min_size; ++i)
    {
        ALObject::list_type next_tuple{};

        for (auto &el : *eval_list) { next_tuple.push_back(el->children()[i]); }

        new_list.push_back(make_object(next_tuple));
    }

    return make_object(new_list);
}

ALObjectPtr Ffilter(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    CHECK(assert_size<2>(obj));

    auto fun_obj = eval->eval(obj->i(0));
    auto list    = eval->eval(obj->i(1));

    CHECK(assert_list(list));
    CHECK(assert_function(fun_obj));

    ALObject::list_type new_list{};
    for (auto &el : *list)
    {
        if (is_truthy(eval->handle_lambda(fun_obj, make_list(el)))) { new_list.push_back(el); }
    }

    return make_object(new_list);
}

ALObjectPtr Fany(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    CHECK(assert_size<2>(obj));

    auto fun_obj = eval->eval(obj->i(0));
    auto list    = eval->eval(obj->i(1));

    CHECK(assert_list(list));
    CHECK(assert_function(fun_obj));

    for (auto &el : *list)
    {
        if (is_truthy(eval->handle_lambda(fun_obj, make_list(el)))) { return Qt; }
    }

    return Qnil;
}

ALObjectPtr Fall(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    CHECK(assert_size<2>(obj));

    auto fun_obj = eval->eval(obj->i(0));
    auto list    = eval->eval(obj->i(1));

    CHECK(assert_list(list));
    CHECK(assert_function(fun_obj));

    for (auto &el : *list)
    {
        if (is_falsy(eval->handle_lambda(fun_obj, make_list(el)))) { return Qnil; }
    }

    return Qt;
}


}  // namespace alisp
