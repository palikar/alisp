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


#pragma once

#include <fmt/format.h>
#include <rang.hpp>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_factory.hpp"

#include "alisp/utility.hpp"


namespace alisp
{


inline ALObjectPtr eval_list(eval::Evaluator *evl, const ALObjectPtr &t_obj, size_t t_offset = 0)
{
    auto &objects   = *t_obj;
    const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);
    auto start_it   = std::next(std::begin(objects), hops);
    auto end_it     = std::prev(std::end(objects));

    if (start_it > end_it)
    {
        return Qt;
    }

    while (start_it != end_it)
    {
        evl->eval(*start_it);
        start_it = std::next(start_it);
    }
    return evl->eval(*end_it);
}

template<size_t N> inline ALObjectPtr eval_list_n(eval::Evaluator *evl, const ALObjectPtr &t_obj, size_t t_offset = 0)
{

    auto &objects   = *t_obj;
    const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);
    constexpr auto return_hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(N);

    auto start_it  = std::next(std::begin(objects), hops);
    auto return_it = std::next(std::begin(objects), return_hops - 1);
    auto end_it    = std::end(objects);

    if (start_it > end_it)
    {
        return Qt;
    }

    while (start_it != return_it)
    {
        evl->eval(*start_it);
        start_it = std::next(start_it);
    }
    auto res = evl->eval(*start_it);
    start_it = std::next(start_it);

    while (start_it != end_it)
    {
        evl->eval(*start_it);
        start_it = std::next(start_it);
    }

    return res;
}

inline ALObjectPtr eval_list_1(eval::Evaluator *evl, const ALObjectPtr &t_obj, size_t t_offset = 0)
{
    return eval_list_n<0>(evl, t_obj, t_offset);
}

inline ALObjectPtr eval_list_2(eval::Evaluator *evl, const ALObjectPtr &t_obj, size_t t_offset = 0)
{
    return eval_list_n<1>(evl, t_obj, t_offset);
}

template<bool eval, typename Callable>
inline auto apply(eval::Evaluator *evl, const ALObjectPtr &t_obj, Callable t_fun, size_t t_offset = 0)
{

    auto &objects   = *t_obj;
    const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);

    auto start_it = std::next(std::begin(objects), hops);
    auto end_it   = std::prev(std::end(objects));

    if (start_it > end_it)
    {
        return Qt;
    }

    while (start_it != end_it)
    {
        if constexpr (eval)
        {
            t_fun(evl->eval(*start_it));
        }
        else
        {
            t_fun(*start_it);
        }
        ++start_it;
    }

    if constexpr (eval)
    {
        return t_fun(evl->eval(*end_it));
    }
    else
    {
        return t_fun(*end_it);
    }
}

template<bool eval, typename Callable, typename StartType>
inline StartType reduce([[maybe_unused]] eval::Evaluator *evl,
                        const ALObjectPtr &t_obj,
                        Callable &&t_fun,
                        StartType t_start,
                        size_t t_offset = 0)
{
    auto &objects   = *t_obj;
    const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);

    auto start_it = std::next(std::begin(objects), hops);
    auto end_it   = std::end(objects);

    StartType val = [&]() {
        if constexpr (eval)
        {
            return t_fun(t_start, evl->eval(*start_it++));
        }
        else
        {
            return t_fun(t_start, *start_it++);
        }
    }();

    while (start_it != end_it)
    {

        if constexpr (eval)
        {
            val = t_fun(val, evl->eval(*start_it));
        }
        else
        {
            val = t_fun(val, *start_it);
        }

        start_it = std::next(start_it);
    }

    return val;
}

inline ALObjectPtr eval_transform(eval::Evaluator *evl, const ALObjectPtr &t_obj, size_t t_offset = 0)
{
    auto &objects   = *t_obj;
    const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);
    auto start_it   = std::next(std::begin(objects), hops);
    auto end_it     = std::end(objects);

    std::vector<ALObjectPtr> new_child;

    while (start_it != end_it)
    {
        new_child.push_back(evl->eval(*start_it));
        start_it = std::next(start_it);
    }

    return make_object(new_child);
}

template<typename T>
inline auto eval_check(eval::Evaluator *eval, const ALObjectPtr &t_obj, size_t t_index, T &&assertion)
{
    auto temp = eval->eval(t_obj->i(t_index));
    AL_CHECK(assertion(temp, t_index));
    return temp;
}

inline auto arg_eval(eval::Evaluator *eval, const ALObjectPtr &t_obj, size_t t_index)
{
    return (obj->prop_exists("--evaled--") ? obj->i(index) : eval_obj->eval(obj->i(index)));
}

}  // namespace alisp
