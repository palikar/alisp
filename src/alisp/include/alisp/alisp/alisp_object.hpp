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

#include <algorithm>
#include <string>
#include <sstream>
#include <variant>
#include <vector>
#include <iterator>
#include <memory>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"

#include "alisp/alisp/declarations/constants.hpp"
#include "alisp/alisp/declarations/language_constructs.hpp"

#include "alisp/utility.hpp"

namespace alisp
{

inline auto splice(const ALObjectPtr& t_obj,
                   std::vector<ALObject>::difference_type start_index,
                   std::vector<ALObject>::difference_type end_index = -1)
{

    const auto size     = static_cast<std::vector<ALObject>::difference_type>(std::size(*t_obj));
    const auto end_move = end_index == -1 ? size : end_index;

    auto begin_it = std::next(std::begin(*t_obj), start_index);
    auto end_it   = std::next(std::begin(*t_obj), end_move);

    if (begin_it > end_it)
    {
        return Qnil;
    }

    auto new_child = std::vector<ALObjectPtr>(begin_it, end_it);
    return make_object(new_child);
}

inline auto splice_temp(const ALObjectPtr& t_obj,
                        std::vector<ALObject>::difference_type start_index,
                        std::vector<ALObject>::difference_type end_index = -1)
{

    const auto size     = static_cast<std::vector<ALObject>::difference_type>(std::size(*t_obj));
    const auto end_move = end_index == -1 ? size : end_index;

    auto begin_it = std::next(std::begin(*t_obj), start_index);
    auto end_it   = std::next(std::begin(*t_obj), end_move);

    if (begin_it > end_it)
    {
        return Qnil;
    }

    return make_object(begin_it, end_it);
}

inline auto quote(const ALObjectPtr& t_obj)
{
    return make_object(Qquote, std::move(t_obj));
}


/*  _     _     _    */
/* | |   (_)___| |_  */
/* | |   | / __| __| */
/* | |___| \__ \ |_  */
/* |_____|_|___/\__| */
/*                        _                   _ _   _              */
/*  _ __ ___   __ _ _ __ (_)_ __  _   _  __ _| | |_(_) ___  _ __   */
/* | '_ ` _ \ / _` | '_ \| | '_ \| | | |/ _` | | __| |/ _ \| '_ \  */
/* | | | | | | (_| | | | | | |_) | |_| | (_| | | |_| | (_) | | | | */
/* |_| |_| |_|\__,_|_| |_|_| .__/ \__,_|\__,_|_|\__|_|\___/|_| |_| */
/*                         |_|  */


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

/*  ____              _         _   _ _      */
/* | __ )  ___   ___ | |  _   _| |_(_) |___  */
/* |  _ \ / _ \ / _ \| | | | | | __| | / __| */
/* | |_) | (_) | (_) | | | |_| | |_| | \__ \ */
/* |____/ \___/ \___/|_|  \__,_|\__|_|_|___/ */


inline bool is_falsy(const ALObjectPtr &obj)
{
    if (obj == Qnil) return true;

    if (obj->type() == ALObjectType::LIST) return obj->length() == 0;
    if (obj->type() == ALObjectType::STRING_VALUE) return obj->to_string().empty();
    if (obj->type() == ALObjectType::INT_VALUE) return obj->to_int() == 0;
    if (obj->type() == ALObjectType::REAL_VALUE) return obj->to_real() == 0.0;

    return false;
}

inline bool is_truthy(const ALObjectPtr &obj)
{
    return !is_falsy(obj);
}

inline bool are_objects_numbers(const ALObjectPtr &obj)
{
    if (!obj->is_list())
    {
        return obj->is_int() && obj->is_real();
    }
    for (auto child : *obj)
    {
        if (!(child->is_int() || child->is_real())) return false;
    }
    return true;
}

inline bool are_objects_int(const ALObjectPtr &obj)
{
    if (!obj->is_list())
    {
        return obj->is_int();
    }
    for (auto child : *obj)
    {
        if (!child->is_int()) return false;
    }
    return true;
}

inline bool are_objects_real(const ALObjectPtr &obj)
{
    if (!obj->is_list())
    {
        return obj->is_real();
    }
    for (auto child : *obj)
    {
        if (!child->is_real()) return false;
    }
    return true;
}

inline bool are_objects_string(const ALObjectPtr &obj)
{
    if (!obj->is_list())
    {
        return obj->is_string();
    }
    for (auto child : *obj)
    {
        if (!child->is_string()) return false;
    }
    return true;
}

inline bool min_list_elements(const ALObjectPtr &obj, size_t t_element_cnt)
{
    return obj->is_list() && std::size(*obj) >= t_element_cnt;
}

inline bool max_list_elements(const ALObjectPtr &obj, size_t t_element_cnt)
{
    return obj->is_list() && std::size(*obj) <= t_element_cnt;
}

inline bool psym(const ALObjectPtr &obj)
{
    return obj->is_sym();
}

inline bool pint(const ALObjectPtr &obj)
{
    return obj->is_int();
}

inline bool preal(const ALObjectPtr &obj)
{
    return obj->is_real();
}

inline bool plist(const ALObjectPtr &obj)
{
    return obj->is_list();
}

inline bool pstring(const ALObjectPtr &obj)
{
    return obj->is_string();
}

inline bool pfunction(const ALObjectPtr &obj)
{
    return obj->check_function_flag();
}

inline bool pprime(const ALObjectPtr &obj)
{
    return obj->get_prime() != nullptr;
}

inline bool contains(const ALObjectPtr &obj, const std::string &t_str)
{
    if (!plist(obj))
    {
        return false;
    }

    const auto pred = [&t_str](auto it) {
        if (!psym(it))
        {
            return false;
        }
        if (it->to_string().compare(t_str) == 0)
        {
            return true;
        }
        return false;
    };

    return std::any_of(std::begin(*obj), std::end(*obj), pred);
}

inline auto get_next(const ALObjectPtr &obj, const std::string &t_str) -> std::pair<ALObjectPtr, bool>
{
    if (!plist(obj))
    {
        return { nullptr, false };
    }

    const auto pred = [&t_str](auto it) {
        if (!psym(it))
        {
            return false;
        }
        if (it->to_string().compare(t_str) == 0)
        {
            return true;
        }
        return false;
    };

    auto prop = std::find_if(std::begin(*obj), std::end(*obj), pred);
    auto val  = std::next(prop);

    if (prop == std::end(*obj))
    {
        return { nullptr, false };
    }
    if (val == std::end(*obj))
    {
        return { nullptr, false };
    }

    return std::pair{ *val, true };
}


inline const auto AND_OBJ_FUN = [](bool t_acc, const ALObjectPtr &t_obj) { return t_acc and is_truthy(t_obj); };
inline const auto OR_OBJ_FUN  = [](bool t_acc, const ALObjectPtr &t_obj) { return t_acc or is_truthy(t_obj); };


// inline bool operator bool(ALObjectPtr t_obj) {
//     return is_truthy(t_obj);
// }

uint32_t object_to_resource(const ALObjectPtr &t_obj);

ALObjectPtr resource_to_object(uint32_t t_id);


/*  __  __       _   _             _   _ _      */
/* |  \/  | __ _| |_| |__    _   _| |_(_) |___  */
/* | |\/| |/ _` | __| '_ \  | | | | __| | / __| */
/* | |  | | (_| | |_| | | | | |_| | |_| | \__ \ */
/* |_|  |_|\__,_|\__|_| |_|  \__,_|\__|_|_|___/ */


inline const auto ADD_OBJ_FUN = [](int64_t t_acc, ALObjectPtr &t_obj) { return t_acc + t_obj->to_int(); };
inline const auto SUB_OBJ_FUN = [](int64_t t_acc, ALObjectPtr &t_obj) { return t_acc - t_obj->to_int(); };
inline const auto MUL_OBJ_FUN = [](int64_t t_acc, ALObjectPtr &t_obj) { return t_acc * t_obj->to_int(); };
inline const auto DIV_OBJ_FUN = [](int64_t t_acc, ALObjectPtr &t_obj) { return t_acc / t_obj->to_int(); };

inline const auto ADD_OBJ_FUN_D = [](double t_acc, ALObjectPtr &t_obj) { return t_acc + t_obj->to_real(); };
inline const auto SUB_OBJ_FUN_D = [](double t_acc, ALObjectPtr &t_obj) { return t_acc - t_obj->to_real(); };
inline const auto MUL_OBJ_FUN_D = [](double t_acc, ALObjectPtr &t_obj) { return t_acc * t_obj->to_real(); };
inline const auto DIV_OBJ_FUN_D = [](double t_acc, ALObjectPtr &t_obj) { return t_acc / t_obj->to_real(); };


inline const auto SHIFT_LEFT = [](ALObjectPtr &t_lhs, ALObjectPtr &t_rhs) {
    return t_lhs->to_int() << t_rhs->to_int();
};
inline const auto SHIFT_RIGHT = [](ALObjectPtr &t_lhs, ALObjectPtr &t_rhs) {
    return t_lhs->to_int() >> t_rhs->to_int();
};
inline const auto BIT_OR  = [](ALObjectPtr &t_lhs, ALObjectPtr &t_rhs) { return t_lhs->to_int() | t_rhs->to_int(); };
inline const auto BIT_AND = [](ALObjectPtr &t_lhs, ALObjectPtr &t_rhs) { return t_lhs->to_int() & t_rhs->to_int(); };
inline const auto BIT_XOR = [](ALObjectPtr &t_lhs, ALObjectPtr &t_rhs) { return t_lhs->to_int() ^ t_rhs->to_int(); };
inline const auto BIT_INV = [](ALObjectPtr &t_obj) { return ~t_obj->to_int(); };


/*  _____                  _ _ _          */
/* | ____|__ _ _   _  __ _| (_) |_ _   _  */
/* |  _| / _` | | | |/ _` | | | __| | | | */
/* | |__| (_| | |_| | (_| | | | |_| |_| | */
/* |_____\__, |\__,_|\__,_|_|_|\__|\__, | */
/*          |_|                    |___/  */

inline bool equal(const ALObjectPtr &t_lhs, const ALObjectPtr &t_rhs);


inline bool list_equal(const ALObjectPtr &t_lhs, const ALObjectPtr &t_rhs)
{

    auto &children_1 = *t_lhs;
    auto &children_2 = *t_rhs;

    if (std::size(children_1) != std::size(children_2))
    {
        return false;
    }

    size_t index = 0;
    for (index = 0; index < std::size(children_1); ++index)
    {
        if (!equal(children_1[index], children_2[index])) return false;
    }

    return true;
}


inline bool real_equal(const ALObjectPtr &t_lhs, const ALObjectPtr &t_rhs)
{
    return t_lhs->to_real() == t_rhs->to_real();
}


inline bool equal(const ALObjectPtr &t_lhs, const ALObjectPtr &t_rhs)
{

    if (t_lhs->type() != t_rhs->type())
    {
        return false;
    }

    return make_visit(
      t_lhs,
      type(ALObjectType::SYMBOL) or type(ALObjectType::STRING_VALUE) >>=
      [t_rhs](ALObjectPtr t_obj) { return t_obj->to_string().compare(t_rhs->to_string()) == 0; },
      type(ALObjectType::INT_VALUE) >>= [t_rhs](ALObjectPtr t_obj) { return t_obj->to_int() == t_rhs->to_int(); },
      type(ALObjectType::REAL_VALUE) >>= [t_rhs](ALObjectPtr t_obj) { return real_equal(t_obj, t_rhs); },
      type(ALObjectType::LIST) >>= [t_rhs](ALObjectPtr t_obj) { return list_equal(t_obj, t_rhs); },
      any_pattern() >>= [](ALObjectPtr) { return false; });
}

inline bool eq(const ALObjectPtr &t_lhs, const ALObjectPtr &t_rhs)
{
    if (t_lhs->type() != t_rhs->type())
    {
        return false;
    }

    return make_visit(
      t_lhs,
      type(ALObjectType::INT_VALUE) >>= [t_rhs](ALObjectPtr t_obj) { return t_obj->to_int() == t_rhs->to_int(); },
      type(ALObjectType::REAL_VALUE) >>= [t_rhs](ALObjectPtr t_obj) { return real_equal(t_obj, t_rhs); },
      type(ALObjectType::STRING_VALUE) or type(ALObjectType::SYMBOL) or type(ALObjectType::LIST) >>=
      [t_rhs](ALObjectPtr t_obj) { return t_rhs == t_obj; });
}


struct NameValidator
{

    template<typename T> static bool is_reserved_word(const T &t_s) noexcept
    {
        switch (hash::hash(t_s))
        {
            case hash::hash("print"):
            case hash::hash("println"):

            case hash::hash("import"):
            case hash::hash("modref"):
            case hash::hash("defun"):
            case hash::hash("defvar"):
            case hash::hash("defconst"):
            case hash::hash("defmacro"):
            case hash::hash("set"):
            case hash::hash("setq"):
            case hash::hash("lambda"):
            case hash::hash("function"):
            case hash::hash("funcall"):

            case hash::hash("quote"):
            case hash::hash("if"):
            case hash::hash("while"):
            case hash::hash("dolist"):
            case hash::hash("cond"):
            case hash::hash("unless"):
            case hash::hash("when"):
            case hash::hash("progn"):
            case hash::hash("let*"):
            case hash::hash("let"):
            case hash::hash("backqoute"):

            case hash::hash("signal"):
            case hash::hash("return"):

            case hash::hash("and"):
            case hash::hash("or"):
            case hash::hash("not"): return true;
            default: return false;
        }
    }

    template<typename T> static bool is_special_symbol(const T &t_s) noexcept
    {

        switch (hash::hash(t_s))
        {
            case hash::hash("--doc--"):
            case hash::hash("--all--"): return true;
            default: return false;
        }
    }

    template<typename T> static bool valid_object_name(const T &t_name) noexcept
    {
        return utility::starts_with(t_name, "--");
    }

    template<typename T> static void validate_object_name(const T &t_name)
    {
        if (is_special_symbol(t_name))
        {
            return;
        }

        if (is_reserved_word(t_name))
        {
            throw illegal_name_error(t_name, "This is a reserved keyword.");
        }

        if (valid_object_name(t_name))
        {
            throw illegal_name_error(t_name, "Symbol names should not start with \"--\"");
        }
    }
};

inline auto to_string(const ALObjectPtr &t_obj)
{

    return make_visit(
      t_obj,
      is_function() >>= [](ALObjectPtr obj) { return (obj->get_prop("--name--")->to_string()); },
      is_char() >>= [](ALObjectPtr obj) { return (std::string(1, char(obj->to_int()))); },
      type(ALObjectType::INT_VALUE) >>= [](ALObjectPtr obj) { return (std::to_string(obj->to_int())); },
      type(ALObjectType::REAL_VALUE) >>=
      [](ALObjectPtr obj) {
          std::stringstream ss;
          ss << obj->to_real();
          return (ss.str());
      },
      type(ALObjectType::STRING_VALUE) >>= [](ALObjectPtr obj) { return (obj->to_string()); },
      type(ALObjectType::SYMBOL) >>= [](ALObjectPtr obj) { return (obj->to_string()); },
      any_pattern() >>= [](ALObjectPtr) { return ""; }

    );
}

}  // namespace alisp
