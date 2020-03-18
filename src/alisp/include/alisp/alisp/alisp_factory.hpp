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
#include "alisp/utility.hpp"


namespace alisp
{

/*   ____                _   _               _          _                 */
/*  / ___|_ __ ___  __ _| |_(_) ___  _ __   | |__   ___| |_ __  _ __ ___  */
/* | |   | '__/ _ \/ _` | __| |/ _ \| '_ \  | '_ \ / _ \ | '_ \| '__/ __| */
/* | |___| | |  __/ (_| | |_| | (_) | | | | | | | |  __/ | |_) | |  \__ \ */
/*  \____|_|  \___|\__,_|\__|_|\___/|_| |_| |_| |_|\___|_| .__/|_|  |___/ */
/*                                                       |_|   */

namespace detail
{

struct ALObjectHelper
{

  public:
    template<typename T> static auto init_ptr(T &&val)
    {
        if constexpr (USING_SHARED) { return std::shared_ptr<ALObject>(val); }
        else
        {
            return val;
        }
    }


    template<typename T> static auto init_ptr_temp(T &&val)
    {
        if constexpr (USING_SHARED)
        {
            return std::shared_ptr<ALObject>(val, [](ALObject *) {});
        }
        else
        {
            return val;
        }
    }


    template<typename T> static auto allocate_ptr(T &&) { return nullptr; }

  public:
    template<typename T> static auto get(T a) -> typename std::enable_if_t<std::is_integral_v<T>, ALObjectPtr>
    {
        const auto val = static_cast<ALObject::int_type>(a);
        auto obj       = new ALObject(val);

        return init_ptr(obj);
    }

    template<typename T> static auto get(T a) -> typename std::enable_if_t<std::is_floating_point_v<T>, ALObjectPtr>
    {
        return init_ptr(new ALObject(static_cast<ALObject::real_type>(a)));
    }

    template<typename T>
    static auto get(T a) -> typename std::enable_if_t<std::is_constructible_v<std::string, T>, ALObjectPtr>
    {
        return init_ptr(new ALObject(std::string(a)));
    }

    template<typename T>
    static auto get(T a, bool) -> typename std::enable_if_t<std::is_constructible_v<std::string, T>, ALObjectPtr>
    {
        return init_ptr(new ALObject(std::string(a), true));
    }

    static auto get(std::vector<ALObjectPtr> vec_objs) { return init_ptr(new ALObject(vec_objs)); }

    static auto get(ALObjectPtr obj) -> ALObjectPtr { return obj; }

    template<typename... T> static auto get(T... objs) -> ALObjectPtr
    {
        std::vector<ALObjectPtr> vec_objs;
        vec_objs.reserve(sizeof...(objs));

        (vec_objs.push_back(ALObjectHelper::get(objs)), ...);

        return init_ptr(new ALObject(vec_objs));
    }
};

}  // namespace detail

template<typename... T> inline auto make_object(T &&... args)
{
    return detail::ALObjectHelper::get(std::forward<T>(args)...);
}

template<typename T> inline auto make_symbol(T name, [[maybe_unused]] const std::string &t_doc = {})
{
    static_assert(std::is_constructible_v<std::string, T>, "Name must be string like type.");

    auto new_obj = detail::ALObjectHelper::get(std::string(name), true);
#ifdef ENABLE_OBJECT_DOC
    new_obj->set_prop("--doc--", detail::ALObjectHelper::get(t_doc));
#endif
    return new_obj;
}

template<typename T> inline auto make_int(T value)
{
    static_assert(std::is_integral_v<T>, "Value must be of integer type");
    return make_object(static_cast<ALObject::int_type>(value));
}

template<typename T> inline auto make_char(T value)
{
    static_assert(std::is_integral_v<T>, "Value must be of integer type");

    auto obj = make_object(static_cast<ALObject::int_type>(value));

    if (0 <= value && value <= 127) { obj->set_char_flag(); }

    return obj;
}

template<typename T> inline auto make_double(T value)
{
    static_assert(std::is_arithmetic_v<T>, "Value must be of real type");
    return make_object(static_cast<double>(value));
}

template<typename T> inline auto make_real(T value)
{
    static_assert(std::is_arithmetic_v<T>, "Value must be of real type");
    return make_object(static_cast<double>(value));
}

inline auto make_string(std::string value)
{
    return make_object(value);
}

inline auto make_list(ALObjectPtr obj)
{
    return detail::ALObjectHelper::get(std::vector{ obj });
}

inline auto make_list()
{
    return detail::ALObjectHelper::get(ALObject::list_type{});
}

inline auto make_list(std::vector<std::string> strs)
{
    auto new_obj = detail::ALObjectHelper::get(ALObject::list_type{});
    for (auto &str : strs) { new_obj->children().push_back(make_object(str)); }
    return new_obj;
}

inline auto make_list(ALObject::list_type elements)
{
    return make_object(elements);
}

template<typename ... I>
inline auto make_list(I ... objs)
{
    auto new_obj = detail::ALObjectHelper::get(ALObject::list_type{});
    ( new_obj->children().push_back(objs), ...  );
    return new_obj;
}

inline auto make_prime(Prim::func_type t_function, std::string t_name, [[maybe_unused]] const std::string &t_doc = {})
{
    auto sym = make_object(ALObject::list_type{});
    sym->make_prime(t_function);
    sym->set_prop("--name--", make_string(std::move(t_name)));

#ifdef ENABLE_OBJECT_DOC
    sym->set_prop("--doc--", make_string(t_doc));
#endif

    return sym;
}


inline auto operator"" _al(unsigned long long t_val)
{
    return make_int(t_val);
}

inline auto operator"" _al(long double t_val)
{
    return make_real(t_val);
}

inline auto operator"" _al(const char *t_name)
{
    return make_string(t_name);
}

inline auto operator"" _sym(const char *t_name)
{
    return make_symbol(t_name);
}


inline auto getraw(ALObjectPtr &t_obj)
{
    if constexpr (USING_SHARED) { return t_obj.get(); }
    else
    {
        return t_obj;
    }
}


}  // namespace alisp
