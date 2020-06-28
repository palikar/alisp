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

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_files.hpp"
#include "alisp/alisp/alisp_streams.hpp"
#include "alisp/alisp/alisp_memory.hpp"

#include <fmt/format.h>

namespace alisp
{

template<size_t N> inline void assert_min_size(const ALObjectPtr &obj)
{
    if (!min_list_elements(obj, N))
        throw argument_error(fmt::format("The object must be a list with at least {} elements", std::to_string(N)),
                             obj);
}

template<size_t N> inline void assert_max_size(const ALObjectPtr &obj)
{
    if (!max_list_elements(obj, N))
        throw argument_error(fmt::format("The object must be a list with maximum of {} elements", std::to_string(N)),
                             obj);
}

template<size_t N> inline void assert_size(const ALObjectPtr &obj)
{
    if (obj->length() != N)
        throw argument_error(fmt::format("The object must be a list with {} elements", std::to_string(N)), obj);
}

template<typename... A> inline void assert_numbers(const ALObjectPtr &obj, A... args)
{
    if (!are_objects_numbers(obj))
        throw argument_error("The list must contain only numbers (real or int)", obj, args...);
}

template<typename... A> inline void assert_symbol(const ALObjectPtr &obj, A... args)
{
    if (!obj->is_sym()) throw argument_error("Object must be a symbol", obj, args...);
}

template<typename... A> inline void assert_string(const ALObjectPtr &obj, A... args)
{
    if (!obj->is_string()) throw argument_error("Object must be a string", obj, args...);
}

template<typename... A> inline void assert_list(const ALObjectPtr &obj, A... args)
{
    if (!obj->is_list() and obj != Qnil) throw argument_error("Object must be a list", obj, args...);
}

template<typename... A> inline void assert_number(const ALObjectPtr &obj, A... args)
{
    if (!obj->is_int() and !obj->is_real()) throw argument_error("Object must be a number", obj, args...);
}

template<typename... A> inline void assert_int(const ALObjectPtr &obj, A... args)
{
    if (!obj->is_int()) throw argument_error("Object must be an integer", obj, args...);
}

template<typename... A> inline void assert_char(const ALObjectPtr &obj, A... args)
{
    if (!obj->is_int() and !obj->check_char_flag()) throw argument_error("Object must be a char", obj, args...);
}

template<typename... A> inline void assert_function(const ALObjectPtr &obj, A... args)
{
    if (!obj->check_function_flag()) throw argument_error("Object must be a function", obj, args...);
}

template<typename... A> inline void assert_non_const(const ALObjectPtr &obj, A... args)
{
    if (!obj->check_const_flag()) throw argument_error("The object must not be const.", obj, args...);
}

template<typename... A> inline void assert_file(const ALObjectPtr &obj, A... args)
{
    if (!files::files_registry.belong(object_to_resource(obj)))
        throw argument_error("The object must point to a file", obj, args...);
}

template<typename... A> inline void assert_stream(const ALObjectPtr &obj, A... args)
{
    if (!al::streams_registry.belong(object_to_resource(obj)))
        throw argument_error("The object must point to a stream", obj, args...);
}

template<typename... A> inline void assert_memory(const ALObjectPtr &obj, A... args)
{
    if (!memory::memory_registry.belong(object_to_resource(obj)))
        throw argument_error("The object must point to a stream", obj, args...);
}

template<typename... A> inline void assert_byte(const ALObjectPtr &obj, A... args)
{
    if (!obj->is_int()) throw argument_error("The object must be intrepretable as byte.", obj, args...);
    auto val = obj->to_int();
    if (!(0 <= val and val <= 255)) throw argument_error("The object must be intrepretable as byte.", obj, args...);
}

template<typename... A> inline void assert_byte_array(const ALObjectPtr &obj, A &&... args)
{
    if (!obj->is_list()) throw argument_error("The object must be intrepretable as byte array.", obj, args...);
    for (auto &el : *obj)
    {
        auto val = el->to_int();
        if (!(0 <= val and val <= 255))
            throw argument_error("The object must be intrepretable as byte array.", obj, args...);
    }
}


}  // namespace alisp
