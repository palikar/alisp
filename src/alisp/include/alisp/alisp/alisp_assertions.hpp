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


namespace alisp
{


template<size_t N> inline void assert_min_size(ALObjectPtr obj)
{
    if (!min_list_elements(obj, N))
        throw argument_error("Invalid argument. Must be list with at least " + std::to_string(N) + " elements", obj);
}

template<size_t N> inline void assert_max_size(ALObjectPtr obj)
{
    if (!max_list_elements(obj, N))
        throw argument_error("Invalid argument. Must be list with maximum of " + std::to_string(N) + " elements", obj);
}

template<size_t N> inline void assert_size(ALObjectPtr obj)
{
    if (obj->length() != N)
        throw argument_error("Invalid argument. Must be list with  " + std::to_string(N) + " elements", obj);
}

inline void assert_numbers(ALObjectPtr obj)
{
    if (!are_objects_numbers(obj))
        throw argument_error("Invalid argument. The list must contain only numbers (real of int)", obj);
}

inline void assert_symbol(ALObjectPtr obj)
{
    if (!obj->is_sym()) throw argument_error("Invalid argument. Object must be symbol", obj);
}

inline void assert_string(ALObjectPtr obj)
{
    if (!obj->is_string()) throw argument_error("Invalid argument. Object must be string", obj);
}

inline void assert_list(ALObjectPtr obj)
{
    if (!obj->is_list() and obj != Qnil) throw argument_error("Invalid argument. Object must be list", obj);
}

inline void assert_number(ALObjectPtr obj)
{
    if (!obj->is_int() and !obj->is_real()) throw argument_error("Invalid argument. Object must be a number", obj);
}

inline void assert_int(ALObjectPtr obj)
{
    if (!obj->is_int()) throw argument_error("Invalid argument. Object must be an integer", obj);
}

inline void assert_char(ALObjectPtr obj)
{
    if (!obj->is_int() and !obj->check_char_flag())
        throw argument_error("Invalid argument. Object must be a char", obj);
}

inline void assert_function(ALObjectPtr obj)
{
    if (!obj->check_function_flag()) throw argument_error("Invalid argument. Object must be a function", obj);
}

inline void assert_non_const(ALObjectPtr obj)
{
    if (!obj->check_const_flag()) throw argument_error("A symbol is const.", obj);
}

inline void assert_file(ALObjectPtr obj)
{
    if (!files::files_registry.belong(object_to_resource(obj)))
        throw argument_error("The object does not point to a file", obj);
}

inline void assert_stream(ALObjectPtr obj)
{
    if (!al::streams_registry.belong(object_to_resource(obj)))
        throw argument_error("The object does not point to a stream", obj);
}

inline void assert_memory(ALObjectPtr obj)
{
    if (!memory::memory_registry.belong(object_to_resource(obj)))
        throw argument_error("The object does not point to a stream", obj);
}

inline void assert_byte(ALObjectPtr obj)
{
    if (!obj->is_int()) throw argument_error("Object cannot be intrepreted as byte.", obj);
    auto val = obj->to_int();
    if (!(0 <= val and val <= 255)) throw argument_error("Object cannot be intrepreted as byte.", obj);
}

inline void assert_byte_array(ALObjectPtr obj)
{
    if (!obj->is_list()) throw argument_error("Object cannot be intrepreted as byte-array.", obj);
    for (auto &el : *obj)
    {
        auto val = el->to_int();
        if (!(0 <= val and val <= 255)) throw argument_error("Object cannot be intrepreted as byte-array.", obj);
    }
}

}  // namespace alisp
